#lang racket
(require xml)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTML file generation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-dirs path-parts)
  (let loop ((base 'same) (parts path-parts))
    (unless (empty? parts)
      (let ((next (build-path base (car parts))))
        (unless (directory-exists? next)
          (make-directory next))
        (loop next (cdr parts))))))
(define (xexpr->pretty-string xexpr)
  (call-with-output-string
    (curry display-xml/content (xexpr->xml xexpr))))
(define (xexpr->html-string xexpr)
  (string-append "<!DOCTYPE html>" (xexpr->string xexpr)))
(define (write-html-file path xexpr)
  (define-values (directory filename is-root) (split-path path))
  (when (path? directory) (make-dirs (explode-path directory)))
  (call-with-output-file path #:exists 'replace
    (curry display (xexpr->html-string xexpr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; site description
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (reachable graph roots)
  (let loop ((reached (set))
             (roots (list->set (sequence->list roots))))
    (if (set-empty? roots) reached
      (let* ((reached (set-union reached roots))
             (nexts (map (curry dict-ref graph) (set->list roots)))
             (next (apply set-union nexts))
             (next (set-subtract next reached)))
        (loop reached next)))))

(struct node-ref (name data) #:transparent)
(define (tree-lift-refs tree)
  (match tree
    ((node-ref name data) (list (set name) data))
    ((? list?)
     (match-let (((list names trees)
                  (apply (curry map list) (map tree-lift-refs tree))))
       (list (apply set-union names) trees)))
    (_ (list (set) tree))))

(define (path-tree->name=>path+node-ref descs ptree)
  (define (attr-add attrs name base filename)
    (when (dict-has-key? attrs name)
      (error (format "path tree contains duplicate entries for: ~a" name)))
    (let* ((path (apply build-path (reverse (cons filename base))))
           (href (string-append "/" (path->string path)))
           (desc (dict-ref descs name))
           (link `(a ((href ,href)) ,desc)))
      (dict-set attrs name (list path (node-ref name link)))))
  (define (synthesize-fold attrs base children)
    (for/fold ((attrs attrs))
              ((child children))
      (synthesize attrs base child)))
  (define (synthesize attrs base child)
    (match child
      ((cons subdir children)
        (let* ((base (cons (symbol->string subdir) base))
               (attrs (attr-add attrs subdir base "index.html")))
          (synthesize-fold attrs base children)))
      (_
        (attr-add attrs child base
          (string-append (symbol->string child) ".html")))))
  (let* ((attrs (synthesize-fold (hash) '() (cdr ptree)))
         (attrs (attr-add attrs (car ptree) '() "index.html")))
    attrs))

(require (for-syntax racket/dict racket/function))
(define-for-syntax (build-site stx path-tree exprs pages)
  (let* ((pages (map syntax->list (syntax->list pages)))
         (page-idents (map car pages))
         (page-names (map syntax->datum page-idents))
         (page-descs (map syntax->datum (map cadr pages)))
         (page-bodies (map caddr pages))
         (descs (make-immutable-hash (map cons page-names page-descs)))
         (ptree (syntax->datum path-tree))
         (page-root (car ptree)))
    (let* ((names (dict-keys descs))
          (idents (map (curry datum->syntax stx) names)))
      (datum->syntax stx
        (append
          (list #'begin)
          (syntax->list #`(
            (define n=>p+nr (path-tree->name=>path+node-ref
                              (make-immutable-hash '#,(dict->list descs))
                              '#,ptree))
            (define n++p+nr (dict->list n=>p+nr))
            (define name=>path
              (make-immutable-hash
                (map (match-lambda ((cons name (list path nr)) (cons name path)))
                    n++p+nr)))
            (match-define
              (list #,@idents)
              (map (compose1 cadr (curry dict-ref n=>p+nr)) '#,names))))
          (syntax->list exprs)
          (list
            #`(let* ((page-reachables+xexpr
                       (map tree-lift-refs (list #,@page-bodies)))
                     (page-reachables (map car page-reachables+xexpr))
                     (page-graph (make-immutable-hash
                                   (map cons '#,page-names page-reachables)))
                     (pages-reached (reachable page-graph (list '#,page-root)))
                     (pages-not-reached (set-subtract (list->set '#,page-names)
                                                      pages-reached))
                     (page-xexprs (map cadr page-reachables+xexpr))
                     (page-paths (map (curry dict-ref name=>path)
                                      '#,page-names)))
                (unless (set-empty? pages-not-reached)
                  (error (format "unreachable pages: ~a"
                                 (set->list pages-not-reached))))
                (for ((path page-paths) (xexpr page-xexprs))
                     (write-html-file path xexpr)))))))))

(define-syntax (define-site stx)
  (syntax-case stx ()
    ((_ path-tree exprs page ...)
     (build-site stx #'path-tree #'exprs #'(page ...)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; an actual site
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-site
  ; path structure
  (home about)

  ; utilities
  (
   (define (anchor href description)
     `(a ((href ,href)) ,description))
   (define head
     `(head
        (title "Greg's Metareflection")
        (meta ((charset "utf-8")))
        (meta ((name "author") (content "Greg Rosenblatt")))
        (meta ((name "description") (content "Personal page of Greg Rosenblatt")))))
   (define nav
     `(nav
        (ul
          (li ,home)
          (li ,about))))
   (define (content body)
     `(html
        ,head
        (body ,body
        ,nav)))
   )

  ; page definitions
  (home "Home"
    (content
      `(article
         (p "hi")
         (p ,(anchor "https://github.com/gregr" "I'm on github.")))))

  (about "About"
    (content
      `(article
         (h1 "A little bit about me")
         (section
           (h2 "Summary")
           (p "I currently live and work in Toronto, Ontario."))
         (section
           (h2 "Notable personal projects")
           (p "The following projects were the product of significant effort made as I grew up as a programmer.")
           (p "Being possibly overcritical about my own past work, I'm not particularly proud of any of these.  But even with such reservations, I feel it's still important to share.")
           (ul
             (li
               (h3 ,(anchor "https://github.com/gregr/chive" "The Chive Programming Language"))
               (h4 "2009 - 2010")
               (p "This was my first serious attempt at a full programming language implementation.  It features a scheme-like hygienic macro system based on syntactic closures.")
               (p "It was put aside when I started working at Facebook.")
               )
             (li
               (h3 ,(anchor "https://github.com/gregr/uriel" "Uriel"))
               (h4 "2005 - 2006")
               (p "a tile-based multiplayer game framework")
               (p "Several amusing games were made with this.  They are lost."))
             (li
               (h3 ,(anchor "https://github.com/gregr/starscape" "Starscape"))
               (h4 "1999 - 2005")
               (p "a 3D game programming system, including a GUI library"))
             (li
               (h3 "Creating a Scripting System in C++")
               (h4 "2002 - 2003")
               (p "This is an article series I wrote for " ,(anchor "http://www.gamedev.net/" "gamedev.net") " in five parts.")
               (ul
                 (li ,(anchor "http://www.gamedev.net/page/resources/_/technical/game-programming/creating-a-scripting-system-in-c-part-i-an-i-r1633" "I"))
                 (li ,(anchor "http://www.gamedev.net/page/resources/_/technical/game-programming/creating-a-scripting-system-in-c-part-ii-dat-r1686" "II"))
                 (li ,(anchor "http://www.gamedev.net/page/resources/_/technical/game-programming/creating-a-scripting-system-in-c-part-iii-dy-r1788" "III"))
                 (li ,(anchor "http://www.gamedev.net/page/resources/_/technical/game-programming/creating-a-scripting-system-in-c-part-iv-the-r1803" "IV"))
                 (li ,(anchor "http://www.gamedev.net/page/resources/_/technical/game-programming/creating-a-scripting-system-in-c-part-v-func-r1877" "V"))))))
         (section
           (h2 "Professional experience")
           (p "I'll fill this in later."))
         (section
           (h2 "Education")
           (ul
             (li
               (h3 "Rochester Institute of Technology")
               (h4 "1999 - 2004")
               (p "Bachelor of Science in Mechanical Engineering")
               (p "Concentration in Aerospace"))
             (li
               (h3 "Stanford Online Courses for AI and Machine Learning")
               (h4 "Fall 2011")
               (h5 "Statement of Accomplishment")
               (p "This was the original online offering of these two classes from which "
                  ,(anchor "https://www.coursera.org/" "coursera")
                  " and "
                  ,(anchor "https://www.udacity.com/" "udacity")
                  " were spun off."))))
         )))
  )
