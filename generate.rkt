#lang racket
(require xml)

(define-syntax (for/foldm stx)
  (syntax-case stx ()
    ((_ accs ((pattern seq) ...) body)
     (with-syntax (((element ...)
                    (generate-temporaries #'((pattern seq) ...))))
      #'(for/fold accs
                ((element seq) ...)
          (match* (element ...)
            ((pattern ...) body)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; file generation
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

(define (write-file path content)
  (define-values (directory filename is-root) (split-path path))
  (when (path? directory) (make-dirs (explode-path directory)))
  (call-with-output-file path #:exists 'replace
    (curry display content)))

(define (write-html-file path xexpr)
  (write-file path (xexpr->html-string xexpr)))

(define (css-style->string style)
  (let ((parts
          (for/foldm ((parts '()))
                     (((cons field value) (dict->list style)))
            (cons (format "  ~a: ~a;\n" field value) parts))))
    (string-append "{\n" (apply string-append (reverse parts)) "}\n")))

(define (css->string css)
  (let ((parts
          (for/foldm ((parts '()))
                     (((cons selector style) css))
            (cons (string-append selector " " (css-style->string style)) parts))))
    (apply string-append (reverse parts))))

(define (write-css-file path css)
  (write-file path (css->string css)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CSS description
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define-css stx)
  (syntax-case stx ()
    ((_ path (selector style) ...)
     #`(write-css-file path
         (list
           (cons selector style) ...)))))

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

(define nav-width "100px")

(define-css "main.css"
  ("body"
   (hash
     "font-family" "'Lucida Grande', Verdana, Helvetica, sans-serif"
     "font-size" "80%"
     "line-height" "120%"
     ))

  ("#nav-main"
   (hash
     "position" "fixed"
     "top" "0"
     "left" "0"
     "width" nav-width))
  ("#content-main"
   (hash
     "margin-left" nav-width))

  (".date"
   (hash
     "font-style" "italic"
     "font-weight" "bold"))
  (".date-end:before"
   (hash
     "content" "\" - \""))

  (".multipart-series li"
   (hash
     "display" "inline"
     "padding" "10px"))

  (".employment-list>li"
   (hash
     "margin-bottom" "1em"
     ))
  (".employment-employer"
   (hash
     "display" "inline"
     "font-weight" "bold"
     ))
  (".employment-location"
   (hash
     "font-weight" "normal"
     "display" "inline"
     ))
  (".employment .date-range"
   (hash
     "margin-bottom" "1em"
     ))
  (".employment-location:before"
   (hash
     "content" "\" - \""))
  )

(define-site
  ; path structure
  (home about)

  ; utilities
  (
   (define head
     `(head
        (title "Greg's Metareflection")
        (meta ((charset "utf-8")))
        (meta ((name "author") (content "Greg Rosenblatt")))
        (meta ((name "description") (content "Personal page of Greg Rosenblatt")))
        (link ((rel "stylesheet") (href "main.css")))))
   (define nav
     `(nav ((id "nav-main"))
        (ul
          (li ,home)
          (li ,about))))
   (define (content body)
     `(html
        ,head
        (body
          (div ((id "content-main")) ,body)
          ,nav)))
   (define (anchor href description)
     `(a ((href ,href)) ,description))
   (define (date-single d)
     `(div ((class "date")) ,d))
   (define (date-range start end)
     `(div ((class "date-range"))
           (span ((class "date")) ,start)
           (span ((class "date date-end")) ,end)))
   (define (employment employer location title start end . details)
     `(div ((class "employment"))
        (div
          (h3 ((class "employment-employer")) ,employer)
          (div ((class "employment-location")) ,location))
        (h4 ((class "employment-title")) ,title)
        ,(date-range start end)
        (div ((class "employment-details")) ,@details)))

   (define (anchor-beat-cap desc)
     (anchor "http://nathanmarz.com/blog/how-to-beat-the-cap-theorem.html" desc))
   (define (anchor-consistent-hash desc)
     (anchor "http://www.tomkleinpeter.com/2008/03/17/programmers-toolbox-part-3-consistent-hashing/" desc))
   (define (anchor-raft-paxos desc)
     (anchor "http://www.reddit.com/comments/1c1rjx" desc))
   (define (anchor-props-as-types desc)
     (anchor "http://homepages.inf.ed.ac.uk/wadler/papers/propositions-as-types/propositions-as-types.pdf" desc))
   (define (anchor-relational-prog desc)
     (anchor "https://www.youtube.com/watch?v=5Q9x16uIsKA" desc))

   (define (anchor-100-percent-solutions desc)
     (anchor "http://www.ccs.neu.edu/home/shivers/papers/sre.txt" desc))
   (define (anchor-postgres desc)
     (anchor "https://en.wikipedia.org/wiki/PostgreSQL" desc))
   (define (anchor-redis desc)
     (anchor "https://en.wikipedia.org/wiki/Redis" desc))
   (define (anchor-haskell desc)
     (anchor "https://en.wikipedia.org/wiki/Haskell_(programming_language)" desc))
   (define (anchor-racket desc)
     (anchor "https://en.wikipedia.org/wiki/Racket_(programming_language)" desc))
   (define (anchor-python desc)
     (anchor "https://en.wikipedia.org/wiki/Python_(programming_language)" desc))
   (define (anchor-git desc)
     (anchor "https://en.wikipedia.org/wiki/Git_(software)" desc))

   (define (anchor-c++-fqa desc)
     (anchor "http://yosefk.com/c++fqa/" desc))
   (define (anchor-wat-talk desc)
     (anchor "https://www.destroyallsoftware.com/talks/wat" desc))
   (define (anchor-php-fractally-bad desc)
     (anchor "http://me.veekun.com/blog/2012/04/09/php-a-fractal-of-bad-design/" desc))
   (define (anchor-mysql-choose-something-else desc)
     (anchor "http://grimoire.ca/mysql/choose-something-else" desc))

   (define (anchor-SYJMrF desc)
     (anchor "https://en.wikipedia.org/wiki/Surely_You're_Joking,_Mr._Feynman!" desc))
   (define (anchor-GEB desc)
     (anchor "https://en.wikipedia.org/wiki/G%C3%B6del,_Escher,_Bach" desc))
   (define (anchor-logicomix desc)
     (anchor "https://en.wikipedia.org/wiki/Logicomix" desc))
   (define (anchor-prag-prog desc)
     (anchor "https://en.wikipedia.org/wiki/The_Pragmatic_Programmer" desc))
   (define (anchor-peopleware desc)
     (anchor "https://en.wikipedia.org/wiki/Peopleware:_Productive_Projects_and_Teams" desc))
   (define (anchor-sicp desc)
     (anchor "https://mitpress.mit.edu/sicp/" desc))
   (define (anchor-algo-design-manual desc)
     (anchor "http://www.algorist.com/" desc))
   (define (anchor-okasaki desc)
     (anchor "http://www.amazon.ca/Purely-Functional-Structures-Chris-Okasaki/dp/0521663504" desc))
   (define (anchor-prog-pearls desc)
     (anchor "http://www.cs.bell-labs.com/cm/cs/pearls/" desc))
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
           (p "I currently live and work in Toronto, Ontario.  I am a programmer.")
           (p
             "I am "
             ,(anchor-beat-cap "much")
             " "
             ,(anchor-consistent-hash "more")
             " "
             ,(anchor-raft-paxos "interested")
             " "
             ,(anchor-relational-prog "in")
             " "
             ,(anchor-props-as-types "knowledge")
             " than in specific technologies.  Though, I do like to choose "
             ,(anchor-postgres "which")
             " "
             ,(anchor-redis "technologies")
             " I work with, "
             ,(anchor-racket "preferring")
             " "
             ,(anchor-haskell "well-designed")
             " "
             ,(anchor-python "languages")
             ", "
             ,(anchor-git "tools")
             " and "
             ,(anchor-100-percent-solutions "100% solutions")
             ".  Unfortunately I have "
             ,(anchor-c++-fqa "plenty")
             " of "
             ,(anchor-wat-talk "experience")
             " with "
             ,(anchor-php-fractally-bad "poorly-designed")
             " "
             ,(anchor-mysql-choose-something-else "technology")
             ".")
           (p
             "Proper "
             ,(anchor-peopleware "management")
             " trumps technology decisions."
             ))
         (section
           (h2 "Notable personal projects")
           (p "The following projects were the product of significant effort made as I grew up as a programmer.")
           (p "Being possibly overcritical about my own past work, I'm not particularly proud of any of these.  But even with such reservations, I feel it's still important to share.")
           (ul
             (li
               (h3 ,(anchor "https://github.com/gregr/chive" "The Chive Programming Language"))
               ,(date-range "2009" "2010")
               (p "This was my first serious attempt at a full programming language implementation.  It features a scheme-like hygienic macro system based on syntactic closures.")
               (p "It was put aside when I started working at Facebook.")
               )
             (li
               (h3 ,(anchor "https://github.com/gregr/uriel" "Uriel"))
               ,(date-range "2005" "2006")
               (p "a tile-based multiplayer game framework")
               (p "Several amusing games were made with this.  They are lost."))
             (li
               (h3 ,(anchor "https://github.com/gregr/starscape" "Starscape"))
               ,(date-range "1999" "2005")
               (p "a 3D game programming system, including a GUI library"))
             (li
               (h3 "Creating a Scripting System in C++")
               ,(date-range "2002" "2003")
               (p "This is an article series I wrote for " ,(anchor "http://www.gamedev.net/" "gamedev.net") " in five parts.")
               (ul ((class "multipart-series"))
                 (li ,(anchor "http://www.gamedev.net/page/resources/_/technical/game-programming/creating-a-scripting-system-in-c-part-i-an-i-r1633" "I"))
                 (li ,(anchor "http://www.gamedev.net/page/resources/_/technical/game-programming/creating-a-scripting-system-in-c-part-ii-dat-r1686" "II"))
                 (li ,(anchor "http://www.gamedev.net/page/resources/_/technical/game-programming/creating-a-scripting-system-in-c-part-iii-dy-r1788" "III"))
                 (li ,(anchor "http://www.gamedev.net/page/resources/_/technical/game-programming/creating-a-scripting-system-in-c-part-iv-the-r1803" "IV"))
                 (li ,(anchor "http://www.gamedev.net/page/resources/_/technical/game-programming/creating-a-scripting-system-in-c-part-v-func-r1877" "V"))))))
         (section
           (h2 "Professional experience")
           (ul ((class "employment-list"))
             (li
               ,(employment "Tulip Retail" "Toronto, ON"
                            "Software Engineer"
                            "October 2013" "Present"
                  `(p "Part of the founding team originally from Well.ca.")
                  `(p "Worked on both retail platform and individual client projects.")
                  `(ul
                     (li "Re-design of data model and platform in terms of a service-oriented architecture")
                     (li "Framework for building hypermedia APIs")
                     (li "Data ingestion and processing library")
                     (li "Continuous image import system")
                     )))
             (li
               ,(employment "Well.ca" "Kitchener, ON"
                            "Software Engineer"
                            "August 2012" "October 2013"
                  `(p "Refactored and redesigned the warehouse management system")
                  `(ul
                     (li "Separation of WMS into an independent service providing a web API")
                     (li "Design and performance analysis of alternative communication protocols")
                     (li "Internal tools and automation for development and deployment")
                     )))
             (li
               ,(employment "Facebook" "Palo Alto, CA"
                            "Software Engineer"
                            "January 2011" "June 2012"
                  `(p "Developed and maintained internal development tools and infrastructure")
                  `(ul
                     (li "Distributed continuous test run infrastructure")
                     (li "Automatic test failure blame assignment and task creation")
                     (li "Phabricator")
                     (li "Task management system")
                     (li "Dev-server allocation and management system")
                     (li "Real-time asynchronous job tier")
                     )))
             (li
               ,(employment "Ellington Management Group" "Old Greenwich, CT"
                            "Quantitative Developer"
                            "May 2006" "August 2009"
                  `(p "Developed and maintained various modelling and trading systems.")
                  `(ul
                     (li "Distributed computing infrastructure")
                     (li "Cross-language Services and Remote Procedure Call library")
                     (li "Quantitative Model implementation")
                     (li "Real-time market data streaming and aggregation")
                     (li "Automated trading and order management system")
                     )))
             ))
         (section
           (h2 "Education")
           (ul
             (li
               (h3 "Rochester Institute of Technology")
               ,(date-range "1999" "2004")
               (p "Bachelor of Science in Mechanical Engineering")
               (p "Concentration in Aerospace"))
             (li
               (h3 "Stanford Online Courses for AI and Machine Learning")
               ,(date-single "Fall 2011")
               (h4 "Statement of Accomplishment")
               (p "This was the original online offering of these two classes from which "
                  ,(anchor "https://www.coursera.org/" "coursera")
                  " and "
                  ,(anchor "https://www.udacity.com/" "udacity")
                  " were spun off."))))
         (section
           (h2 "Some reading I've enjoyed")
           (ul
             (li ,(anchor-SYJMrF "Surely You're Joking, Mr. Feynman!"))
             (li ,(anchor-GEB "Gödel, Escher, Bach"))
             (li ,(anchor-logicomix "Logicomix"))
             (li ,(anchor-prag-prog "The Pragmatic Programmer"))
             (li ,(anchor-peopleware "Peopleware"))
             (li ,(anchor-sicp "Structure and Interpretation of Computer Programs"))
             (li ,(anchor-algo-design-manual "The Algorithm Design Manual"))
             (li ,(anchor-okasaki "Purely Functional Data Structures"))
             (li ,(anchor-prog-pearls "Programming Pearls"))
             ))
         )))
  )
