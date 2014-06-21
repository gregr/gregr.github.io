#lang racket
(provide
  define-anchors
  define-css
  define-site
  )

(require
  gregr-misc/file
  gregr-misc/match
  xml
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; file generation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (xexpr->pretty-string xexpr)
  (call-with-output-string
    (curry display-xml/content (xexpr->xml xexpr))))
(define (xexpr->html-string xexpr)
  (string-append "<!DOCTYPE html>" (xexpr->string xexpr)))

(define (write-html-file path xexpr)
  (display-to-file* (xexpr->html-string xexpr) path))

(define (css-style->string style)
  (let ((parts
          (for/fold/match ((parts '()))
                          (((cons field value) (dict->list style)))
            (cons (format "  ~a: ~a;\n" field value) parts))))
    (string-append "{\n" (apply string-append (reverse parts)) "}\n")))

(define (css->string css)
  (let ((parts
          (for/fold/match ((parts '()))
                          (((cons selector style) css))
            (cons (string-append selector " " (css-style->string style)) parts))))
    (apply string-append (reverse parts))))

(define (write-css-file path css)
  (display-to-file* (css->string css) path))

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

(define-for-syntax (identifier-prefixed prefix ident)
  (datum->syntax
    ident
    (string->symbol
      (string-append prefix (symbol->string (syntax->datum ident))))))

(define-syntax (define-anchors stx)
  (syntax-case stx ()
    ((_ (name url) ...)
     (with-syntax (((anchor-name ...)
                    (map (curry identifier-prefixed "anchor-")
                         (syntax->list #'(name ...)))))
      #'(begin
          (define (anchor-name desc)
            `(a ((href ,url)) ,desc)) ...)))))
