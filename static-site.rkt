#lang racket
(provide
  define-anchors
  define-css
  define-site
  node-ref
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

(struct node-ref (name) #:transparent)
(define (tree-lift-refs name=>data tree)
  (match tree
    ((node-ref name) (list (set name) (dict-ref name=>data name)))
    ((? list?)
     (match-let (((list names trees)
                  (apply (curry map list)
                         (map (curry tree-lift-refs name=>data) tree))))
       (list (apply set-union names) trees)))
    (_ (list (set) tree))))

(define (path-tree->name=>path path-root ptree)
  (define (attr-add attrs name base filename)
    (when (dict-has-key? attrs name)
      (error (format "path tree contains duplicate entries for: ~a" name)))
    (let ((path (apply build-path (reverse (cons filename base)))))
      (dict-set attrs name path)))
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
  (let* ((attrs (synthesize-fold (hash) (list path-root) (cdr ptree)))
         (attrs (attr-add attrs (car ptree) (list path-root) "index.html")))
    attrs))
(define (pages->name=>desc pages)
  (make-immutable-hash (map cons (map car pages) (map cadr pages))))
(define (pages->name=>xexpr pages)
  (make-immutable-hash (map cons (map car pages) (map caddr pages))))

(define (define-site path-root ptree pages)
  (define dict-keys->set (compose1 list->set dict-keys))
  (define page-root (car ptree))
  (define name=>path (path-tree->name=>path path-root ptree))
  (define name=>desc (pages->name=>desc pages))
  (let ((path-names (dict-keys->set name=>path))
        (desc-names (dict-keys->set name=>desc)))
    (unless (equal? path-names desc-names)
      (error (format "missing page list names: ~a; missing path tree names: ~a"
                     (set->list (set-subtract path-names desc-names))
                     (set->list (set-subtract desc-names path-names))))))
  (define name=>anchor
    (for/hash (((name path) (in-dict name=>path)))
      (let ((desc (dict-ref name=>desc name))
            (href (string-append "/" (path->string path))))
        (values name `(a ((href ,href)) ,desc)))))
  (define name=>reachable+xexpr
    (for/hash (((name xexpr) (in-dict (pages->name=>xexpr pages))))
      (values name (tree-lift-refs name=>anchor xexpr))))
  (define name=>reachable
    (for/hash (((name r+x) (in-dict name=>reachable+xexpr)))
      (values name (car r+x))))
  (define name=>xexpr
    (for/hash (((name r+x) (in-dict name=>reachable+xexpr)))
      (values name (cadr r+x))))
  (define pages-reached (reachable name=>reachable (list page-root)))
  (define pages-not-reached
    (set-subtract (list->set (map car pages)) pages-reached))
  (unless (set-empty? pages-not-reached)
    (error (format "unreachable pages: ~a" (set->list pages-not-reached))))
  (for (((name path) (in-dict name=>path)))
    (write-html-file path (dict-ref name=>xexpr name))))

(define-for-syntax (identifier-prefixed prefix ident)
  (datum->syntax
    ident
    (string->symbol
      (string-append prefix (symbol->string (syntax->datum ident))))))

(require (for-syntax racket/function))
(define-syntax (define-anchors stx)
  (syntax-case stx ()
    ((_ (name url) ...)
     (with-syntax (((anchor-name ...)
                    (map (curry identifier-prefixed "anchor-")
                         (syntax->list #'(name ...)))))
      #'(begin
          (define (anchor-name desc)
            `(a ((href ,url)) ,desc)) ...)))))
