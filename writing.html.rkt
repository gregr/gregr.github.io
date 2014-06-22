#lang racket
(provide
  writing
  writing-names
  writing-pages
  )

(require
  "common.rkt"
  "static-site.rkt"
  )

(define nav
  `(nav ((id "nav-local"))))

(define (writing-content title . rest)
  (content
    title
    nav
    `(article
       (h1 ((class "content-title")) ,title)
       ,@rest)))

(define writing
  (writing-content
    "Misguided Writing"
    `(section
       (p "This is where you will find my attempts at meaningful writing.")
       (p ,(node-ref 'test))
       )
    ))

(define (read-all inp)
  (let loop ((forms '()))
    (let ((form (read inp)))
      (if (eq? form eof)
        (cons 'begin (reverse forms))
        (loop (cons form forms))))))
(define writing-namespace
  (let ((local-ns (current-namespace)))
    (parameterize ((current-namespace (make-base-namespace)))
      (namespace-attach-module local-ns "common.rkt")
      (namespace-attach-module local-ns "static-site.rkt")
      (namespace-require "common.rkt")
      (namespace-require "static-site.rkt")
      (namespace-set-variable-value! 'writing-content writing-content)
      (current-namespace))))

(define writing-names
  '(
    test
    ))

(define writing-paths
  (for/list ((name writing-names))
    (string-append "writing/" (symbol->string name) ".html.rkt")))
(define writing-results
  (for/list ((path writing-paths))
    (eval (call-with-input-file path read-all) writing-namespace)))
(define writing-pages
  (for/list ((name writing-names) (result writing-results))
    (let ((desc (car result)) (xexpr (cadr result)))
      (list name desc xexpr))))
