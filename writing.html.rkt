#lang racket
(provide
  writing
  writing-feed
  writing-names
  writing-pages
  )

(require
  "common.rkt"
  "static-site.rkt"
  gregr-misc/list
  gregr-misc/sugar
  racket/runtime-path
  )

(define names-dates
  '((creative-workflow ("2015-03-17" "2015-03-18"))
    (reluctant-bash-scripting ("2014-10-25" "2014-12-07"))
    (staying-sane-writing-software ("2014-06-22" "2014-06-22"))
    ))
(define writing-names (forl (list name _) <- names-dates
                            name))
(define published-updated (forl (list _ pub-upd) <- names-dates
                                pub-upd))

(define home-href (string-append "http://www." domain))
(define (writing-href name)
  (string-append home-href "/writing/" (symbol->string name)))

(define writing-nodes
  (for/list ((name writing-names))
    `(li ,(node-ref name))))
(define (writing-content title nav . rest)
  (content
    title
    nav
    `(article
       (h1 ((class "content-title")) ,title)
       ,@rest)
    (list (atom-feed-link))))

(define writing
  (list
    "Writing"
    (writing-content
      "Writing"
      (nav-local '())
      `(section
         (ul ,@writing-nodes)
         )
      )))

(define (read-all inp)
  (let loop ((forms '()))
    (let ((form (read inp)))
      (if (eq? form eof)
        (cons 'begin (reverse forms))
        (loop (cons form forms))))))
(define-runtime-module-path common-rkt "common.rkt")
(define-runtime-module-path static-site-rkt "static-site.rkt")
(define writing-namespace
  (let ((local-ns (current-namespace))
        (ns-mods (list common-rkt static-site-rkt 'racket/match)))
    (parameterize ((current-namespace (make-base-namespace)))
      (for ((mod ns-mods))
        (namespace-attach-module local-ns mod)
        (namespace-require mod))
      (namespace-set-variable-value! 'writing-content writing-content)
      (current-namespace))))
(define-runtime-path writing-root "writing/")
(define writing-paths
  (for/list ((name writing-names))
    (build-path writing-root (string-append (symbol->string name) ".html.rkt"))))
(define writing-results
  (for/list ((path writing-paths))
    (eval (call-with-input-file path read-all) writing-namespace)))
(define writing-pages
  (for/list ((name writing-names) (result writing-results))
    (let ((desc (car result)) (xexpr (cadr result)))
      (list name desc xexpr))))

(define writing-feed
  (atom-feed
    (string-append home-href "/" "writing.xml")
    home-href
    (forl
      (list (list name desc xexpr)
            (list published updated)) <- (zip* writing-pages published-updated)
      (atom-entry desc (symbol->string name) (writing-href name)
                  published updated desc))))
