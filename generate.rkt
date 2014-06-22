#lang racket
(require
  "static-site.rkt"
  )
(require
  "main.css.rkt"
  )
(require
  "about.html.rkt"
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; an actual site
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-site "."
  ; path structure
  (about)

  ; page definitions
  (about "About" about)
  )
