#lang racket
(require xml)

(define (xexpr->pretty-string xexpr)
  (call-with-output-string
    (curry display-xml/content (xexpr->xml xexpr))))
(define (xexpr->html-string xexpr)
  (string-append "<!DOCTYPE html>" (xexpr->pretty-string xexpr)))
(define (write-html-file path xexpr)
  (call-with-output-file path #:exists 'replace
    (curry display (xexpr->html-string xexpr))))
