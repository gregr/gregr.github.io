#lang racket
(require xml)

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
  (string-append "<!DOCTYPE html>" (xexpr->pretty-string xexpr)))
(define (write-html-file path xexpr)
  (define-values (directory filename is-root) (split-path path))
  (when (path? directory) (make-dirs (explode-path directory)))
  (call-with-output-file path #:exists 'replace
    (curry display (xexpr->html-string xexpr))))

(define (reachable graph roots)
  (let loop ((reached (set))
             (roots (list->set (sequence->list roots))))
    (if (set-empty? roots) reached
      (let* ((reached (set-union reached roots))
             (nexts (map (curry dict-ref graph) (set->list roots)))
             (next (apply set-union nexts))
             (next (set-subtract next reached)))
        (loop reached next)))))
