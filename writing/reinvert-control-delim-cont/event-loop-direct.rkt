#lang racket
(provide
  (all-from-out "event-loop.rkt")
  async-op-direct)
(require
  "event-loop.rkt"
  racket/control)

(define (with-callbacks->direct aop)
  (lambda args
    (shift k (aop args
                  (lambda () (k #t))
                  (lambda () (k #f))))))

(define async-op-direct (with-callbacks->direct async-op))
