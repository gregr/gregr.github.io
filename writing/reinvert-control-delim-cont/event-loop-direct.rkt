#lang racket
(provide
  (all-from-out "event-loop.rkt")
  async-op-direct
  set-timeout-direct)
(require
  "event-loop.rkt"
  racket/control)

(define (async-op-direct . args)
  (shift k (async-op args
                     (lambda () (k #t))
                     (lambda () (k #f)))))

(define (set-timeout-direct latency)
  (shift k (set-timeout latency (lambda () (k (void))))))
