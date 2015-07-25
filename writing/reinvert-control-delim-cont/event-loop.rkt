#lang racket
(provide
  async-op
  event-loop)

(define async-ops (box '()))

(define (event-loop)
  (match (unbox async-ops)
    ('() 'done)
    ((cons op ops)
     (set-box! async-ops ops)
     (op)
     (event-loop))))

(define (async-op args succeed fail)
  (define (op)
    (displayln (format "async operation started with: ~v" args))
    (sleep 2)
    (displayln "async operation finished")
    (if (= 0 (random 2)) (succeed) (fail)))
  (set-box! async-ops (append (unbox async-ops) (list op))))
