#lang racket
(require
  "event-loop-direct.rkt"
  racket/control)

(define-syntax-rule (wait-until body ...)
  (let loop ()
    (set-timeout-direct 1)
    (displayln "are we there yet?")
    (if (begin body ...) (displayln "we're there!")
      (begin (displayln "no, not there yet") (loop)))))

(define (direct-concurrency)
  (define count 10)
  (define results (box '()))
  (define (add-result result)
    (set-box! results (cons result (unbox results))))
  (reset
    (displayln "perform async operations concurrently")
    (for ((index (range count)))
      (reset (add-result (async-op-direct index))))
    (wait-until (= count (length (unbox results))))
    (displayln results)))

(displayln "direct-concurrency")
(direct-concurrency)
(event-loop)
