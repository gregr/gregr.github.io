#lang racket
(require
  "event-loop-direct.rkt"
  racket/control)

(define (direct)
  (reset
    (displayln "perform an async operation")
    (if (async-op-direct 'arg1 'arg2)
      (begin
        (displayln "handle success and perform another operation")
        (if (async-op-direct 'arg3 'arg4)
          (displayln "handle success again")
          (displayln "handle failure of second operation")))
      (displayln "handle failure of first operation"))))

(displayln "direct")
(direct)
(event-loop)
