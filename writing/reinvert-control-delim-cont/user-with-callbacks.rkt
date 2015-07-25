#lang racket
(require
  "event-loop.rkt")

(define (with-callbacks)
  (displayln "perform an async operation")
  (async-op
    (list 'arg1 'arg2)
    (lambda ()
      (displayln "handle success and perform another operation")
      (async-op
        (list 'arg3 'arg4)
        (lambda () (displayln "handle success again"))
        (lambda () (displayln "handle failure of second operation"))))
    (lambda () (displayln "handle failure of first operation"))))

(displayln "with-callbacks")
(with-callbacks)
(event-loop)
