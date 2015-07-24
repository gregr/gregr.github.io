#lang racket

(define (async-op args succeed fail)
  (displayln (format "async operation started with: ~v" args))
  (sleep 2)
  (displayln "async operation finished")
  (if (= 0 (random 2)) (succeed) (fail)))

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

(require racket/control)

(define (async-op-direct . args)
  (shift k
    (async-op args (lambda () (k #t)) (lambda () (k #f)))))

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

(displayln "with-callbacks")
(with-callbacks)
(displayln "direct")
(direct)
