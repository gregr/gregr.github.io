#lang racket

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



(require racket/control)

(define (with-callbacks->direct aop)
  (lambda args
    (shift k (aop args
                  (lambda () (k #t))
                  (lambda () (k #f))))))

(define async-op-direct (with-callbacks->direct async-op))



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
