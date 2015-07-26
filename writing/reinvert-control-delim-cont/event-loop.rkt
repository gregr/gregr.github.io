#lang racket
(provide
  async-op
  event-loop)

(struct async-operation (compute finish))

(define pending (make-channel))
(define finished (make-channel))

(define (pending-loop)
  (match-let (((async-operation compute finish) (channel-get pending)))
    (thread (lambda ()
              (let ((result (compute)))
                (channel-put finished (lambda () (finish result)))))))
  (pending-loop))
(void (thread pending-loop))

(define (event-loop)
  (let loop ()
    ((channel-get finished))
    (loop)))

(define (pending-add aop) (channel-put pending aop))

(define (async-op args succeed fail)
  (pending-add
    (async-operation
      (lambda ()
        (displayln (format "async operation started with: ~v" args))
        (sleep 2)
        (displayln "async operation finished")
        (random 2))
      (lambda (result) (if (= 0 result) (succeed) (fail))))))
