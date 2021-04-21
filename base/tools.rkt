#lang racket

(provide (all-defined-out))

(define (measure p)
    (define start (current-inexact-milliseconds))
    (define result (p))
    (define end (current-inexact-milliseconds))
    (fprintf (current-output-port)
             "result: ~s ellapsed: ~sms\n"
             result
             (- end start)))