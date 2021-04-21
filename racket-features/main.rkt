#lang racket
 
(define (print-cake n)
  (show "   ~a   " n #\.)
  (show " .-~a-. " n #\|)
  (show " | ~a | " n #\space)
  (show "---~a---" n #\-))
 
(define (show fmt n ch)
  (printf fmt (make-string n ch))
  (newline))
 
; (module* main #f
;   (print-cake 10))

(module+ main
  (print-cake 10)
  (print-cake 20)
  (print-cake 30))