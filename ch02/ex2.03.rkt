#lang racket

(require "ex2.02.rkt")

; top layer
; (define (perimeter rect)
;     (* (+ (len rect)
;           (wid rect))
;        2))

; (define (area rect)
;     (* (len rect) 
;        (wid rect)))

; fake layer for testing
; the right definition is up there
(define (perimeter rect len wid)
    (* (+ (len rect)
          (wid rect))
       2))

(define (area rect len wid)
    (* (len rect) 
       (wid rect)))

; rectangle representation 1: 
; uses three points to represent a rectangle
(define (make-rect-1 start x-point y-point)
    (cons start (cons x-point y-point)))
(define (len-1 rect) 
    (- (x-coor (car (cdr rect)))
       (x-coor (car rect))))
(define (wid-1 rect)
    (- (y-coor (cdr (cdr rect)))
       (y-coor (car rect))))

; rectangle representation 2:
; uses a start point, length, width
(define (make-rect-2 start length width)
    (cons start (cons length width)))
(define (len-2 rect) (car (cdr rect)))
(define (wid-2 rect) (cdr (cdr rect)))

; the testing rectangle:
; ^ (3, 13)
; |
; |
; |
; |____________> (7, 4)
; (3, 4)
; perimeter: 26
; area: 36

(module+ test
    (require rackunit)
    (let ((rect (make-rect-1 (make-point 3 4)
                             (make-point 7 4)
                             (make-point 3 13))))
         (check-eq? (perimeter rect len-1 wid-1) 26)
         (check-eq? (area rect len-1 wid-1) 36))
    (let ((rect (make-rect-2 (make-point 3 4)
                             4
                             9)))
         (check-eq? (perimeter rect len-2 wid-2) 26)
         (check-eq? (area rect len-2 wid-2) 36)))

; to run the test: raco test ex2.03.rkt