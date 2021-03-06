#lang racket

;; Function 
(define (distance-to-origin x y)
  (sqrt (+ (sqr x) (sqr y))))

(define (cube-volume a)
  (* a a a))

(define (cube-surface-area a)
  (sqr a))

(define (string-head str)
  (string-ref str 0))

(define (string-last str)
  (string-ref str (- (string-length str) 1)))

(define (==> left right)
  (or (not left) right))

;; Function composition
(define (profit ticket-price)
  (define attendees 
    (round (+ 120 (* 15 (/ (- 5 ticket-price) 0.1)))))
  (define cost
    (* attendees 1.5))
  (define revenues
    (* ticket-price attendees))
  (- revenues cost))
