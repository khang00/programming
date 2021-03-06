#lang racket

;; Numbers arithemic
(define x 3)
(define y 4)
(sqrt (+ (sqr x) (sqr y)))

;; String arithemic 
(define str "helloworld")
(define i 5)
(string-append (substring str 0 i)
               "_"
               (substring str i
                          (string-length str)))

(string-append (substring str 0 i)
               (substring str (+ i 1) (string-length str)))

;; Boolean arithemic
(define sunny #true)
(define friday #false)
(or (not sunny) friday)

;; Predicate
(define in "random shit")
(cond [(string? in) (string-length in)]
      [(number? in) (if (> in 0) (- in 1) in)]
      [(boolean? in) (if in 10 20)])
