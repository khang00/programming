#lang racket
;; chapter 1.1: The elements of programming
(define (sum-sqr a b c) (- (+ (sqr a) (sqr b) (sqr c) (sqr (min a b c)))))

(define (square-root number)
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (average x y) (/ (+ x y) 2))
  (define (good-enough? guess x)
    (define epsilon (expt 2 -52))
    (define tolerance (* 1.5 epsilon))
    (define min-float (expt 2 -1022))
    (< (abs (- (sqr guess) x)) (if (= x 0)
                                   min-float
                                   (* tolerance x))))
  (sqrt-iter 1.0 number))

(define (cube-root number)
  (define (cube-iter guess x)
    (if (good-enough? guess x)
        guess
        (cube-iter (improve guess x) x)))
  (define (improve guess x)
    (/ (+ (/ x (sqr guess)) (* 2 guess)) 3))
  (define (good-enough? guess x)
    (define epsilon (expt 2 -52))
    (define tolerance (* 1.5 epsilon))
    (define min-float (expt 2 -1022))
    (< (abs (- (* guess (sqr guess)) x)) (if (= x 0)
                                             min-float
                                             (* tolerance x))))
  (cube-iter 1.0 number))

;; chapter 1.2: Procedures and the process their generate
(define (f-recur n)
  (if (< n 3)
      n
      (+ (f-recur (- n 1))
         (* (f-recur (- n 2)) 2)
         (* (f-recur (- n 3)) 3))))

(define (f-iter number)
  (define (cal-sum f0 f1 f2)
    (+ f0
       (* 2 f1)
       (* 3 f2)))
  (define (loop f0 f1 f2 i n)
    (if (> i n)
        f0
        (loop (cal-sum f0 f1 f2)
              f0
              f1
              (+ i 1)
              n)))
  (if (< number 3)
      number
      (loop 2 1 0 3 number)))

(define (expt-recur b n)
  (cond ((= n 0) 1)
        ((even? n) (sqr (expt-recur b (/ n 2))))
        ((odd? n) (* b (expt-recur b (- n 1))))))

(define (expt-iter base exponent)
  (define (loop b prod ex)
    (cond ((= 0 ex) prod)
          ((even? ex) (loop (sqr b) prod (/ ex 2)))
          ((odd? ex) (loop b (* prod b) (- ex 1)))))
  (loop base 1 exponent))

(define (multiply a b)
  (cond ((= b 0) 0)
        ((even? b) (double (multiply a (halve b))))
        ((odd? b) (+ a (multiply a
                                 (- b 1))))))

(define (multiply-iter a b)
  (define (loop a b sum)
    (cond ((= b 0) sum)
          ((even? b) (loop (double a)
                           (halve b)
                           sum))
          ((odd? b) (loop a
                          (- b 1)
                          (+ sum a)))))
  (loop a b 0))

(define (double number)
  (+ number number))

(define (halve number)
  (arithmetic-shift number -1))

(define (fibonacci n)
  (define initial-vector (list 1 0))
  (define (transform left right)
    (define a (car right))
    (define b (car (cdr right)))
    (map (lambda (row)
           (list (+ (* (car row) a)
                    (* (car (cdr row)) b))))
         left))
  (define initial-matrix (list (list 1 1) (list 1 0)))
  (define (mul-matrix-2d left right)
    (define top-row (car right))
    (define bottom-row (car (cdr right)))
    (map (lambda (row)
           (list (+ (* (car row) (car top-row))
                    (* (car (cdr row)) (car bottom-row)))
                 (+ (* (car row) (car (cdr top-row)))
                    (* (car (cdr row)) (car (cdr bottom-row))))))
         left))
  (define (sqr-matrix-2d matrix)
    (mul-matrix-2d matrix matrix))
  ;; fibonacci calculate with recursive process
  (define (fibonacci-matrix base n)
    (cond ((= n 1) base)
          ((even? n) (sqr-matrix-2d (fibonacci-matrix base (/ n 2))))
          ((odd? n) (mul-matrix-2d (fibonacci-matrix base (- n 1))
                                base))))
  ;; fibonacci calculate with iterative process
  (define identity-matrix (list (list 1 0) (list 0 1)))
  (define (loop prod base exponent)
    (cond ((= exponent 1) (mul-matrix-2d prod base))
          ((even? exponent) (loop prod
                                  (sqr-matrix-2d base)
                                  (/ exponent 2)))
          ((odd? exponent) (loop (mul-matrix-2d prod base)
                                 base
                                 (- exponent 1)))))
  (define (get-number ls) (car (car (cdr ls))))
  (get-number (transform (loop identity-matrix
                             initial-matrix
                             n)
                       initial-vector)))

;; Abstraction with procedures
(define (cube x) (* x x x))

(define (integrate f a b n)
  (define h (/ (- b a) n))
  (display h)
  (define (loop i sum)
    (cond ((>= i n) (+ sum (f (+ a (* i h)))))
          ((= i 0) (loop (+ i 1)
                         (+ sum (f (+ a (* i h))))))
          ((odd? i) (loop (+ i 1)
                          (+ sum (* 4 (f (+ a (* i h)))))))
          ((even? i) (loop (+ i 1)
                           (+ sum (* 2 (f (+ a (* i h)))))))))
  (/ 3 (* h (loop 0 0))))

;; Procedure as retunred values
(define (repeat-apply f n)
  (define (loop h i)
    (cond ((= i n) h)
          ((< i n) (loop (compose f h)
                         (+ i 1)))))
  (loop f 0))

(define (iterative-improvement good? improve)
  (define (iter guess)
    (if (good? guess) guess
        (iter (improve guess))))
  iter)

(define (sqrt-improve x)
  (define (improve guess) (average guess (/ x guess)))
  (define (average a b) (/ (+ a b) 2))
  (define (good? guess)
    (define epsilon (expt 2 -52))
    (define tolerance (* 1.5 epsilon))
    (define min-float (expt 2 -1022))
    (< (abs (- (sqr guess) x)) (if (= x 0)
                                   min-float
                                   (* tolerance x))))
  ((iterative-improvement good? improve) 1.0))
