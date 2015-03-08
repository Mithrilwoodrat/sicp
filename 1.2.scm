#lang racket
(define (square n)
  (* n n))
;;;1.2
(define (factorial1 n)
  (if (= n 1)
      1
      (* n (factorial1 (- n 1)))))
(define (factorial2 n)
  (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))
;;;;1.10
(define (A x y)
  (cond ( (= y 0) 0)
        ( (= x 0) (* 2 y))
        ( (= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
;;1.2.2
(define (fib1 n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib1(- n 1))
                 (fib1(- n 2))))))
(define (fib2 n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))
;;;;;换你妹的零钱啊靠
(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0 )
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))
;;;;;1.11
(define (f1 n)
  (cond ((< n 3) n)
        ((>= n 3) (+ (f1 (- n 1))
                     (* 2 (f1 (- n 2)))
                     (* 3 (f1 (- n 3)))))))
(define (f2 n)
  (define(iter a b c count)
    (if (< count 3)
        c
        (iter b c (+ (* 3 a) (* 2 b) c) (- count 1))))
  (if(< n 3)
  n
  (iter 0 1 2 n)))
;;;;1.12
(define (pascal row col)
  (cond ((> col row)
         (error "unvalid col value"))
        ((or (= col 0) (= row col))
         1)
        (else (+ (pascal (- row 1) (- col 1))
                 (pascal (- row 1) col)))))

;;;;;1.2.3
;;1.15
(define (sine angle)
  (define (cube x) (* x x x))
  (define (p x) (- (* 3 x) (* 4 (cube x))))
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;;;;1.2.4
(define (expt1 b n)
  (if (= n 0)
      1
      (* b ( expt1 b (- n 1)))))
(define (expt2 b n)
  (define (expt-iter  counter product)
    (if (= counter 0)
        product
        (expt-iter (- counter 1)
                   (* b product))))
  (expt-iter n 1))
(define (fast-expt1 b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt1 b (/ n 2))));;2^n=(2^(1/2n))^2
        (else (* b (fast-expt1 b (- n 1))))))
(define (even? n)
  (= (remainder n 2) 0))
;;;1.17
(define (fast-expt2 b n)
  (define (fast-expt-iter product counter)
    (cond ((= counter 1) product)
    ((even? counter)  (fast-expt-iter 
                             (square product)
                             (/ counter 2)))
    (else (fast-expt-iter (* b product)
                          (- counter 1)))))
  (if (= n 0) 
      1
  (fast-expt-iter b n)))
;;;1.17
(define (multiply1 a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))
(define (fast-mult1 a  b)
  (cond ((= b 0) 0)
        ((even? b) (* (fast-mult1 a (/ b 2)) 2))
        (else (+ (fast-mult1 a (- b 1)) a))))
(define (fast-mult2 a b)
  (define (mult-iter a b product)
    (cond ((= b 0) product)
          ((even? b) (mult-iter (* a 2) (/ b 2) product))
          (else (mult-iter a  (- b 1) (+ a product)))))
  (mult-iter a b 0))
;;;;;1.19
(define (fib n)
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (square p) (square q))
                     (+ (* 2 p q) (square q))
                     (/ count 2)))
          (else
           (fib-iter (+ (* b q) (* a q) (* a p))
                     (+ (* b p) (* a q))
                     p
                     q
                     (- count 1)))))
  (fib-iter 1 0 0 1 n))
;;;;;;;;;;;;;;;;我是万恶的分割线;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define (gcd a b)
;  (if (= b 0)
;      a
;      (gcd b (remainder a b))))

;;;1.2.6
(define (smallest-divisor n)
  (find-divisor  n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))
;;;;
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2 ) m))
          m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
        
;;;;;1.22
(define (next-odd n)
  (if (odd? n) (+ n 2)
      (+ n 1)))
(define (continue-primes n count)
  (cond ((= count 0)
         (display "are primes"))
        ((prime? n)
         (display n)
         (newline)
         (continue-primes (next-odd n) (- count 1)))
        (else
         (continue-primes (next-odd n) count))))
;;;;;1.27
(define (car-test n)
  (define (test-iter a n)
    (cond ((= a n)
           true)
          ((congruent? a n)
           (test-iter (+ a 1) n))
          (else
           true)))
  (define (congruent? a n)
    (= (expmod a n n) a))
  (test-iter 1 n))
         
                   
        
      
  
  
                  

                  