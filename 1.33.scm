#lang racket
;;;;;1,33
;(define (filtered-accumulate combine null-value term a next b valid?)
;  (if (> a b)
;      null-value
 ;     (let ((result-terms (filtered-accumulate combine 
  ;                                             null-value
   ;                                            term
    ;                                           (next a)
     ;                                          next
      ;                                         b
       ;                                        valid?)))
        ;(if (valid? a)
         ;   (combine (term a) result-terms)
          ;  result-terms))))
;;;;书上的素数检测
(define (square n)
  (* n n))
(define (inc n)
  (+ n 1))


(define (prime? n)
(define (smallest-divisor n)
  (find-divisor  n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
  (= n (smallest-divisor n)))
;;;1.33a
(define (sum-primes a b)
  (filtered-accumulate + 
                       0 
                       (lambda (x) x) 
                       a 
                       inc 
                       b 
                       prime?));;;理论上说1不是素数
;;;1.33b
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(define (coprime? a b)
  (= (gcd a b) 1));;定义一个检测互质的函数
(define (product-of-coprimes n)
  (filtered-accumulate * 
                       1 
                       (lambda (x) x) 
                       1 
                       inc 
                       n 
                       (lambda (x) (coprime? x n))))
;;;;迭代版
(define (filtered-accumulate combine null-value term a next b valid?)
  (define (iter i result)
    (cond ((> i b)
           result)
          ((valid? i)
           (iter (next i) (combine (term i) result)))
          (else
           (iter (next i) result))))
  (iter a null-value))
