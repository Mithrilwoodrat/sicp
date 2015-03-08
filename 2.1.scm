;(make-rat <n> <d>)返回一个有理数的分子分母
;(numer <x>)返回分子 (denom <x>)返回有理数x的分母
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< d 0)
	(cons (- (/n g))
	      (- (/d g)))
	(cons (/ n g) (/ d g)))))
(define (numer x) (car x))
(define (denom x) (cdr x))
;有理数加法
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define  (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))
;(define x (cons 1 2))

(define one-half (make-rat 1 2))

;;;;2.2
(define (make-segment x y)
  (cons x y))
(define (start-segment x) (car x))
(define (end-segment x) (cdr x))
(define (make-point x y)
  (cons x y))
(define (x-point x) (car x))
(define (y-point x) (cdr x))
(define (midpoint-segment segment)
  (let ((start (start-segment segment))
	(end (end-segment segment)))
  (make-point (/ (+ (x-point start)
		    (x-point end))
		    2)
	      (/ (+ (y-point start)
		    (y-point end))
		    2))))
(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ".")
  (display (y-point p))
  (display ")")
  (newline))

;; (print-point(midpoint-segment
;; 	     (make-segment 
;; 	      (make-point 0 0) 
;; 	      (make-point 2 2))))

;;;;;2.1.3
;; (define (cons x y)
;;   (define (dispatch m)
;;     (cond ((= m 0) x)
;; 	  ((= m 1) y)
;; 	  (else (error "Argument not 0 or 1 -- CONS" m))))
;;   dispatch)
;; (define (car z) (z 0))
;; (define (cdr z) (z 1))
;;;2.4
;; (define (cons x y)
;;   (lambda (m) (m x y)))
;; (define (car z)
;;   (z (lambda (p q) q)))
;; (define (cdr z)
;;   (z (lambda (p q) p)))
;;;;2.5
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define add
  (lambda (m n)
      (lambda (f)
	(lambda (x)
	  ((m f) ((n f) x))))))
;;;2.1.4
(define (make-interval a b) (cons a b))
(define (upper-bound x) (car x))
(define (lower-bound x) (cdr x))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))
	 
;;;;2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
		 (- (lower-bound y) (upper-bound x))



