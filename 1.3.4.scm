(define (square x) (* x x))
(define (cube x) (* x x x))
(define (average a b)
  (/ (+ a b) 2.0))
(define (average-damp f)
  (lambda (x) (average x (f x))))
;;;
(define tolerance 0.00001);;;定义近似值范围
(define (close-enough? x y)
  (< (abs (- x y)) tolerance))
(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))
;;;;;;newtown6

(define (deriv g)
  (define dx 0.00001)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
;((deriv cube) 5)


(define (newtown-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newtown-transform g) guess))
;(define (sqrt x)
;  (newtons-method (lambda (y) (- (square y) x))
;		 1.0))
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

;;(define (sqrt x)
;; (fixed-point-of-transform (lambda (y) (/ x y))
;;			    average-damp
;;			    1.0))
 (define (sqrt x)
   (fixed-point-of-transform (lambda (y) (- (square y) x))
 			    newtown-transform
 			    1.0))

;;;;;;1.40
(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a (* x x))
       (* b x)
       c)))
(define (cubic-point a b c )
  (newtons-method (cubic a b c) 1.0))
;;;1.41
(define (double f)
  (lambda (x)
    (f (f x))))
;(((double (double double)) 1+) 5)
;((double square) 2)

;;;;;;1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))
;;;;1.43
;; (define (repeated f n)
;;   (if (= n 1)
;;        f
;;        (lambda (x)
;;              (f  ((repeated  f (- n 1)) x)))))

;; (define  (repeated f n)
;;   (if (= n 1)
;;       f
;;       (compose f 
;; 	       (repeated f (- n 1)))))

;;迭代版
(define (repeated f n)
  (define (iter i repeated-f)
    (if (= i n)
	repeated-f
	(iter (+ 1 i)
	      (compose f repeated-f))))
  (iter 1 f))
;;;;1.44
(define (smooth f)
  (define dx 0.0001)
  (lambda (x)
    (/ (+ (f x)
	  (f (- x dx))
	  (f (+ x dx)))
       3)))
;; (define (smooth-n-times f n)
;;   (if (= n 0)
;;       f
;;       (smooth (smooth-n-times f (- n 1)))))

;; (define (smooth-n-times f n)
;;   (define (iter i smoothed-f)
;;     (if (= 0 n)
;; 	smoothed-f
;; 	(iter (- i 1) (smooth smoothed-f))))
;;   (iter n f))


(define (smooth-n-times f n)
  ((repeated smooth n) f))

;;;;1.45
(define (expt base n)
  (if (= n 0 )
      1
      ((repeated (lambda (x) (* base x)) n) 1)))

(define (average-damp-n-times n f)
  ((repeated average-damp n) f))

(define (damped-nth-root n damp-times)
  (lambda (x)
    (fixed-point
     (average-damp-n-times damp-times
			   (lambda (y)
			     (/ x (expt y (- n 1)))))
     1.0)))

;(define sqrt (damped-nth-root 2 1))
(define 4th-root (damped-nth-root 4 2))

;;;;;;;1.46
