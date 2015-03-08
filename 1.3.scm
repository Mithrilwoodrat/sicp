#lang planet neil/sicp
;;;1.3用高阶数据做抽象
;;;;1.3.1用过程做参数
(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))
(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))
;;;;;;
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (inc n) (+ n 1))
(define (cube n) (* n n n))
(define (sum-cubes a b)
  (sum cube a inc b))
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* ( sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
;;;;;;;;1.29
(define (next n) (+ n 1))
(define (simpson f a b n)
  (define h (/ (- b a) n) );;h=(a-b)/n,n增大时近似值精度增加
  (define (y k) (f (+ a (* k h))))
  (define (factor k);;y的系数和下标的关系
    (cond ((or (= k 0) (= k n)) 
           1)
          ((odd? k)
           4)
          (else
           2)))
  (define (term k)
    (* (factor k)
       (y k)))
  (* (/ h 3) (sum term (exact->inexact 0) next n)))  ;;a+0h到a+nh=b的积分
;;;;;1.30
(define (sum2 term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter next(a) (+ result (term a)))))
  (iter a 0))
;;;1.31
(define (product2 term a next b)
  (if (> a b)
      1
      (* (term a)
         (product2 term (next a) next b))))
(define (product term a next b)
  (define (iter a result)
   (if (> a b)
       result
       (iter (next a) (* result (term a)))))
  (iter a 1))
(define (factorial n)
  (product (lambda (a) a) 1 inc n))      
;;;;1.31
;;π/4=(2/3)*(3/4)*(6/5)*(6/7)~~~((2n-1+1)/(2n-1+2))*(2n+2)/(2n+1)
(define (pi n)
  (define (numer-term i);分子
    (cond ((= 1 i)
          2)
          ((even? i)
           (+ i 2))
          (else (+ i 1))));分母
  (define (denom-term i)
    (if  (even? i)
           (+ i 1)
           (+ i 2)))
  (* 4 (exact->inexact
   (/ (product numer-term
               1
               inc
               n)
      (product denom-term
               1
               inc
               n)))))
;;;;1.32
(define (accmulate2 combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
             (accmulate2 combiner null-value term (next a) next b))))
(define (accmulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (combiner result (term a)))))
  (iter a null-value))
;;;1.33
(define (filtered-accumulate combine null-value term a next b valid?)
  (if (> a b)
      null-value
      (let ((rest-terms (filtered-accumulate combine
                                             null-value
                                             term
                                             (next a)
                                             next
                                             b
                                             valid?)))
        (if (valid? a)
            (combine (term a) rest-terms)
            rest-terms))))

(define (prime? n)
  (define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
  (define (divides? a b)
  (= (remainder b a) 0))
  (= n ((lambda (n)
         (find-divisor n 2)
         ) n )))
(define (prime-sum a b)
  (filtered-accumulate + 
                       0 
                       (lambda (x) x)
                       a
                       (lambda (x) (+ x 1))
                       b
                       prime?))


;;;;;;1.3.2用LAMBDA构造过程
(define (square a) (* a a))
(define (f1 x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1(* x y))
            (- 1 y)))
(define (f2 x y)
  ((lambda (a b)
     (+ (* x(square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))
(define (f3 x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))
;;;1.34
(define (f g)
  (g 2))
;;;1.3.3
;;;折办法求方程根
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (average a b)
  (/ (+ a b) 2.0))
;;;改进的折半法,如果给了两个相同符号的点则抛出错误
(define tolerance 0.00001);;;定义近似值范围
(define (close-enough? x y)
  (< (abs (- x y)) tolerance))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))
;;;;; 求不动点
(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
;;ex 求余弦函数的不动点(fixed-point cos 1.0)
;;求y=siny+cosy的一个解
;(fixed-point (lambda (y) (+ (sin y) (cos y)))
;             1.0)



;;y**2=x => y=x/y
;;设next(y)=(1/2)(y+y/x)
;(define (sqrt x)
;  (fixed-point (lambda (y) (/ x y))
;               1.0))
;这一搜寻不收敛.....在答案两边反复震荡。可以做出猜测使之不像y/x一样离y那么远
(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)));y+y=y+y/x => y=(1/2)(y+y/x)
                 1.0))
;;1.35
;(fixed-point (lambda (x) (+ 1 
;                            (/ 1 x)))
;               1.0)
;;1.36
(define (display-info guess step)
  (display "Step: ")
  (display step)
  (display " ")
  
  (display "Guess: ")
  (display guess)
  (newline))
(define (new-fixed-point f first-guess)
  (define (try guess step)
    (display-info guess step)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          (begin
            (display-info next (+ 1 step))
             next)
          (try next (+ 1 step)))))
  (try first-guess 1))

;求x^x=1000即求x->log(1000)/log(x)的不动点
(define formula
  (lambda (x)
    (/ (log 1000)
       (log x))))
;(new-fixed-point formula 1.1)
;平均阻尼函数
(define (average-damp f)
  (lambda (x)
    (average x
             (f x))))
;(new-fixed-point (average-damp formula) 1.1)

;;;;;1.37无穷连分式
(define (cont-frac2 n d k)
  (define (cf i) 
    (if (= k i)
      (/ (n k) (d k))
      (/ (n i) 
         (+ (d i) 
            (cf (+ i 1))))))
  (cf 1))
(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1)
              (/ (n i)
                 (+ (d i) result)))))
  (iter (- k 1) (/ (n k) (d k))))
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)
;;;;1.38 e
(define (e k)
  (define (N i)
    1)
  (define (D i)
    (if (= 0 (remainder (+ i 1) 3))
        (* 2 (/ (+ i 1) 3))
        1))
  (+ 2.0
     (cont-frac N D k)))
;;;1.39 tan
(define (tan-cf x k)
  (define (N i)
    (if (= i 1)
        x
        (- (square x))))
  (define (D i)
    (- (* 2.0 i) 1))
  (cont-frac N D k))

                 
        
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  