(define (sum-of-n n limit)
  (define (sum x)
    (/ (* x (+ x 1)) 2.0))
  (* (sum (div (- limit 1) n)) n))
 (- (+ (sum-of-n 3 1000) (sum-of-n 5 1000)) (sum-of-n 15 1000))