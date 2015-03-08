(load "sicp.scm")
;; (define (sum-odd-squares tree)
;;   (cond ((null? tree) 0)
;; 	((not (pair? tree))
;; 	 (if (odd? tree) (square tree) 0))
;; 	(else (+ (sum-odd-squares (car tree))
;; 		 (sum-odd-squares (cdr tree))))))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
	((predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))


(define (enumerate-tree tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (list tree))
	(else (append (enumerate-tree (car tree))
		      (enumerate-tree (cdr tree))))))

(define (sum-odd-square tree)
  (accumulate +
	      0
	      (map square
		   (filter odd?
			   (enumerate-tree tree)))))
(define (fib n)
  (define (iter n a b)
    (if (= n 0)
	b
	(iter (- n 1) b (+ a b))))
  (iter n 0 1))

(define (even-fib n)
  (accumulate cons
	      '()
	      (filter even?
		      (map fib
			   (enumerate-interval 0 n)))))
(define (list-fib-square n)
  (accumulate cons
	      '()
	      (map square
		   (map fib
			(enumerate-interval 0 n)))))
;;;;2.33
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
	      '()
	      sequence))
(define (append seq1 seq2)
  (accumulate cons
	      seq2
	      seq1))
(define (length sequence)
  (accmulate (lambda (x y) (+ 1 y))
	     0
	     sequence))
;;;;2.34
;; (define (horner-eval x coefficient-sequence)
;;   (accumulate (lambda (thin-coeff higher-terms)
;; 		(* (+ thin-coeff  higher-terms) x))
;; 	      0
;; 	      coefficient-sequence))
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (thin-coeff higher-terms)
		(+ thin-coeff (* higher-terms x)))
	      0
	      coefficient-sequence))
;;;;;2.35
(define (count-leaves tree)
  (accumulate +
	      0
	      (map (lambda (sub-tree)
		     (if (pair? sub-tree)
			 (count-leaves tree)
			 1))
		   tree)))
;;;;2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))
;;;2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
		 (cdr rest))))
  (iter initial sequence))
;;;;2.39
(define (reverse sequence)
  (accumulate (lambda (x y) (append y (list x)))
	      '()
	      sequence))
(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x))
	     '()
	     sequence))
;;;;;
(define (make-pair n)
  (accumulate append
	      '()
	      (map (lambda (i)
		     (map (lambda (j) (list i j))
			  (enumerate-interval 1 (- i 1))))
		   (enumerate-interval 1 n))))
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (prime-sum-pair n)
  (map make-pair-sum
       (filter prime-sum?
	       (flatmap
		(lambda (i)
		  (map (lambda (j) (list i j))
		       (enumerate-interval 1 (- i 1))))
		(enumerate-interval 1 n)))))
;;;;
(define (remove item sequence)
  (filter (lambda (x)
	    (not (= x item)))
	  sequence))
(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
		 (map (lambda (p) (cons x p))
		      (permutations (remove x s))))
	       s)))
;;;;;2.40
(define (unique-pairs n)
  (flatmap (lambda (i)
	     (map (lambda (j) (list i j))
		  (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))
;;;;;2.41
(define (sum li)
  (accumulate + 0 li))
(define (3-pairs n)
  (flatmap (lambda (i)
	     (map (lambda (j) (cons i j))
		  (unique-pairs (- i 1))))
	  (enumerate-interval 1 n)))
(define (triple-sum-equal s triple)
  (filter (lambda (x) (= (sum x) s))
	   triple))
;;;;;2.42
(define empty-board '())
(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))
(define (safe? k positions)
  (define (iter-check row-of-new-queen rest-of-queens i)
    (if (null? rest-of-queens)
	#t
	(let ((row-of-current-queen (car rest-of-queens)))
	  (if (or (= row-of-new-queen row-of-current-queen)
		  (= row-of-new-queen (+ i row-of-current-queen))
		  (= row-of-new-queen (- row-of-current-queen i)))
	      #f
	      (iter-check row-of-new-queen
			  (cdr rest-of-queens)
			  (+ i 1))))))
  (iter-check (car positions) (cdr positions) 1))
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions) (safe? k positions))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))
  (queen-cols board-size))

(for-each (lambda (pos)
	    (begin
	      (display pos)
	      (newline)))
	  (queens 8))
;;;;;;
