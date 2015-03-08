;;;;;Huffman
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
	'()
	(let ((next-branch
	       (choose-branch (car bits) current-branch)))
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch)
		    (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else (error "bad bit --CHOOSE-BRANCH" bit))))
;;;;;
(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(else (cons (car set)
		    (adjoin-set x (cdr set))))))
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair) ;symbol
			       (cadr pair));frequency
		    (make-leaf-set (cdr pairs))))))

;;;;2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree
		   (make-leaf 'B 2)
		   (make-code-tree (make-leaf 'D 1)
				   (make-leaf 'C 1)))))
(define msg '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode msg sample-tree)
;;;;2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))
(define (encode-symbol symbol tree)
  (cond ((leaf? tree)
	 '())
	((symbol-in-tree? symbol (left-branch tree))
	 (cons 0
	       (encode-symbol symbol (left-branch tree))))
	((symbol-in-tree? symbol (right-branch tree))
	 (cons 1
	       (encode-symbol symbol (right-branch tree))))
	(else
	 (error "This symbol not in the tree: " symbol))))
(define (symbol-in-tree? symbol tree)
  (define (iter symbol symbol-list)
    (cond ((null? symbol-list)
	   #f)
	  ((eq? (car symbol-list) symbol)
	   #t)
	  (else
	   (iter symbol (cdr symbol-list)))))
  (iter symbol (symbols tree)))
;;;;;2.69
;(define pairs (list (list 'A 4) (list 'B 2) (list 'C 1) (list 'D 1)))
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (successive-merge leaf-set)
  (let ((len (length leaf-set)))
    (cond ((= len 0)
	   '())
	  ((= len 1)
	   (car leaf-set))
	  (else
	   (successive-merge
	    (adjoin-set
	     (make-code-tree
	      (car leaf-set) (cadr leaf-set))
	     (cddr leaf-set)))))))
;;;;2.70
(define eg-pairs (list (list 'A 2) (list 'NA 16)
  (list 'BOOM 1) (list 'SHA 3) (list 'GET 2) (list 'YIP 9)
  (list 'JOB 2) (list 'WAH 1)))
(define eg-tree (generate-huffman-tree eg-pairs))
(encode '(GET A JOB) eg-tree)