(define a 1)
(define b 2)
(list a b)
(list 'a 'b)
(define (memq item x)
  (cond ((null? x) false)
	((eq? item (car x)) x)
	(else (memq item (cdr x)))))
(memq 'apple '(pear banana prune))
(memq 'apple '(x (apple sauce) y apple pear))
;;;;;2.53
(list 'a 'b 'c)
(list (list 'george))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue socks)))
(memq 'red '(red shoes blue socks))
;;;;2.54
(define (equal? list1 list2)
  (cond ((and (null? list1) (null? list2)) #t)
	((or (null? list1) (null? list2)) #f)
	((eq? (car list1) (car list2))
	 (equal? (cdr list1) (cdr list2)))
	(else #f)))
;;;;;