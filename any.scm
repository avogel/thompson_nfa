;;;; Preston Thompson and Ari Vogel
;;;; Any
;;;; May 14 2014

(define (match:any? pattern)
  (and (pair? pattern)
       (eq? (car pattern) '?:any)))

#|
;;; test cases for match:any?

(match:any? `(? x))
;Value: #f

(match:any? `(?:any a (? x)))
;Value: #t
|#

(define (match:any . match-combinators)
  (define (any-match network outer-start-node end-node)
    (let lp ((matchers match-combinators)
	     (start-node outer-start-node))
      (cond ((pair? (cdr matchers))
	     (let ((intermediate (new-node network)))
	       ((car matchers) network start-node intermediate)
	       (lp (cdr matchers)
		   intermediate)))
	    ((null? (cdr matchers))
	     ((car matchers) network start-node outer-start-node))
	    ((null? matchers)
	     network)))
    (add-edge network
	      outer-start-node
	      end-node
	      (lambda (data step-expand) 
		(not step-expand)))
    network)
  any-match)

(define (match:pattern-list pattern) (cdr pattern))

(defhandler match:->combinators
  (lambda (pattern)
    (apply match:any
	   (map match:->combinators
		(match:pattern-list pattern))))
  match:any?)

#|
;;;test cases for any match:->combinators

(define any-abc (new-network `(?:any a b c)))

(start-node any-abc)
;Value 24: (node ((edge #[compound-procedure 25] end) (edge #[compound-procedure 26] 2)))


(define start-edges (node-edges (start-node any-abc)))
((edge-predicate (cadr start-edges)) '(a b c))
;Value: #t
((edge-predicate (cadr start-edges)) '(a))
;Value: #t
((edge-predicate (cadr start-edges)) '(b))
;Value: #f
((edge-predicate (cadr start-edges)) '(a c))
;Value: #t

(define second-node (get-node any-abc (edge-destination (cadr abc-edges))))
(define second-node-edges (node-edges second-node))
((edge-predicate (car second-node-edges)) '(b))
;Value: #t
((edge-predicate (car second-node-edges)) '(c))
;Value: #f
((edge-predicate (car second-node-edges)) '(b c))
;Value: #t
((edge-predicate (car second-node-edges)) '(a))
;Value: #f

(define third-node (get-node any-abc (edge-destination (car second-node-edges))))
(define third-node-edges (node-edges third-node))
((edge-predicate (car third-node-edges)) '(a))
;Value: #f
((edge-predicate (car third-node-edges)) '(c a))
;Value: #t

(edge-destination (car third-node-edges))
;Value: start

(define start (start-node any-abc))
(define first-edge-out-of-start (car (node-edges start)))
((edge-predicate first-edge-out-of-start) '() #t)

(match:maker 
 (new-network `(a (?:any b c) d))
 `(a d))
;Value: #t

(match:maker 
 (new-network `(a (?:any b c) d))
 `(a b d))
;Value: #f

(match:maker 
 (new-network `(a (?:any b c) d))
 `(a b c d))
;Value: #t

(match:maker
 (new-network `(a (?:any b c) d))
 `(a b c b c b c d))
;Value: #t

(match:maker
 (new-network `(a (?:any b c) d))
 `(a b c b c b c b d))
;Value: #f
|#

