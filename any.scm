;;;; Preston Thompson and Ari Vogel
;;;; Any
;;;; May 5, 2014

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
	      (lambda (data) #t)))
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

(match:maker 
 (new-network `(?:any a b c))
 `(a b c))
;Value: #t

(match:maker 
 (new-network `(?:any a b c))
 `())
;Value: #t

(match:maker 
 (new-network `(?:any a b c))
 `(a b c a b c))
;Value: #t

(match:maker 
 (new-network `(?:any a b c))
 `(a b))
;Value: #f

|#
