;;;; Preston Thompson and Ari Vogel
;;;; Choice
;;;; May 14 2014

(define (match:choice? pattern)
  (and (pair? pattern)
       (eq? (car pattern) '?:choice)))

#|
;;; test cases for match:choice?

(match:choice? `(? x))
;Value: #f

(match:choice? `(?:choice a (? x)))
;Value: #t
|#

(define (match:choice . match-combinators)
  (define (choice-match network start-node end-node)
    (let lp ((matchers match-combinators))
      (cond ((pair? matchers)
	     (begin
	       ((car matchers) network start-node end-node)
	       (lp (cdr matchers))))
	    (else network))))
  choice-match)

(define (match:pattern-list pattern) (cdr pattern))

(defhandler match:->combinators
  (lambda (pattern)
    (apply match:choice
	   (map match:->combinators
		(match:pattern-list pattern))))
  match:choice?)

#|
;;;test cases for choice

(match:maker 
 (new-network `(?:choice a b c))
 `())
;Value: #f

(match:maker 
 (new-network `(?:choice a b c))
 `(a))
;Value: #t

(match:maker 
 (new-network `(?:choice a b c))
 `(b))
;Value: #t

(match:maker 
 (new-network `(?:choice a b c))
 `(c))
;Value: #t

(match:maker 
 (new-network `(?:choice (a d) b c))
 `(a))
;Value: #f

(match:maker 
 (new-network `(?:choice (a d) b c))
 `(a d))
;Value: #t
|#
