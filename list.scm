;;; Preston Thompson and Ari Vogel
;;; List
;;; May 14 2014

;;; returns true if the pattern is a list pattern
(define (match:list? pattern)
  (and (list? pattern)
       (or (null? pattern)
	   (not (memq (car pattern) '(?:choice ?:optional ?:any ?:plus))))))

;;; returns a match combinator link a concatenation of match combinators
;;; from the start node to the end node in a network
(define (match:list . match-combinators)
  (define (list-match network outer-start-node outer-end-node)
    (let lp ((matchers match-combinators)
	     (start-node outer-start-node))
      (cond ((pair? (cdr matchers))
	     (let ((intermediate (new-node network)))
	       ((car matchers) network start-node intermediate)
	       (lp (cdr matchers)
		   intermediate)))
	    ((null? (cdr matchers))
	     ((car matchers) network start-node outer-end-node))
	    ((null? matchers)
	     network))))
  list-match)

;;; adds list to the generic operator
(defhandler match:->combinators
  (lambda (pattern)
    (apply match:list (map match:->combinators pattern)))
  match:list?)

#|
;;; match:list examples

(match:maker
 (new-network `(a b c))
 `(a b c))
;Value: #t

(match:maker
 (new-network `(a b c))
 `())
;Value: #f

(match:maker
 (new-network `(a b c))
 `(a b))
;Value: #f

(match:maker
 (new-network `(a b))
 `(a b c))
;Value: #t

(match:maker
 (new-network `(a b c))
 `(a b d))
;Value: #f

|#
