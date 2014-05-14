;;;; Preston Thompson and Ari Vogel
;;;; plus
;;;; May 5, 2014

(define (match:plus? pattern)
  (and (pair? pattern)
       (eq? (car pattern) '?:plus)))

#|
;;; test cases for match:plus?

(match:plus? `(?:choice a b))
;Value: #f

(match:plus? `(?:plus a (?:choice b c)))
;Value: #t
|#

(define (match:plus . match-combinators)
  (define (plus-match network start-node end-node)
    (let ((intermediate (new-node network)))
      ((apply match:list match-combinators) ((apply match:any match-combinators) network intermediate end-node) start-node intermediate)))
  plus-match)
      

(define (match:pattern-list pattern) (cdr pattern))

(defhandler match:->combinators
  (lambda (pattern)
    (apply match:plus
	   (map match:->combinators
		(match:pattern-list pattern))))
  match:plus?)


(define plus-abc (new-network `(?:plus a b c)))

(match:maker 
 (new-network `(?:plus a b c))
 `(a b c))
;Value: #t

(match:maker 
 (new-network `(?:plus a b c))
 `())
;Value: #f

(match:maker 
 (new-network `((?:plus a b c) d))
 `(a b c))
;Value: #f


(match:maker 
 (new-network `((?:plus a b c) d))
 `(a b c d))
;Value: #t

(match:maker 
 (new-network `((?:plus a b c) d))
 `(d))
;Value: #f

(match:maker 
 (new-network `((?:plus a b c) d))
 `(a b c a b c d))
;Value: #t

(match:maker 
 (new-network `((?:plus a b c) d))
 `(a b c a b c a b c d))
;Value: #t

(match:maker 
 (new-network `((?:plus a b c) d))
 `(a b c d a b c d))
;Value: #t


(match:maker 
 (new-network `((?:plus a b c) d))
 `(a b c s a b c d))
;Value: #f













