;;;; Preston Thompson and Ari Vogel
;;;; Choice
;;;; May 5, 2014

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
  (define (choice-match data dictionary succeed)
    (and (pair? data)
	 (let lp ((data data)
		  (matchers match-combinators)
		  (dictionary dictionary))
	   (cond ((pair? matchers)
		  (or ((car matchers)
		       data
		       dictionary
		       succeed)
		      (lp data
			  (cdr matchers)
			  dictionary)))
		 (else #f)))))
  choice-match)

(define (match:pattern-list pattern) (cdr pattern))

(defhandler match:->combinators
  (lambda (pattern)
    (apply match:choice
	   (map match:->combinators
		(match:pattern-list pattern))))
  match:choice?)

#|
;;;test cases for choice match:->combinators

((match:->combinators
  `(?:choice a b (? x) c))
 `(z)
 `()
 (lambda (d n) `(succeed ,d)))
;Value: (succeed ((x z)))

((match:->combinators
  `((? y) (?:choice a b (? x ,string?) (? y ,symbol?) c)))
 `((z z))
 `()
 (lambda (d n) `(succeed ,d)))
;Value: (succeed ((y z)))

((match:->combinators
  `(?:choice b (? x ,symbol?)))
 `(b)
 `()
 (lambda (d n)
   (pp `(succeed ,d ,n))
   #f))
; (succeed ())
; (succeed ((x b)))
;Value: #f
|#
