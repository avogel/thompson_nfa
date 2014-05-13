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
  (define (any-match data dictionary succeed)
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

((match:->combinators
  `(?:any a b (? x) c))
 `(z)
 `()
 (lambda (d n) `(succeed ,d)))
;Value: (succeed ((x z)))

((match:->combinators
  `((? y) (?:any a b (? x ,string?) (? y ,symbol?) c)))
 `((z z))
 `()
 (lambda (d n) `(succeed ,d)))
;Value: (succeed ((y z)))

((match:->combinators
  `(?:any b (? x ,symbol?)))
 `(b)
 `()
 (lambda (d n)
   (pp `(succeed ,d ,n))
   #f))
; (succeed ())
; (succeed ((x b)))
;Value: #f
|#
