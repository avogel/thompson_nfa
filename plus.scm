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
  (define (plus-match data dictionary succeed)
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
  plus-match)

(define (match:pattern-list pattern) (cdr pattern))

(defhandler match:->combinators
  (lambda (pattern)
    (apply match:plus
	   (map match:->combinators
		(match:pattern-list pattern))))
  match:plus?)

#|
;;;test cases for plus match:->combinators

((match:->combinators
  `(?:plus a b (? x) c))
 `(z)
 `()
 (lambda (d n) `(succeed ,d)))
;Value: (succeed ((x z)))

((match:->combinators
  `((? y) (?:plus a b (? x ,string?) (? y ,symbol?) c)))
 `((z z))
 `()
 (lambda (d n) `(succeed ,d)))
;Value: (succeed ((y z)))

((match:->combinators
  `(?:plus b (? x ,symbol?)))
 `(b)
 `()
 (lambda (d n)
   (pp `(succeed ,d ,n))
   #f))
; (succeed ())
; (succeed ((x b)))
;Value: #f
|#
