;;;; Preston Thompson and Ari Vogel
;;;; Optional
;;;; May 5, 2014

;; `(a (?:optional ((?:choice b c) d)))
;;
;; matches a
;; matched abd
;; matches acd

(define (match:optional? pattern)
  (and (pair? pattern)
       (eq? (car pattern) '?:optional)))

(define (match:optional . match-combinators)
  (define (optional-match data-outer outer-dictionary succeed)
    (let lp ((data data-outer)
	     (match-combinators match-combinators)
	     (dictionary outer-dictionary)
	     (n 0))
      (cond ((pair? match-combinators)
	     (or ((car match-combinators)
		  data
		  dictionary
		  (lambda (d nprime)
		    (lp (cdr data) (cdr match-combinators)
			d (+ n nprime))))
		 (succeed outer-dictionary 0)))
	    (else (succeed dictionary n)))))
  optional-match)

(define (match:optional-combinators pattern) (cdr pattern))

(defhandler match:->combinators
  (lambda (pattern)
    (apply match:optional
	   (map match:->combinators (match:optional-combinators pattern))))
  match:optional?)

#|
((match:->combinators
  `((?:optional a) a))
 `((a a))
 `()
 (lambda (d n)
   (pp `(succeed ,d ,n))
   #f))
(succeed () 1)
;Value: #f

((match:->combinators
  `((?:optional b) a))
 `((b a))
 `()
 (lambda (d n)
   (pp `(succeed ,d ,n))
   #f))
(succeed () 1)
;Value: #f

((match:->combinators
  `((?:optional b) a))
 `((a))
 `()
 (lambda (d n)
   (pp `(succeed ,d ,n))
   #f))
(succeed () 1)
;Value: #f

(define pattern `(,o ,o ,o ,o ,o ,o ,o ,o ,o ,o ,o ,o ,o ,o ,o ,o ,o ,o ,o ,o ,o ,o a a a a a a a a a a a a a a a a a a a a a a))
;Value: pattern

((match:->combinators pattern)
 `((a a a a a a a a a a a a a a a a a a a a a a))
 `()
 (lambda (d n)
   (pp `(succeed ,d ,n))
   #f))
(succeed () 1)
;Value: #f
; took 3 minutes 15 seconds

|#











































































