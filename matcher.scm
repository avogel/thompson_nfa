;;;; Matcher based on match combinators, CPH/GJS style.
;;;     Idea is in Hewitt's PhD thesis (1969).

(declare (usual-integrations))

;;; There are match procedures that can be applied to data items.  A
;;; match procedure either accepts or rejects the data it is applied
;;; to.  Match procedures can be combined to apply to compound data
;;; items.

;;; A match procedure takes a list containing a data item, a
;;; dictionary, and a success continuation.  The dictionary
;;; accumulates the assignments of match variables to values found in
;;; the data.  The success continuation takes two arguments: the new
;;; dictionary, and the number of items absorbed from the list by the
;;; match.  If a match procedure fails it returns #f.

;;; Primitive match procedures:

(define (match:eqv pattern-constant)
  (define (eqv-match data dictionary succeed)
    (and (pair? data)
	 (eqv? (car data) pattern-constant)
	 (succeed dictionary 1)))
  eqv-match)

(define (match:list . match-combinators)
  (define (list-match data dictionary succeed)
    (and (pair? data)
	 (let lp ((lst (car data))
		  (matchers match-combinators)
		  (dictionary dictionary))
	   (cond ((pair? matchers)
		  ((car matchers)
		   lst
		   dictionary
		   (lambda (new-dictionary n)
		     (if (> n (length lst))
			 (error "Matcher ate too much."
				n))
		     (lp (list-tail lst n)
			 (cdr matchers)
			 new-dictionary))))
		 ((pair? lst) #f)
		 ((null? lst)
		  (succeed dictionary 1))
		 (else #f)))))
  list-match)

;;; Syntax of matching is determined here.

(define (match:list? pattern)
  (and (list? pattern)
       (or (null? pattern)
	   (not (memq (car pattern) '(?:choice ?:optional ?:any ?:plus))))))

(define match:->combinators
  (make-generic-operator 1 'eqv match:eqv))

(defhandler match:->combinators
  (lambda (pattern)
    (apply match:list (map match:->combinators pattern)))
  match:list?)

(define (matcher pattern)
  (let ((match-combinator (match:->combinators pattern)))
    (lambda (datum)
      (match-combinator (list datum)
			'()
			(lambda (dictionary n)
			  (and (= n 1)
			       dictionary))))))