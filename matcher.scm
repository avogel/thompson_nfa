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
  (define (eqv-match network start-node end-node)
    (add-edge network
	      start-node
	      end-node
	      (eqv-predicate pattern-constant)
	      #f)
    network)
  eqv-match)

(define (eqv-predicate pattern-constant)
  (lambda (data step-expand?)
    (and (pair? data)
	 (eqv? (car data) pattern-constant))))

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

(define (match:maker network data)
  (let step ((probes '(start))
	     (data data))
    (depth-expand network probes)
    (cond ((memq 'end probes)
	   #t)
	  ((not (pair? data))
	   #f)
	  (else
	   (let probe-loop ((probes-left (unique probes))
			    (new-probes '())
			    )
	     (cond ((pair? probes-left)
		    (let edge-loop ((edges (node-edges (get-node network
								 (car probes-left))))
				    (edge-new-probes new-probes))
		      (cond ((pair? edges)
			     (if ((edge-predicate (car edges)) data #t)
				 (edge-loop (cdr edges)
					    (cons (edge-destination (car edges))
						  edge-new-probes))
				 (edge-loop (cdr edges) edge-new-probes)))
			    (else
			     (probe-loop (cdr probes-left) edge-new-probes)))))
		   (else
		    (step new-probes (cdr data)))))))))

(define (depth-expand network probes)
  (let probe-loop ((probes-left probes)
		   (new-probes '()))
    (cond ((pair? probes-left)
	   (probe-loop (cdr probes-left)
		       (depth-expand-probe network (car probes-left) new-probes)))
	  (else (append! probes (unique new-probes))))))

;;; taken from http://stackoverflow.com/a/17413712
(define (unique lst)
  (let loop ((lst lst) (res '()))
    (if (not (pair? lst))
        (reverse res)
        (let ((c (car lst)))
          (loop (cdr lst) (if (member c res) res (cons c res)))))))

(define (depth-expand-probe network probe the-probes)
  (let edge-loop ((edges (node-edges (get-node network probe)))
		  (expanded-probes the-probes))
    (cond ((pair? edges)
	   (edge-loop
	    (cdr edges)
	    (if ((edge-predicate (car edges)) '() #f)
		(depth-expand-probe
		 network
		 (edge-destination (car edges))
		 (cons (edge-destination (car edges)) expanded-probes)
		 ;(if (edge-leave-probe? (car edges))
		 ;    (cons (edge-destination (car edges)) expanded-probes)
		 ;    (cons (edge-destination (car edges))
			   ;(cdr expanded-probes)))
		)
		expanded-probes)))
	  (else expanded-probes))))

(define (new-network pattern)
  (let ((network (make-network)))
    ((match:->combinators pattern)
     network
     'start
     'end)))

#|
(match:maker
 (new-network `(a))
 `(a))
;Value: #t

(match:maker
 (new-network `(a))
 `())
;Value: #f

(match:maker
 (new-network `(a))
 `(a b))
;Value: #t

(match:maker
 (new-network `(a))
 `(b))
;Value: #f

(match:maker
 (new-network `(a))
 `(b a))
;Value: #f

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












