;;;; Preston Thompson and Ari Vogel
;;;; Matcher
;;;; May 14 2014

(declare (usual-integrations))

;;; These are match procedures that create a Thompson NFA network.
;;; When data is applied to the network, the algorithm will track the
;;; progression through the network as it steps through the data, and
;;; will be considered a success if it reaches the end state of the
;;; network without running out of data.

;;; A match procedure takes a network, a start node, and an end node.
;;; The match procedure will add the correct nodes and edges to the
;;; network to connect the start node and end node according to the
;;; particular type of the match procedure. The match procedure then
;;; returns the updated network.

;;; returns a match combinator that will create a network
;;; from start-node to end-node where the data must match
;;; the pattern constant
(define (match:eqv pattern-constant)
  (define (eqv-match network start-node end-node)
    (add-edge network
	      start-node
	      end-node
	      (eqv-predicate pattern-constant))
    network)
  eqv-match)

;;; returns an edge predicate where the first element of data
;;; must match pattern-constant for the predicate to be true
(define (eqv-predicate pattern-constant)
  (lambda (data step-expand)
    (and (pair? data)
	 (eqv? (car data) pattern-constant))))

;;; makes the match:->combinators generic operator that will
;;; be implemented for various pattern types
(define match:->combinators
  (make-generic-operator 1 'eqv match:eqv))

;;; The items below run the step loop that will apply a network
;;; created by match:->combinators and determine whether it
;;; matches data

;;; applies the data to the network
;;; returns true if it matches, false otherwise
(define (match:maker network data)
  (let step ((probes '(start))
	     (data data))
    (depth-expand network probes)
    ;(pp (unique probes))
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

;;; expands all the probes as far as possible down empty edges
(define (depth-expand network probes)
  (let probe-loop ((probes-left probes)
		   (new-probes '()))
    (cond ((pair? probes-left)
	   (probe-loop (cdr probes-left)
		       (depth-expand-probe network (car probes-left) new-probes)))
	  (else (append! probes (unique new-probes))))))

;;; expands a probe as far as possible down empty edges and returns a list of the
;;; resulting probes
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
		 (cons (edge-destination (car edges)) expanded-probes))
		expanded-probes)))
	  (else expanded-probes))))

;;; returns a list without any repeating elements
;;; taken from http://stackoverflow.com/a/17413712
(define (unique lst)
  (let loop ((lst lst) (res '()))
    (if (not (pair? lst))
        (reverse res)
        (let ((c (car lst)))
          (loop (cdr lst) (if (member c res) res (cons c res)))))))



#|
;;; match:eqv examples

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

|#












