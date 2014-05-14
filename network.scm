;;;; Preston Thompson and Ari Vogel
;;;; Network
;;;; May 14 2014


;;; node procedures

;;; nodes are represented as a tagged list containing a list of out edges.  e.g.:
;;; '(node (edge-1 edge-2 edge-3)) 

;;; adds the node to network and returns the node's key
(define (add-node network node)
  (let ((key (hash-table/count network)))
    (hash-table/put! network key node)
    key))

(define (get-node network node-key)
  (hash-table/lookup network
		     node-key
		     (lambda (datum) datum)
		     (lambda () (error 'node-does-not-exist-with-key node-key))))

(define (start-node network)
  (hash-table/get network 'start (empty-node)))

(define (end-node network)
  (hash-table/get network 'end (empty-node)))

(define (new-node network)
  (add-node network (empty-node)))

(define (empty-node)
  '(node ()))

;;; edge procedures

;;; edges are represented as a tagged list with the start and end nodes of the edge, and a predicate.
;;; the predicate is a function which takes data and a flag indicating if the resulting node should be
;;; 'expanded' e.g.: '(edge 'start 2 (lambda (data step-expand) (eqv? (car data) '(a))))

(define (add-edge network start-node-key end-node-key predicate)
  (let ((start-node (hash-table/get network start-node-key (empty-node))))
    (let ((new-node `(node ,(cons `(edge ,predicate ,end-node-key)
				  (node-edges start-node)))))
      (hash-table/put! network start-node-key new-node))))

(define (node-edges node)
  (cadr node))

(define (edge-predicate edge)
  (cadr edge))

(define (edge-destination edge)
  (caddr edge))


;;; networks procedures

;;; makes and returns a network
(define (make-network)
  (let ((network (make-eq-hash-table)))
    (hash-table/put! network 'end (empty-node))
    (hash-table/put! network 'start (empty-node))
    network))

;;; creates a network with a pattern
(define (new-network pattern)
  (let ((network (make-network)))
    ((match:->combinators pattern)
     network
     'start
     'end)))













