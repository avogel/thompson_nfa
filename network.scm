;;;; Preston Thompson and Ari Vogel
;;;; Node
;;;; May 13 2014

;;; makes and returns a network
(define (make-network)
  (let ((network (make-eq-hash-table)))
    (hash-table/put! network 'end (empty-node))
    (hash-table/put! network 'start (empty-node))
    network))

;;; adds the node to network and returns the node's key
(define (add-node network node)
  (let ((key (hash-table/count network)))
    (hash-table/put! network key node)
    key))

(define (start-node network)
  (hash-table/get network 'start (empty-node)))

(define (end-node network)
  (hash-table/get network 'end (empty-node)))

(define (new-node network)
  (add-node network (empty-node)))

(define (empty-node)
  '(node ()))

(define (add-edge network start-node-key end-node-key predicate)
  (let ((start-node (hash-table/get network start-node-key (empty-node))))
    (let ((new-node `(node ,(cons `(edge ,predicate ,end-node-key) (node-edges start-node)))))
      (hash-table/put! network start-node-key new-node))))

(define (node-edges node)
  (cadr node))