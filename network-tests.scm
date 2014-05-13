;;;; Preston Thompson and Ari Vogel
;;;; Node
;;;; May 13 2014


#|
;;; makes and returns a network
(define (make-network)
  (let ((network (make-eq-hash-table)))
    (hash-table/put! network 'end (empty-node))
    (hash-table/put! network 'start (empty-node))
    network))
|#


(define net (make-network))
;Value: net
(eqv? (hash-table/get net 'start 'doesnotexist) (empty-node))
;Value: #t
(eqv? (hash-table/get net 'end 'doesnotexist) (empty-node))
;Value: #t

#|
;;; adds the node to network and returns the node's key
(define (add-node network node)
  (let ((key (hash-table/count network)))
    (hash-table/put! network key node)
    key))
|#

(define test-node-1 `(node ((edge #t start))))

(pp test-node-1)
;(node ((edge #t start)))

(add-node net test-node-1)
;Value: 2

(hash-table/get net 2 'doesnotexist)
;Value 16: (node ((edge #t start)))

#|
(define (new-node network)
  (add-node network (empty-node)))

(define (empty-node)
  '(node ()))
|#

(define empty (empty-node))
(list? empty)
; Value: #t
(eqv? (car empty) 'node)
; Value: #t
(eqv? (cadr empty) '())
; Value: #t


(define (add-edge network start-node end-node predicate)
  (let ((start-node (hash-table/get network start-node (empty-node))))
    `(node ,(cons `(edge ,predicate ,end-node) (node-edges start-node)))))

(define (node-edges node)
  (cadr node))

(define test-node-2 `(node ((edge #t start) (edge #t 2))))
;Value: test-node-2

(node-edges test-node-2)
;Value 19: ((edge #t start) (edge #t 2))