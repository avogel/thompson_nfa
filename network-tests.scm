;;;; Preston Thompson and Ari Vogel
;;;; Network Tests
;;;; May 13 2014


(define net (make-network))
;Value: net
(eqv? (hash-table/get net 'start 'doesnotexist) (empty-node))
;Value: #t
(eqv? (hash-table/get net 'end 'doesnotexist) (empty-node))
;Value: #t

(start-node net)
;Value: (node ())

(end-node net)
;Value: (node ())

(define test-node-1 `(node ((edge (lambda (data step-expand) #t)
				  end)
			    (edge (lambda (data step-expand)
				    (eqv? (car data) 'c))
				  start))))
(pp test-node-1)
;(node
; ((edge (lambda (data step-expand) #t) end)
;  (edge (lambda (data step-expand) (eqv? (car data) 'c)) start)))

(add-node net test-node-1)
;Value: 2

(hash-table/get net 2 'doesnotexist)
;Value 25: (node ((edge (lambda (data step-expand) #t) end) (edge (lambda (data step-expand) (eqv? (car data) (quote c))) start)))


(define empty (empty-node))
(list? empty)
; Value: #t
(eqv? (car empty) 'node)
; Value: #t
(eqv? (cadr empty) '())
; Value: #t


(add-edge net 'start 'end 
	  (lambda (data step-expand)
	    #t))

(pp (start-node net))
; (node ((edge #[compound-procedure 26] end)))

((edge-predicate (car (node-edges (start-node net)))) '() #t)
;Value: #t


(add-edge net 'start 2
	  (lambda (data step-expand)
	    (eqv? (car data) 'a)))

((edge-predicate (car (node-edges (start-node net)))) '(a) #f)
;Value: #t

((edge-predicate (car (node-edges (start-node net)))) '(b) #f)
;Value: #f

(node-edges (start-node net))
;Value 27: ((edge #[compound-procedure 28] 2) (edge #[compound-procedure 26] end))


(node-edges (get-node net 2))
;Value 29: ((edge (lambda (data step-expand) #t) end) (edge (lambda (data step-expand) (eqv? (car data) (quote c))) start))
