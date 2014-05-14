;;;; Preston Thompson and Ari Vogel
;;;; Tests for matcher.scm
;;;; May 14 2014

;;; test cases for eqv-predicate
((eqv-predicate 'a) '(a b c))
;Value: #t

((eqv-predicate 'a) '(b a c))
;Value: #f

((eqv-predicate 'a) '())
;Value: #f

;;; test cases for match:eqv
(let ((network (make-network)))
  ((match:eqv 'a) network 'start 'end)
  (hash-table/get network 'start 'doesntexist))
;Value: (node ((edge #[compound-procedure XX] end)))

(let ((network (make-network)))
  (let ((new-node (new-node network)))
    ((match:eqv 1) network 'start new-node)
    (hash-table/get network 'start 'doesntexist)))
;Value: (node ((edge #[compound-procedure XX] 2)))

















