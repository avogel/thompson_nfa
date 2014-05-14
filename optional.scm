;;;; Preston Thompson and Ari Vogel
;;;; Optional
;;;; May 14 2014

;; `(a (?:optional ((?:choice b c) d)))
;;
;; matches a
;; matched abd
;; matches acd

(define (match:optional? pattern)
  (and (pair? pattern)
       (eq? (car pattern) '?:optional)))

(define (match:optional . match-combinators)
  (define (optional-match network start-node end-node)
    (let ((connected-network ((apply match:list match-combinators)
			     network
			     start-node
			     end-node)))
      (add-edge
       connected-network
       start-node
       end-node
       (lambda (data step-expand) 
	 (not step-expand)))
      connected-network))
  optional-match)

(define (match:optional-combinators pattern) (cdr pattern))

(defhandler match:->combinators
  (lambda (pattern)
    (apply match:optional
	   (map match:->combinators (match:optional-combinators pattern))))
  match:optional?)

#|

(match:maker 
 (new-network `(a (?:optional b) c))
 `(a b c))
;Value: #t

(match:maker 
 (new-network `(a (?:optional b) c))
 `(a c))
;Value: #t

(match:maker 
 (new-network `(a (?:optional b) c))
 `(a d c))
;Value: #f

|#











































































