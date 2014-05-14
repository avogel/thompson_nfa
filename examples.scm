;;;; Preston Thompson and Ari Vogel
;;;; Examples of intricate patterns
;;;; May 14 2014

#|

(match:maker
 (new-network `((a b (?:choice (?:plus c d) (e f)))))
 `(a b e f))
;Value: #t

(match:maker
 (new-network `((a b (?:choice (?:plus c d) (e f)))))
 `(a b))
;Value: #f

(match:maker
 (new-network `((a b (?:choice (?:plus c d) (e f)))))
 `(a e f))
;Value: #f

(match:maker
 (new-network `((a b (?:choice (?:plus c d) (e f)))))
 `(a b c d))
;Value: #t

(match:maker
 (new-network `((a b (?:choice (?:plus c d) (e f)))))
 `(a b c d c d c d))
;Value: #t

(match:maker
 (new-network `(a (?:any (?:choice (c d) (e f))) b))
 `(a e f b))
;Value: #t

(match:maker
 (new-network `(a (?:any (?:choice (c d) (e f))) b))
 `(a e f c d b))
;Value: #t

(match:maker
 (new-network `(a (?:any (?:choice (c d) (e f))) b))
 `(a c b))
;Value: #f

(match:maker
 (new-network `(a (?:any (?:choice (c d) (e f))) b))
 `(a e f c d e f c d b))
;Value: #t

(match:maker
 (new-network `(a (?:any (?:choice (c d) (e f))) b))
 `(a e f c d a b))
;Value: #f

(match:maker
 (new-network `(a (?:any (?:choice (c d) (e f))) (?:optional g) b))
 `(a e f b))
;Value: #t

(match:maker
 (new-network `(a (?:any (?:choice (c d) (e f))) (?:optional g) b))
 `(a e f g b))
;Value: #t

(match:maker
 (new-network `(a (?:any (?:choice (c d) (e f))) (?:optional g) b))
 `(a e f g g b))
;Value: #f

(match:maker
 (new-network `(a (?:any (?:choice (c d) (e f))) (?:optional g) b))
 `(a b))
;Value: #t

(match:maker
 (new-network `(a (?:any (?:choice (c d) (e f))) (?:optional g) b))
 `(a g b))
;Value: #t
|#
