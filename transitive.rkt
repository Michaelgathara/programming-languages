#lang racket

; (set `(0 1) `(0 2) `(1 3))
(define g (set `(0 1) `(0 2) `(1 3) `(2 3) `(3 4)))

(define (outgoing-edges-of g v)
  (set->list (filter (λ (e) (= v (car e))) g))
)

(define (outgoing-edges-of-fold g v)
    (foldl (λ (edge acc) (match-define `(,a ,b) edge) (if (equal? v a) (set-add acc edge) acc)) (set) (set->list g))
)

; (outgoing-edges-of g 0)
(outgoing-edges-of-fold g 0)

