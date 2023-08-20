#lang racket

(require racket/trace)

; (define (interp exp [env (hash)])
;   (match exp
;     [(? symbol? x) (hash-ref env x (lambda () (error (format "var ~a is undefined" x))))]
;     [`(lambda (,x) ,eb) `(closure ,exp ,env)]
;     [`(,ef ,ea)
;      (define vf (interp ef env))
;      (define va (interp ea env))
;      (match vf
;        [`(closure (lambda (,x) ,eb) ,envlam) (interp eb (hash-set envlam x va))])]))

(define prims `(+ - * /))
(define (prim? op)
  (member op prims))

(define (interp exp [env (hash)])
    (match exp
        [(? number? x) x]
        [(? symbol? x) (hash-ref env x (lambda () (error (format "var ~a is undefined" x))))]
        [`(lambda (,x) ,eb) `(closure ,exp ,env)]
        [`(,op ,x ,y ...) #:when (prim? op)
            (define v (interp x env))
            (define vs (map (lambda (e) (interp e env)) y))
            (apply (eval op (make-base-namespace)) vs)
        ]
        [`(,ef ,ea)
            (define vf (interp ef env))
            (define va (interp ea env))
            (match vf
                [`(closure (lambda (,x) ,eb) ,envlam)
                    (interp eb (hash-set envlam x va))
                ]
            )
        ]
    )
)

; (interp `(+ 1 2))
; (interp `(- 1 2))
; (interp `(* 1 2))
; (interp `(/ 1 2))
; (interp `((lambda (x) (+ x 1)) 2))
; (interp `((lambda (x) (+ x 1)) (+ 1 2)))
; (interp (((lambda (x) (lambda (y) (+ x y))) 1) (+ 1 2)))
; (interp `((lambda (x) (lambda (y) (+ x y))) 1))
; (interp `(((lambda (x) (lambda (y) (+ x y))) 1) (+ 1 z)))

; (interp `(+ 1 2 3 7 8 0 012))
; (interp `(+ 2 3 4 (- 12 3 4)))

; (interp `(((lambda (x) (lambda (y) (+ x y))) 1 (+ 1 2))))
; (interp `((lambda (x) (x x)) (lambda (y) y)))
; (interp `((lambda (z) ((lambda (x) (x x)) (lambda (y) z))) (lambda (w) w)))

; denotational semantics
; operational semantics
; axiomatic semantics
