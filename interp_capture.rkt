#lang racket


; capture avoiding substituion in racket

(require racket/trace)
(define (freevars e)
    (match e
        [(? symbol? x) (set x)]
        [(? number? y) (set)]
        [`(lambda (,x) ,eb) (set-remove (freevars eb) x)]
        [`(,ef ,ea) (set-union (freevars ef) (freevars ea))]
    )
)

(define (cas e0 x e1) 
    ; (displayln (~a "e0: " e0 "\nx: " x "\ne1: " e1 "\n"))
    (match e0    
        [(? symbol? y) #:when (eq? y x) e1]
        [(? symbol? y) y]
        [`(lambda (,y) ,eb) #:when (eq? y x) e0]
        [`(lambda (,y) ,eb) #:when (not (set-member? (freevars e1) y)) `(lambda (,y) ,(cas eb x e1))]
        [`(,ef ,ea) `(,(cas ef x e1) ,(cas ea x e1))]
        [(? number? y) y]
    )
)


; (cas '((lambda (y) x) z) 'x 'y)
; (cas '((lambda (y) x) z) 'x 'y)
; (trace cas)
; (cas  'x 5)

; ; (cas `(lambda (x) x) 'x 5)
; ; (cas `(lambda (x) x) `(`(lambda (y) y) 'y) 5)
; ; (cas `(`(lambda (x) x) `(lambda (y) y)) 'y 5)
; (cas (cas `(lambda (x) x) 'x 0) 'x '(lambda (x) x))

(define (atom? exp)
    (match exp
        [`(lambda (,_) ,_) #t]
        [(? symbol? y) #t]
        [(? number? y) #t]
        [_ #f]
    )
)

(define (value? exp)
    (match exp
        [`(lambda (,_) ,_) #t]
        [(? number? y) #t]
        [_ #f]
    )
)

(define (reduce exp)
    (match exp
        [(? symbol? y) y]
        [`(lambda (,x) ,eb) exp]
        [(? number? y) y]
        [`((lambda (,y) ,eb) ,args) #:when (value? args) (cas eb y args)]
        [`((lambda (,y) ,eb) ,args) `((lambda (,y) ,eb) ,(reduce args))]
        [`(,ef ,ea) `(,(reduce ef) ,ea)]
    )
)
; (trace reduce)
; (cas `(((lambda (x) ((lambda (y) x) y)) y)))
; (reduce `((lambda (x) x)(lambda (y) y)))
; (reduce (reduce `((lambda (x) x)(lambda (y) y))))

(define (all-beta-reductions exp) 
    (let loop ([exp exp] [acc (set)] )
        ;(displayln (~a "acc: " acc "\nexp: " exp "\n"))
        (let ([nRedex (reduce exp)])
            (cond 
                [(equal? nRedex exp) acc]
                [else (loop nRedex (set-add acc nRedex))]
            )
        )
    )
)

(define (gatherCBV exp)
    (let loop ([trace (list exp)])
        (if (value? (car trace))
            (reverse trace)
            (loop (cons (reduce (car trace)) trace))
        )
    )
)

; (define (reduce exp)
;     (match exp
;         [(? symbol? y) y]
;         [`(lambda (,x) ,eb) exp]
;         [(? number? y) y]
;         [`((lambda (,y) ,eb) ,args) #:when (value? args) (cas eb y args)]
;         [`((lambda (,y) ,eb) ,args) `((lambda (,y) ,eb) ,(reduce args))]
;         [`(,ef ,ea) `(,(reduce ef) ,ea)]
;     )
; )

(define (reduce-cbn exp)
    ; (displayln (~a "Exp: " exp "\n"))
    (match exp
        [(? symbol? y) y]
        ; [(? number? y) y]
        [`((lambda (,y) ,eb) ,args) (reduce-cbn (cas eb y args))]
        [`((lambda (,y) ,eb) ,args) #:when (value? args) exp]
        [`(lambda (,y) ,eb) `(lambda (,y) ,(reduce-cbn eb))]
        [`((lambda (,x) ,eb) ((lambda (,y) ,ex) ,args)) (cas eb x args)]
        [`(,ef ,ea) `(,(reduce-cbn ef) ,(reduce-cbn ea))]
        ; [`((,ef ,ea) (,e3 ,e4)) exp]
    )
    
)

;cbn
; ((lambda (x) (lambda (y) x)) ((lambda (z) z) w))
; (lambda (y) ((lambda (z) z) w))
; (lambda (y) w)



(define (reduce-cbn-v2 exp)
    (match exp
        [(? symbol? y) y]
        [`(lambda (,x) ,eb) 
            (if (atom? eb)
                exp
                `(lambda (,x) ,(reduce-cbn-v2 eb))
            )
        ]
        [`((lambda (,y) ,eb) ,args) 
            (cas eb y args)]
        [`(,ef ,ea) `(,(reduce-cbn-v2 ef) ,ea)]
    )
)

(define (gatherCBN exp) 
    (let loop ([exp exp] )
        (let ([nRedex (reduce-cbn-v2 exp)])
            (displayln (~a "exp: " exp "\nnRedex: "nRedex "\n---"))
            (cond 
                [(equal? nRedex exp) exp]
                [else (loop nRedex)]
            )
        )
    )
)

; (define (gatherCBN exp)
;     (let loop ([trace (list exp)])
;         (displayln (~a "Car of Trace: " (car trace) "\n"))
;         (cond 
;             [(value? (car trace)) (reverse trace)]
;             [(and ((equal? (car trace) exp) (value? (car trace)))) (reverse trace)]
;             [else (loop (cons (reduce-cbn (car trace)) trace))]
;         )
;         ; (if (value? (car trace))
;         ;     (reverse trace)
;         ;     (loop (cons (reduce-cbn (car trace)) trace))
;         ; )
;     )
; )




; (require racket/trace)
(trace gatherCBN)
(trace gatherCBV)
(trace cas)
(trace reduce)
(trace reduce-cbn)
(trace reduce-cbn-v2)
(trace all-beta-reductions)
; (pretty-print (gatherCBN `((lambda (a) (a (lambda (b) (a b)))) 5)))
(displayln (~a "\n\n"))
(pretty-print (gatherCBN `((lambda (a) ((lambda (b) (a b)) a)) 5)))


; (λ (a) (a (λ (b) (a b)))) vs (λ (a) ((λ (b) (a b)) a))

(λ (a) (a (λ (b) (a b))))
CBN: (λ (a) (a (λ (b) (a b))))
-> (λ (a) (a (λ (b) (a b)))) ; Under lambda, CBN stops here

APP: ((λ (a) (a (λ (b) (a b)))) (lambda (x) x))
->𝛽  (lambda (x) x) (lambda (b) ((lambda (x) x) b))
->𝛽  (lambda (x) x) (lambda (b) (b))

(λ (a) ((λ (b) (a b)) a))
CBN: (λ (a) ((λ (b) (a b)) a))
-> (λ (a) ((λ (b) (a b)) a)) ; Under lambda, CBN stops here

APP: ((lambda (a) ((lambda (b) (a b)) a)) (lambda (x) x))
->𝛽  ((lambda (b) ((lambda (x) x) b)) (lambda (x) x))
->𝛽  (lambda (b) ((lambda (x) x) (lambda (x) x)))
->𝛽 (lamdba (b) (lambda (x) x))






; (pretty-print (gatherCBN `((lambda (x) (lambda (y) (x y))) ((lambda (a) (a a)) (lambda (w) w)))))
; (pretty-print (gatherCBN `((lambda (y) (((lambda (a) (a a)) (lambda (w) w)) y)))))

; I'm not really getting the match form in gather, why not just loop with a reduced value at the front of the trace when you have not yet reached a value? What does this case do: [`((lambda (,y) ,eb) ,args) (loop (cons args trace))] ?
; In reduce-cbn: you are not quite doing CBN evaluation due to the last case. What happens when you have ((e1 e2) (e3 e4)) where e1 is a lambda?

; come up with a set of all possibilities, which are right -> traces that can be CBV, CBN, etc

; (trace all-beta-reductions)
; (all-beta-reductions `((lambda (x) x)(lambda (y) y)))
; (pretty-print (reduce-cbn `((lambda (x) x) (((lambda (a) (lambda (b) (a b))) ((lambda (y) y) w)) (lambda (z) z)))))
; (pretty-print (all-beta-reductions-cbn `((lambda (x) x) (((lambda (a) (lambda (b) (a b))) (lambda (y) y)) (lambda (z) z)))))
; (pretty-print (gatherCBN `(((lambda (a) (lambda (b) (a b))) (lambda (y) y)) (lambda (z) z))))
; (pretty-print (all-beta-reductions-cbn `((lambda (x) (lambda (y) x)) ((lambda (z) z) w))))
; (pretty-print (gatherCBN `((lambda (x) (x x)) (lambda (y) (y y)))))
; (cas `(lambda (b) (a b)) 'a `(lambda (y) y))
; (beta-all-reductions `((lambda (x) x) (lambda (y) y) (lambda (z) z)))
; (beta-all-reductions `((lambda (x) x) (((lambda (a) (lambda (b) (a b))) (lambda (y) y)) (lambda (z) z))))
; (all-beta-reductions `((lambda (x) x) (((lambda (a) (lambda (b) (a b))) (lambda (y) y)) (lambda (z) z))))
; (reduce `(lambda (y) y))


;cbn
; ((lambda (x) (lambda (y) x)) ((lambda (z) z) w))
; (lambda (y) ((lambda (z) z) w))
; (lambda (y) w)
