#lang racket

; cesk* machine rules from the AAM paper
;〈x, ρ, σ, a〉                  〈v, ρ′, σ, a〉 where (v, ρ′) = σ(ρ(x))
;〈(e0 e1), ρ, σ, a〉            〈e0, ρ, σ[b → ar(e1, ρ, a)], b〉
;〈v, ρ, σ, a〉
; if κ = ar(e, ρ′, c)            〈e, ρ′, σ[b → fn(v, ρ, c)], b〉
; if κ = fn((lambda x . e), ρ′, c)    〈e, ρ′[x → b], σ[b → (v, ρ)], c〉


(define (cesk*-interp prog)
  (define (eval exp env store kontptr)
    ;(displayln (~a "\n>>>eval : " (list '--exp: exp '--env: env '--store: store '--kontptr: kontptr)))
    (match exp
      [(? symbol? x)
       (ret (hash-ref store (hash-ref env x)) store kontptr)]

      [(or (? number?) (? boolean?))
       (ret exp store kontptr)]

      [`(lambda (,x) ,eb)
       (ret `(clo (lambda (,x) ,eb) ,env) store kontptr)]

      [`(,ef ,ea)
       (define newadr (gensym 'adr))
       (eval ef env (hash-set store newadr `(ar ,ea ,env ,kontptr)) newadr)]
      
      [else (raise `(Error occurred in eval function!...State: ,exp ,env ,store ,kontptr))]))

  (define (ret val store kontptr)
    ;(displayln (~a ">>>ret  : " (list '--val: val '--store: store '--kontptr: kontptr)))
    (match (hash-ref store kontptr)
      ['mt (list val store)]

      [`(ar ,ea ,this_env ,this_kontptr)
       (define newadr (gensym 'adr))
       (eval ea this_env (hash-set store newadr `(fn ,val ,this_kontptr)) newadr)]

      [`(fn ,vf ,this_kontptr)
       (apply vf val store this_kontptr)]
      
      [else (raise `(Error occurred in ret function!...State: ,val ,store ,kontptr))]))

  (define (apply vf va store kontptr)
    ;(displayln (~a ">>>apply: " (list '--vf: vf '--va: va '--store: store '--kontptr: kontptr)))
    (match vf
      [`(clo (lambda (,x) ,eb) ,env)
       (define newadr (gensym 'adr))
       (eval eb (hash-set env x newadr) (hash-set store newadr va) kontptr )]

      [else (raise `(Error occurred in apply function!...State: ,vf ,va ,store ,kontptr))]))

  (define newadr (gensym 'adr))
  (eval prog (hash) (hash newadr 'mt) newadr))

; (cesk*-interp '((lambda (x) x) (lambda (y) y)))
; (cesk*-interp '((lambda (x) x) 3))
; (cesk*-interp '((lambda (x) (x (lambda (z) z))) (lambda (y) y)))
; (displayln (cesk*-interp '((lambda (x) (lambda (x) x)) 7)))
(cesk*-interp `((lambda (x) (lambda (y) (lambda (x) x))) (lambda (y) y)))
; (displayln (cesk*-interp '((lambda (x) (lambda (y) (lambda (z) (x y z)))) 2 3 4)))
