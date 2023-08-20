#lang racket

(require racket/trace)

(define (interp-cek exp)
  (define (eval exp env kont)
    (match exp
    ;   [(? number? n) (ret n kont)]
    ;   [(? boolean? x) (ret x kont)]
      [(or (? number?) (? boolean?))
        (ret exp kont)]
      [(? symbol? y) (ret (hash-ref env y) kont)]
      [`(lambda (,x) ,eb) (ret `(closure ,exp ,env) kont)]
      [`(if ,ec ,et ,ef) (eval ec env `(if-k ,et ,ef ,env ,kont))]
      [`(,ef ,ea) (eval ef env `(ar ,ea ,env ,kont))]))

  (define (ret val kont)
    (match kont
      ['halt val]
      [`(ar ,ea ,env ,k) (eval ea env `(fn ,val ,k))]
    ;   [`(fn ,vf ,k) #:when (and (number? vf) (equal? k 'halt)) val]
      [`(fn ,vf ,k) (apply vf val k)]
      [`(if-k ,et ,ef ,env ,k) (if val (eval et env k) (eval ef env k))]))

  (define (apply vf va kont)
    (match vf
      [`(closure (lambda (,x) ,eb) ,envlam)
        (eval eb (hash-set envlam x va) kont)]))

  ; to DEBUG
  (trace apply)
  (trace ret)
  (trace eval)

  (eval exp (hash) 'halt))

; (interp-cek `((lambda (x) (x (lambda (c) c))) 5))
; (interp-cek `#t)
; (interp-cek '((lambda (x) (lambda (x) x)) 7))
(interp-cek `((lambda (x) (lambda (y) (lambda (x) x))) (lambda (y) y)))