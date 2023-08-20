#lang racket
(require racket/trace)
; small step rules require you to implement a stack
; Control Env Kontinuations machine
; (e, env, 'mt) -> CEK state -> CEK state -> ....... -> (v, env, 'mt)
; (x, p, k) -> lookup in p for x
; (x, p, k) -> (p(x), k)
; ((lam (x) eb), p, k) -> (<((lam (x) eb), p), k)
; ((ef, ea), p, k) -> (ef, p, <ea, p, k>)
;                                      -> (vf, (ea, p, k)) -> (ea, p, <vf, k>)
;                                                                           -> (va, (vf, k)) -> (vf, va, k)
; (<(lam (x) eb), px>, va, k) -> (eb, p[x -> va], k)


;;;;; CEK MACHINE
; (define (interp-cek exp)
;   (define (eval exp env kont)
;     (match exp
;       [(? number? n) (ret n kont)]
;       [(? boolean? x) (ret x kont)]
;       [(? symbol? y) (ret (hash-ref env y) kont)]
;       [`(lambda (,x) ,eb) (ret `(closure ,exp ,env) kont)]
;       [`(if ,ec ,et ,ef) (eval ec env `(if-k ,et ,ef ,env ,kont))]
;       [`(,ef ,ea) (eval ef env `(ar ,ea ,env ,kont))]))

;   (define (ret val kont)
;     (match kont
;       ['halt val]
;       [`(ar ,ea ,env ,k) (eval ea env `(fn ,val ,k))]
;       [`(fn ,vf ,k) (apply vf val k)]
;       [`(if-k ,et ,ef ,env ,k) (if val (eval et env k) (eval ef env k))]))

;   (define (apply vf va kont)
;     (match vf
;       [`(closure (lambda (,x) ,eb) ,envlam)
;         (eval eb (hash-set envlam x va) kont)]))

;   ; to DEBUG
;   ; (trace apply)
;   ; (trace ret)
;   ; (trace eval)

;   (eval exp (hash) 'halt))


;;;; ONE LET SUPPORT
; (define (interp-cek exp)
;   (define (eval exp env kont)
;     (match exp
;       [(? number? n) (ret n kont)]
;       [(? boolean? x) (ret x kont)]
;       [(? symbol? y) (ret (hash-ref env y) kont)]
;       [`(equal? ,x ,y) (eval (equal? (hash-ref env x) y) env kont)]
;       [`(list ,x ,y) (ret (list (hash-ref env x) (hash-ref env y)) kont)]
;       [`(lambda (,x) ,eb) (ret `(closure ,exp ,env) kont)]
;       [`(if ,ec ,et ,ef) (eval ec env `(if-k ,et ,ef ,env ,kont))]
;       [`(,ef ,ea) (eval ef env `(ar ,ea ,env ,kont))]
;       [`(let (,binding) ,body) (match binding [`(,name ,expr) (eval expr env `(let-k ,name ,body ,env ,kont))])]))

;   (define (ret val kont)
;     (match kont
;       ['halt val]
;       [`(list ,x ,y) (ret (list x y) kont)]
;       [`(ar ,ea ,env ,k) (eval ea env `(fn ,val ,k))]
;       [`(fn ,vf ,k) (apply vf val k)]
;       [`(if-k ,et ,ef ,env ,k) (if val (eval et env k) (eval ef env k))]
;       [`(let-k ,name ,body ,env ,k) (eval body (hash-set env name val) k)]))

;   (define (apply vf va kont)
;     (match vf
;       [`(closure (lambda (,x) ,eb) ,envlam)
;        (eval eb (hash-set envlam x va) kont)]))

;   (eval exp (hash) 'halt))


; ; ; ; NESTED LETS SUPPORT
; (define (interp-cek exp)
;   (define (eval exp env kont)
;     (match exp
;       [(? number? n) (ret n kont)]
;       [(? boolean? x) (ret x kont)]
;       [`(equal? ,x ,y) (eval (equal? (hash-ref env x) y) env kont)]
;       [(? symbol? y) (ret (hash-ref env y) kont)]
;       [`(lambda (,x) ,eb) (ret `(closure ,exp ,env) kont)]
;       [`(if ,ec ,et ,ef) (eval ec env `(if-k ,et ,ef ,env ,kont))]
;       [`(,ef ,ea) (eval ef env `(ar ,ea ,env ,kont))]
;       [`(let ,bindings ,body)
;        (define (eval-bindings bindings env)
;          (if (null? bindings)
;              (eval body env kont)
;              (let ([binding (car bindings)])
;                (match binding
;                  [`(,name . ,expr) (eval (car expr) env `(let-k ,name ,(cdr bindings) ,body ,env ,kont))]))))
;        (eval-bindings bindings env)]))

;   (define (ret val kont)
;     (match kont
;       ['halt val]
;       [`(ar ,ea ,env ,k) (eval ea env `(fn ,val ,k))]
;       [`(fn ,vf ,k) (apply vf val k)]
;       [`(if-k ,et ,ef ,env ,k) (if val (eval et env k) (eval ef env k))]
;       [`(let-k ,name ,remaining-bindings ,body ,env ,k)
;        (eval `(let ,remaining-bindings ,body) (hash-set env name val) k)]))

;   (define (apply vf va kont)
;     (match vf
;       [`(closure (lambda (,x) ,eb) ,envlam)
;        (eval eb (hash-set envlam x va) kont)]))

;   (eval exp (hash) 'halt))

; LET* SUPPORT VERSION
(define (interp-cek exp)
  (define (eval exp env kont)
    (match exp
      [(? number? n) (ret n kont)]
      [(? boolean? x) (ret x kont)]
      [(? symbol? y) (ret (hash-ref env y) kont)]
      [`(equal? ,x ,y) (eval x env `(equal?-k ,y ,env ,kont))]
      [`(and ,ea ,eb) (eval ea env `(and-k ,eb ,env ,kont))]
      [`(or ,ea ,eb) (eval ea env `(or-k ,eb ,env ,kont))]
      [`(zero? ,x) (eval x env `(zero?-k ,x env ,kont))]
      [`(+ ,x ,y) (eval x env `(add-k ,y ,env ,kont))]
      [`(list ,x ,y) (eval x env `(list-k ,y ,env ,kont))]
      [`(lambda (,x) ,eb) (ret `(closure ,exp ,env) kont)]
      [`(if ,ec ,et ,ef) (eval ec env `(if-k ,et ,ef ,env ,kont))]
      [`(,ef ,ea) (eval ef env `(ar ,ea ,env ,kont))]
      [`(let ,bindings ,body)
       (define (eval-bindings bindings env)
         (if (null? bindings)
             (eval body env kont)
             (let ([binding (car bindings)])
               (match binding
                 [`(,name . ,expr) (eval (car expr) env `(let-k ,name ,(cdr bindings) ,body ,env ,kont))]))))
       (eval-bindings bindings env)]
      [`(let () ,body) (eval body env kont)]
      [`(let* (,bind ,more ...) ,body) (eval `(let (,bind) (let* ,more ,body)) env kont)]
      [`(let* () ,body) (eval body env kont)]))
      (define (let*eval bindings env body kont)
        (if (null? bindings)
            (eval body env kont)
            (let ([binding (car bindings)])
              (match binding
                [`(,name ,expr) (eval expr env `(let*-k ,bindings ,body ,env ,kont))]))))

  (define (ret val kont)
    (match kont
      ['halt val]
      [`(ar ,ea ,env ,k) (eval ea env `(fn ,val ,k))]
      [`(fn ,vf ,k) (apply vf val k)]
      [`(if-k ,et ,ef ,env ,k) (if val (eval et env k) (eval ef env k))]
      [`(let-k ,name ,remaining-bindings ,body ,env ,k)
       (eval `(let ,remaining-bindings ,body) (hash-set env name val) k)]
      [`(let*-k ,bindings ,body ,env ,k) (let*eval (cdr bindings) (hash-set env (caar bindings) val) body k)]
      [`(equal?-k ,y ,env ,k) (eval y env `(equal?-result ,val ,k))]
      [`(equal?-result ,y-val ,k) (ret (equal? val y-val) k)]
      [`(and-k ,eb ,env ,k) (if val (eval eb env k) (ret #f k))]
      [`(or-k ,eb ,env ,k) (if val (ret #t k) (eval eb env k))]
      [`(add-k ,y ,env ,k) (eval y env `(add-result ,val ,k))]
      [`(add-result ,y-val ,k) (ret (+ val y-val) k)]
      [`(list-k ,y ,env ,k) (eval y env `(list-result ,val ,k))]
      [`(list-result ,y-val ,k) (ret (list val y-val) k)]))

  (define (apply vf va kont)
    (match vf
      [`(closure (lambda (,x) ,eb) ,envlam)
       (eval eb (hash-set envlam x va) kont)]))

  ; DEBUG TRACING
  (trace eval)
  (trace ret)
  (trace apply)

  (eval exp (hash) 'halt))

; LET ACTUAL VERSION
(define (interpCESK expr)
  (define (eval exp env store kont)
    (match exp
      [(? number? n) (ret n store kont)]
      [(? boolean? b) (ret b store kont)]
      [(? symbol? x) (ret (hash-ref store (hash-ref env x)) store kont)]
      [`(equal? ,x ,y) (eval x env store `(equal?-k ,y ,env ,kont))]
      [`(and ,ea ,eb) (eval ea env store `(and-k ,eb ,env ,kont))]
      [`(or ,ea ,eb) (eval ea env store `(or-k ,eb ,env ,kont))]
      [`(zero? ,x) (eval x env store `(zero?-k ,kont))]
      [`(+ ,x ,y) (eval x env store `(add-k ,y ,env ,kont))]
      [`(list ,x ,y) (eval x env store `(list-k ,y ,env ,kont))]
      [`(lambda (,x) ,eb) (ret `(clos ,exp ,env) store kont)]
      [`(if ,ec ,et ,ef) (eval ec env store `(if-k ,et ,ef ,env ,kont))]
      [`(set! ,x ,e) (eval e env store `(set!-cont ,(hash-ref env x) ,kont))]

      ;  fix let later
      [`(let ([,x0 ,rhs0] [,xs ,rhss] ...) ,body)
        (eval rhs0 env store `(let-k (,x0 ,@xs) () ,rhss ,exp ,env ,kont))
      ]
      [`(let `() ,body) (eval body env store kont)]
      ; [`(let* (,bindings ,more ...) ,body) (let*eval bindings env body kont)]
      ; ;(eval `(let (,bind) (let* ,more ,body)) env store kont)
      ; [`(let* () ,body) (eval body env store kont)]

      [`(,ef ,ea) (eval ef env store `(arg ,ea ,env ,kont))]
    ))

    (define (let*eval bindings env body kont)
      (if (null? bindings)
          (eval body env kont)
          (let ([binding (car bindings)])
            (match binding
              [`(,name ,expr) (eval expr env `(let*-k ,bindings ,body ,env ,kont))]))))

  (define (ret v store kont)
    (match kont
      [`(arg ,ea ,env ,kont+) (eval ea env store `(fn ,v ,kont+))]
      [`(fn ,vf ,kont+) (apply vf v store kont+)]
      [`(if-k ,et ,ef ,env ,k) (if v (eval et env store k) (eval ef env store k))]
      [`(set!-cont ,a ,kont+) (ret v (hash-set store a v) kont+)]
      [`(equal?-k ,y ,env ,kont) (eval y env store `(equal?-result ,v ,kont))]
      [`(equal?-result ,y-val ,kont) (ret (equal? v y-val) store kont)]
      [`(and-k ,eb ,env ,kont) (if v (eval eb env store kont) (ret #f store kont))]
      [`(or-k ,eb ,env ,kont) (if v (ret #t store kont) (eval eb env store kont))]
      [`(zero?-k ,kont) (ret (zero? v) store kont)]
      [`(add-k ,y ,env ,kont) (eval y env store `(add-result ,v ,kont))]
      [`(add-result ,y-val ,kont) (ret (+ v y-val) store kont)]
      [`(list-k ,y ,env ,kont) (eval y env store `(list-result ,v ,kont))]
      [`(list-result ,y-val ,kont) (ret (list v y-val) store kont)]
      [`(let-k ,xs ,vs () ,exp ,env ,kont+) 
        ; (eval exp 
        ;   (foldl (lambda (x env) (hash-set env x a)) env xs)
        ;   (foldl (lambda (v store) (hash-set store a v)) store vs)
        ;   kont)
        ; (define addresses (map (lambda (binding) (gensym)) bindings))
        ; (define env+ (foldl (lambda (env name addr) (hash-set env name addr)) env (map car bindings) addresses))
        ; (define store+ (foldl (lambda (store addr expr) (hash-set store    addr (car (eval expr env store 'halt)))) store (map cons addresses (map cadr bindings))))
        ; (eval body env+ store+ kont)
        (define as (map gensym xs))
        (eval exp 
          (foldl (lambda (x a env) (hash-set env x a)) env xs as)
          (foldl (lambda (a v store) (hash-set store a v)) store as vs)
          kont+)
      ]
      [`(let-k ,xs ,vs (,rhs0 ,rhss ...) ,exp ,env ,kont+) 
        (eval rhs0 env store `(let-k ,xs (,@vs ,v) ,rhss ,exp ,env ,kont+))]
      ['halt `(,v ,store)]
    
    ))



  
  (define (apply vf va store kont)
  (match vf
    [`(clos (lambda (,x) ,eb) ,envf)
     (define addr (gensym))
     (eval eb (hash-set envf x addr) (hash-set store addr va) kont)]
     
  ))

  ;DEBUG
  (trace eval)
  (trace ret)
  (trace apply)

  (eval expr (hash) (hash) 'halt))


; (trace apply)
; (trace ret)
(trace interpCESK)
; (interp-cek `(let ([x 5])
;   (let ([y (let ([z (set! x 6)]) x)])
;     (+ x y))))
; (interpCESK `((lambda (x) ((lambda (y) (set! x y)) 20)) 10))
; ; (interp-cek '(
;   (let ((f (lambda (x y z w) (+ x (+ y (+ z w))))))
;     (f 1 2 3 4)
;   )
; ))
(interpCESK `(if #t 5 4))
(interpCESK `((lambda (x) ((lambda (y) (set! x y)) 20)) 10))

; (interpCESK `(let ([x 5])
;   (set! x (let ([x 6])
;   x
;   ))
; ))
; (interpCESK `(if ((lambda (x) x) #t) 5 4))
; (interp-cek `(let ([x 5]) x))
(interpCESK `(let ([x 5])
    (let ([x 2]
          [y x])
      y)))


; (interp-cek `(let ([x 5] [y 6] [z 7]) 
;   (if (or (equal? x 5) (equal? y 3))
;       1
;       2)
; ))

; (interpCESK `(let* ([x 1]
;          [y (+ x 1)])
;     (list y x)))

; (interp-cek `(let ((x 10) (y 7))
;   (let ((diff (if (equal? x y)
;     (+ x y)
;     (+ y x))))
;   diff))
; )

; (interp-cek `(let fac ([n 10])
;     (if (zero? n)
;         1
;         (* n (fac (sub1 n))))))

; (interp-cek `((lambda (y) (lambda (y) y)) (lambda (y) ((lambda (x) x) y))))
; (define factorial-code
;   `((lambda (f n)
;       (if (= n 0)
;           1
;           (* n (f f (- n 1)))))5))

; (interp-cek factorial-code)


; (define (eval-bindings bindings env results)
;         (if (null? bindings)
;             (let ([binding (car bindings)])
;               (match binding
;                 [`(,name . ,expr) (eval (car expr) env `(let-k ,name ,(cdr bindings) ,body ,results ,env ,kont))]))
;             (let ([new-env (foldl (lambda (binding env) (hash-set env (car binding) (cdr  binding))) env results)])