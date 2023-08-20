#lang racket

;; map phase
(define (map-phase docs)
  (foldl (λ (doc acc)
           (append acc (map (λ (word) (cons (string-trim word) 1)) (string-split doc))))
         '()
         docs))

(define (reduce-phase mapped)
  (let loop ([mapped mapped] [result '()])
    (if (null? mapped)
        result
        (begin
          (let* ([pair (car mapped)]
                 [word (car pair)]
                 [count (cdr pair)]
                 [existing (assoc word result)])
            (pretty-print (list pair word count existing))
            (loop (cdr mapped)
                  (if existing
                      (cons (cons word (+ (cdr existing) count))
                            (remove (lambda (item) (equal? (car item) word)) result))
                      (cons pair result))))))))

(define (word-count docs)
  (reduce-phase (map-phase docs)))
(define docs '("Welcome Everyone" "Hello Everyone"))
(word-count docs)