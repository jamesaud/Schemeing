#lang racket
(require "monads.rkt")

(define assv-maybe
  (λ (v ls)
    (cond
      [(empty? ls) (fail)]
      [(member v (car ls)) (return-maybe (car ls))]
      [else (assv-maybe v (cdr ls))])))

(assv-maybe 1  '((1 2) (3 4)))

; (define reciprocals
;   (λ (l)
;     (cond
;       [(null? l) (return-writer '())]
;       [(zero? (car l))
;        ( bind-writer
;          (tell-writer "Saw a 0")
;          (λ (_)
;            ( reciprocals (cdr l))))]
;       [else
;        ( bind-writer
;          (reciprocals (cdr l))
;          (λ (d)
;            ( return-writer
;              (cons (/ 1 (car l)) d ))))])))

(define reciprocals
  (λ (l)
    (cond
      [( null? l) ( return-writer '())]
      [( zero? (car l))
       (do bind-writer
         ( tell-writer "Saw a 0")
         ( reciprocals (cdr l)))]
      [else
       (do bind-writer
         (d <- ( reciprocals (cdr l)))
         ( return-writer
           (cons (/ 1 (car l)) d )))])))


(define partition-writer
  (λ (proc ls)
    (cond
      [(empty? ls) (return-writer '())]
      [(proc (car ls))(bind-writer
                       (tell-writer (car ls))
                       (λ (_)
                         (partition-writer proc (cdr ls))))]                                   
      [else (bind-writer
             (partition-writer proc (cdr ls))
             (λ (d)(return-writer (cons (car ls) d ))))])))

(partition-writer even? '(4 3 1 0 2))


;---------STATE MONAD----------
                                  
(define power
  (lambda (x n)
    (cond
      [(zero? n) 1]
      [(= n 1) x]
      [(odd? n) (* x (power x (sub1 n)))]
      [(even? n) (let ((nhalf (/ n 2)))
                   (let ((y (power x nhalf)))
                     (* y y)))])))

(define power-writer
  (lambda (x n)
    (cond
      [(zero? n) (return-writer 1)]
      [(= n 1)  (return-writer x)]
      [(odd? n) (do bind-writer
                  (* x (power-writer x (sub1 n))))]
      [(even? n)
       (do bind-writer (let ((nhalf (/ n 2)))
                          (let ((y (power-writer x nhalf)))
                            (* (car y) (car y)))))])))
;(power-writer 2 2)

(define replace-with-count
  (λ (val ls)
  (cond
    [(empty? ls) '()]
    [(eqv? val (car ls)) (cons 'count (replace-with-count val (cdr ls)))]
    [else (cons (car ls) (replace-with-count val (cdr ls)))])))


(power-writer 3 1)

(replace-with-count 'd '(a b c d e f g ))




(define traverse
    (lambda (return bind f)
      (letrec
        ((trav
           (lambda (tree)
             (cond
               [(pair? tree)
                (do bind
                  (a <- (trav (car tree)))
                  (d <- (trav (cdr tree)))
                  (return (cons a d)))]
               [else (f tree)]))))
        trav)))


(define reciprocal
  (λ (val)
    (cond
      [(eqv? 0 val) (fail)]
      [else (return-maybe `(,val / 2))])))

(reciprocal 1)
(reciprocal 0)


(define halve
  (λ (val)
    (cond
      [(eqv? (modulo val 2) 0) (cons (return-writer val) (cons '() '()))]
      [else (cons val (list (return-writer val)))]
      )))

(halve 10)
(halve 9)

(define traverse-halve
    (traverse return-writer bind-writer halve))

(traverse-halve '((1 . 2) . (3 . (4 . 5))))

;(define state/sum
;  (λ (num)
;    (λ (num2))
;    ...
