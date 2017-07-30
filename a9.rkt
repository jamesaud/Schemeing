#lang racket
(require "parenthec.rkt")

;Part 2
(define lex
  (λ (LCE acc)
    (match LCE
      [`(lambda (,x) ,b) (list 'lambda (lex b (cons x acc)))]
      [`,y #:when (symbol? y) (cons 'var (cons (count-var y acc) '()))]
      [`,y #:when (number? y) (list 'const y)]
      [`(if ,ec ,et ,ef) (list 'if (lex ec acc)
                                (lex et acc)
                                 (lex ef acc))]
     [`(* ,nexp1 ,nexp2) `(mult ,(lex nexp1 acc) ,(lex nexp2 acc))]
      [`(zero? ,nexp) `(zero ,(lex nexp acc))]
      [`(sub1 ,y) (list `sub1 (lex y acc))]
      [`(let ([,x ,e]) ,b) (list 'let (lex e acc) (lex b acc))] 
      [`(,rator ,rand) `(app ,(lex rator acc) ,(lex rand acc))]
      [`(throw ,kexp ,vexp) `(throw ,(lex kexp acc) ,(lex vexp acc))]
      [`(let/cc ,k ,body) (lex body (cons k acc))]
        )))

(define count-var
   (λ (x ls)
     (cond
       [(empty? ls) 0]
       [(eqv? x (car ls)) 0]
       [else (add1 (count-var x (cdr ls)))])))

(define testMult
  (λ ()
    (lex `(* 2 4) `())))

(define testLet/cc
  (λ ()
    (lex '(let/cc k 5) '())))

                     
(define testLet
  (λ ()
    (lex `((lambda (x) (let ([x 4]) x)) 1)(λ (x) x))))

(define test-simple
  (λ ()
    (lex `((lambda (x) (if (zero? x) 0 (sub1 x))) 20) '())))
;---------------------------------------------------------------------------------------------------

;------------------------REPRESENTATION INDEPNEDENT FUNCTIONS-----------------------------

(define empty-k
  (λ ()
  `(empty-k)))

;(define empty-env
;  (lambda ()
;    (lambda (y^ k^)
;      (error 'value-of "unbound identifier"))))

(define empty-env
  (lambda ()
    `(empty-env)))

(define make-closure
  (λ (body env)
      (λ (a k)
         (value-of-cps body (extend-env env a) k))))

;(define make-closure
 ; (λ (body env)
  ;  `(make-closure ,body ,env)))


;(define extend-env
;  (lambda (env^ y^)
;    (lambda (y k) (if (zero? y)
;                      (apply-k y^ k) (apply-env env^ y k)))))

(define extend-env
  (lambda (env^ y^)
    `(extend-env ,env^ ,y^)))

(define apply-closure
  (λ (closure arg k)
    (closure arg k)))

;(define apply-closure
 ; (λ (closure arg k)
  ;  `(apply-closure ,closure ,arg ,k)))

(define apply-env
  (λ (env y k)
    (match env
      [`(extend-env ,env^ ,y^) (if (zero? y)
                                   (apply-k y^ k) (apply-env env^ (sub1 y) k))]
      [`(empty-env) (error 'value-of "unbound identifier")])))

;-----------------------APPLY MATCH---------------------------------
  
(define *-inner-k
  (λ (v^ k)
    `(*-inner-k ,v^ ,k)))

(define *-outer-k
  (λ (x2 env k)
    `(*-outer-k ,x2 ,env ,k)))

(define sub-inner-k
  (λ (k)
  `(sub-inner-k ,k)))

(define zero-inner-k
  (λ (k)
  `(zero-inner-k ,k)))

(define if-inner-k
  (λ (conseq alt env k)
    `(if-inner-k ,conseq ,alt ,env ,k)))

(define letcc-inner-k
  (λ (env)
    `(letcc-inner-k ,env)))

(define let-inner-k
  (λ (body env k)
   `(let-inner-k ,body ,env ,k)))

(define app-inner-k
  (λ (v^ k)
    `(app-inner-k ,v^ ,k)))

(define app-outer-k
  (λ (rand env k)
    `(app-outer-k ,rand ,env ,k)))

(define throw-inner-k
  (λ (v)
      `(throw-inner-k ,v)))

(define throw-outer-k
  (λ (v env)
    `(throw-outer-k ,v ,env))) 

(define apply-k
  (λ (v k)
   ; (begin
   ;   (display v)
   ;   (newline)
   ;   (display k)
   ;   (newline))
    (match k
      [`(app-inner-k ,v^ ,k) (apply-closure v^ v k) ]
      [`(app-outer-k ,rand ,env ,k) (value-of-cps rand env
                                                   (app-inner-k v k))]
      [`(*-inner-k ,v^ ,k) (apply-k (* v v^) k)]
      [`(*-outer-k ,x2 ,env ,k) (begin
                                   (display "outer-k good "))
                                 (value-of-cps x2 env
                                              (*-inner-k v k))]
      [`(sub-inner-k ,k) (apply-k (sub1 v) k)]
      [`(zero-inner-k ,k) (apply-k (zero? v) k)]
      [`(if-inner-k ,conseq ,alt ,env ,k) (if v
                                              (value-of-cps conseq env k)
                                              (value-of-cps alt env k))]
      [`(let-inner-k ,body ,env ,k) (value-of-cps body (extend-env env v) k)]
      [`(throw-inner-k ,v^) (apply-k v v^)]
      [`(throw-outer-k ,v^ ,env) (value-of-cps v^ env (throw-inner-k v))]
      [`(empty-k) v]
      ))) 


;(let/cc k body)
;=>
;(let ([k current-cont])
;  body)
;(val-of body (extend-env env k) k)
;----------------CPS INTERPRETER----------------------
(define value-of-cps
  (lambda (expr^ env k)
    (union-case expr^ expr
      [(const expr^) (apply-k expr^ k)]
      [(mult x1 x2) (value-of-cps x1 env
                                     (*-outer-k x2 env k))]
      [(sub1 x)  (value-of-cps x env
                                 (sub-inner-k k))]
      [(zero x) (value-of-cps x env
                                (zero-inner-k k))]
      [(if test conseq alt) (value-of-cps test env
                                              (if-inner-k conseq alt env k))]
      
      [(letcc body) (value-of-cps body (extend-env env k) k)]
      [(throw k-exp v-exp) (value-of-cps k-exp env (throw-outer-k v-exp env))]
      
      [(let e body)
       (value-of-cps e env
                     (let-inner-k body env k))]
      [(var expr^) (apply-env env expr^ k)]
      [(lambda body) (apply-k (make-closure body env) k)]
      [(app rator rand) (value-of-cps rator env
                                         (app-outer-k rand env k))])))


;-------------------------A9------------------------------
(define main 
  (lambda ()
    (value-of-cps 
     (expr_let 
      (expr_lambda
       (expr_lambda 
        (expr_if
         (expr_zero (expr_var 0))
         (expr_const 1)
         (expr_mult (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_sub1 (expr_var 0)))))))
      (expr_mult
       (expr_letcc
        (expr_app
         (expr_app (expr_var 1) (expr_var 1))
         (expr_throw (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_const 4)))))
       (expr_const 5)))
     (empty-env)
     (empty-k))))

(define-union expr
  (const cexp)
  (var n)
  (if test conseq alt)
  (mult nexp1 nexp2)
  (sub1 nexp)
  (zero nexp)
  (letcc body)
  (throw kexp vexp)
  (let exp body)              
  (lambda body)
  (app rator rand))

(define test
  (λ ()
    (value-of-cps (expr_const 5) (empty-env) (empty-k))))
(main)