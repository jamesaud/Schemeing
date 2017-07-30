#lang racket

;Need to write:
;apply-closure-fn and closure-fn

;(def a-c
;(λ (p a)
;   (p a)))


;CLOSURES

;at lambda line : `(λ(f) (f y))
;gives us context for the variables by carrying the environment

;13.
(define lex
  (λ (LCE acc)
    (match LCE
      [`(lambda (,x) ,b) (list `lambda (lex b (cons x acc)))]
      [`,y #:when (symbol? y) (cons 'var (cons (count-var y acc) '()))]
      [`,y #:when (number? y) (list `const y)]
      [`(if ,ec ,et ,ef) (list `if (lex ec acc)
                                (lex et acc)
                                 (lex ef acc))]
      [`(* ,y ,x) (list `* (lex y acc) (lex x acc))]
      [`(zero? ,y) (list `zero? (lex y acc))]
      [`(sub1 ,y) (list `sub1 (lex y acc))]
      [`(let ([,x ,e]) ,b) (list `let (lex e acc) (lex b acc))] 
      [`(,rator ,rand) (list (lex rator acc) (lex rand acc))]
        )))

(define count-var
   (λ (x ls)
     (cond
       [(empty? ls) 0]
       [(eqv? x (car ls)) 0]
       [else (add1 (count-var x (cdr ls)))])))

;PART 2
(define value-of-fn
  (lambda (exp env)
    (match exp
      [`(lambda (,x) ,b) (closure-fn x b)]

      [`,y #:when (number? y) y]
      [`,y #:when (symbol? y) (env y) ]
      [`,y #:when (boolean? y) y]

      [`(zero? ,y) (zero? (value-of-fn y env))]
      [`(* ,y ,x) (* (value-of-fn y env) (value-of-fn x env))]
      [`(sub1 ,y) (sub1 (value-of-fn y env))]
      [`(if ,ec ,et ,ef) (if (value-of-fn ec env)
                             (value-of-fn et env)
                             (value-of-fn ef env))]
      [`(let ([,x ,e]) ,b)  (value-of-fn b (extend-env-fn (value-of-fn e env) env x))]
      [`(,rator ,rand) (apply-closure-fn rator rand env)]
      )))

(define extend-env-fn
  (lambda (val env x)
    (λ (y)
     (if (eqv? y x) val (apply-env-fn y env)))))

(define apply-env-fn
  (λ (y env)
    (env y)))

(define empty-env-fn
  (λ ()
    (lambda (y)
    (error 'evaluate "~s something not found" ))))

;(define apply-closure-fn
(define apply-closure-fn
  (λ (rator rand env)
  ((value-of-fn rator env) (value-of-fn rand env))))

;(define closure-fn
(define closure-fn
  (λ (x body env)  
    (lambda (val) (value-of-fn body (extend-env-fn val env x)))))


;PART 3
(define value-of-dynamic
   (lambda (exp env)
    (match exp
      [`(lambda (,x) ,b) `(lambda (,x) ,b)]
      [`,y #:when (number? y) y]
      [`,y #:when (symbol? y) (env y) ]
      [`,y #:when (boolean? y) y]
      [`(quote ,v) v]
      [`(cons ,x ,y) (cons (value-of-dynamic x env) (value-of-dynamic y env))]
      [`(car ,ls)  (car (value-of-dynamic ls env))]
      [`(cdr ,ls)  (cdr (value-of-dynamic ls env))]
      [`(zero? ,y) (zero? (value-of-dynamic y env))]
      [`(* ,y ,x) (* (value-of-dynamic y env) (value-of-dynamic x env))]
      [`(sub1 ,y) (sub1 (value-of-dynamic y env))]
      [`(if ,ec ,et ,ef) (if (value-of-dynamic ec env)
                             (value-of-dynamic et env)
                             (value-of-dynamic ef env))] 
      [`(let ([,x ,e]) ,b)  (value-of-dynamic b (extend-env (value-of-dynamic e env) env x))]
      [`(,rator ,rand) (match-let ((`(lambda (,x) ,b)
                                    (value-of-dynamic rator env))
                                   (a (value-of-dynamic rand env)))
                         (value-of-dynamic b (extend-env a env x)))]
      )))


(define extend-env
  (lambda (val env x)
    (λ (y)
     (if (eqv? y x) val (env y)))))

(define empty-env
  (λ ()
    (lambda (y)
    (error 'evaluate "~s something not found" ))))

