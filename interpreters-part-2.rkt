#lang racket
;-------- call-by-value interpreter -------------------
(define val-of-cbv
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(zero? ,n) (zero? (val-of-cbv n env))]
      [`(sub1 ,n) (sub1 (val-of-cbv n env))]
      [`(* ,n1 ,n2) (* (val-of-cbv n1 env) (val-of-cbv n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbv test env)
                                  (val-of-cbv conseq env)
                                  (val-of-cbv alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbv e1 env) (val-of-cbv e2 env))]
      [`(random ,n) (random (val-of-cbv n env))]
      [`(set! ,x ,rhs) (let ([vrhs (val-of-cbv rhs env)])
                         (set-box! (env x) vrhs))]
      [`,x #:when (symbol? x) (unbox (apply-env x env))]
      [`(lambda (,x) ,body) (make-closure-cbv env x body)]
      [`(,rator ,rand) (apply-closure-cbv rator rand env)])))

(define extend-env
  (lambda (val env x)
    (λ (y)
     (if (eqv? y x) val (apply-env y env)))))

(define apply-env
  (λ (y env)
    (env y)))

(define empty-env
  (λ ()
    (lambda (y)
    (error 'evaluate "~s something not found" ))))

;(define apply-closure
(define apply-closure-cbv
  (λ (rator rand env)
    (cond
      [(symbol? rand) ((val-of-cbv rator env) (box (unbox (env rand))))]
      [else ((val-of-cbv rator env) (box (val-of-cbv rand env)))])))

;(define closure
(define make-closure-cbv
  (λ (env x body)  
    (lambda (val) (val-of-cbv body (extend-env val env x)))))


;;-------- call-by-reference interpreter -------------------
(define val-of-cbr
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(zero? ,n) (zero? (val-of-cbr n env))]
      [`(sub1 ,n) (sub1 (val-of-cbr n env))]
      [`(* ,n1 ,n2) (* (val-of-cbr n1 env) (val-of-cbr n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbr test env)
                                  (val-of-cbr conseq env)
                                  (val-of-cbr alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbr e1 env) (val-of-cbr e2 env))]
      [`(random ,n) (random (val-of-cbr n env))]
      
      [`(set! ,x ,rhs) (symbol? x)
                         (set-box! (env x) (val-of-cbr rhs env))]
      [`,x #:when (symbol? x) (unbox (env x))]
      [`(lambda (,x) ,body) (make-closure-cbr env x body)]
      [`(,rator ,rand) (apply-closure-cbr rator rand env)])))

;(define apply-closure
(define apply-closure-cbr
  (λ (rator rand env)
    (cond
      [(symbol? rand) ((val-of-cbr rator env) (env rand))]
      [else  ((val-of-cbr rator env) (box (val-of-cbr rand env)))])))

;(define closure
(define make-closure-cbr
  (λ (env x body)  
    (lambda (val) (val-of-cbr body (extend-env val env x)))))




;;-------- call-by-name interpreter -------------------
;I GUESS I am confused about the random-sieve tests... the error says it expected a "number?" So I inserted an "and" statement testing for "zero?" and "number?"
(define val-of-cbname
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(zero? ,n) (and (number? (val-of-cbname n env))(zero? (val-of-cbname n env)))]
      [`(sub1 ,n) (sub1 (val-of-cbname n env))]
      [`(* ,n1 ,n2) (* (val-of-cbname n1 env) (val-of-cbname n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbname test env)
                                  (val-of-cbname conseq env)
                                  (val-of-cbname alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbname e1 env) (val-of-cbname e2 env))]
      [`(random ,n) (random (val-of-cbname n env))]
      
      [`,x #:when (symbol? x) (unbox (env x))]
      [`(lambda (,x) ,body) (make-closure-cbname env x body)]
      [`(,rator ,rand) (apply-closure-cbname rator rand env)])))

;(define apply-closure
(define apply-closure-cbname
  (λ (rator rand env)
    (cond
      [(symbol? rand)  ((val-of-cbname rator env) (apply-env env rand))]
      [else  ((val-of-cbname rator env) (box (lambda () (val-of-cbname rand env))))])))

;(define closure
(define make-closure-cbname
  (λ (env x body)  
    (lambda (val) (val-of-cbname body (extend-env val env x)))))



;;-------- call-by-need interpreter -------------------
;I GUESS I am confused about the random-sieve tests... the error says it expected a "number?" So I inserted an "and" statement testing for "zero?" and "number?"
(define val-of-cbneed
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(zero? ,n) (and (number? (val-of-cbneed n env))(zero? (val-of-cbneed n env)))]
      [`(sub1 ,n) (sub1 (val-of-cbneed n env))]
      [`(* ,n1 ,n2) (* (val-of-cbneed n1 env) (val-of-cbneed n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbneed test env)
                                  (val-of-cbneed conseq env)
                                  (val-of-cbneed alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbneed e1 env) (val-of-cbneed e2 env))]
      [`(random ,n) (random (val-of-cbneed n env))]
      
      [`,x #:when (symbol? x) (unbox (env x))]
      [`(lambda (,x) ,body) (make-closure-cbneed env x body)]
      [`(,rator ,rand) (apply-closure-cbneed rator rand env)])))

;(define apply-closure
(define apply-closure-cbneed
  (λ (rator rand env)
    (cond
      [(symbol? rand)  ((val-of-cbneed rator env) (apply-env env rand))]
      [else  ((val-of-cbneed rator env) (box (lambda () (val-of-cbneed rand env))))])))

;(define closure
(define make-closure-cbneed
  (λ (env x body)  
    (lambda (val) (val-of-cbneed body (extend-env val env x)))))

(define random-sieve
    '((lambda (n)
        (if (zero? n)
            (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) #t #f) #f) #f) #f) #f) #f)
            (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f #t))))))))
      (random 2)))