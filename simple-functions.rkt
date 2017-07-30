#lang racket
;;JAMES AUDRETSCH

(print-as-expression #f)
;;(require "a1-student-tests.rkt")

;;(test-file #:file-name "assignment1.rkt")

;;1. countdown
;; number -> LoN
(define countdown
  (lambda (x)
    (cond
      [(zero? x) '(0)]
      [else (cons x (countdown (- x 1)))])))

;;2. insertR 
;; symbol1 symbol2 list -> list
(define insertR
  (lambda (s1 s2 ls)
    (cond
      [(empty? ls) '()]
      [(eqv? s1 (car ls)) (cons (car ls) (cons s2 (insertR s1 s2 (cdr ls))))]
      [else (cons (car ls)(insertR s1 s2 (cdr ls)))])))

;;3. remv-1st
;; symbol list -> list
(define remv-1st
  (lambda (s ls)
    (cond
      [(empty? ls) '()]
      [(eqv? s (car ls)) (cdr ls)]
      [else (cons (car ls) (remv-1st s (cdr ls)))])))

;;4. list-index-ofv?
;; element list -> number
(define list-index-ofv?
  (lambda (e ls)
    (cond
      [(eqv? e (car ls)) 0]
      [else (add1 (list-index-ofv? e (cdr ls)))])))

;;5. filter
;; predicate(-> #t | #f) list -> list
(define filter
  (lambda (pr ls)
    (cond
      [(empty? ls) '()]
      [(pr (car ls)) (cons (car ls) (filter pr (cdr ls)))]
      [else (filter pr (cdr ls))])))

;;6. zip
;; ls ls -> ls
(define zip
  (lambda (ls1 ls2)
    (cond
      [(or (empty? ls1) (empty? ls2)) '()]
      [else (cons (cons (car ls1) (car ls2)) (zip (cdr ls1) (cdr ls2)))]
      )))

;;7. map
;; procedure list -> list
(define map
  (lambda (pro ls)
    (cond
      [(empty? ls) '()]
      [else (cons (pro (car ls)) (map pro (cdr ls)))])))

;; 8. append
;; list list -> list
(define append
  (lambda (ls1 ls2)
    (cond
      [(empty? ls1) ls2]
      [else (cons (car ls1) (append (cdr ls1) ls2))])))

;;9. reverse
;; ls -> ls
(define reverse
  (lambda (ls)
    (cond
      [(empty? ls) '()]
      [else (append (reverse (cdr ls))  (list (car ls) ))])))

;;10. fact
;;nat -> nat
(define fact
  (lambda (x)
    (cond
      [(zero? x) 1]
      [else (* x (fact (sub1 x)))])))

;;11. memv
;; el ls ->
(define memv
  (lambda (el ls)
    (cond
      [(empty? ls) #f]
      [(eqv? (car ls) el) ls]
      [else (memv el (cdr ls))])))

;;12. fib
;;num -> num
(define fib
  (lambda (x)
    (cond
      [(zero? x) 0]
      [(eqv? 1 x) 1]
      [else (+ (fib (sub1 x)) (fib (sub1 (sub1 x))))]
      )))

;;13. ((w x) y (z))    is expressed with '.' as:     '((w . (x . ())) y (z . ())))
;;(equal? '((w x) y (z)) '((w . (x . ())) y (z . ())))

;;14. binary->natural
;; ls -> number
(define binary->natural
  (lambda (ls)
    (cond
      [(empty? ls) 0]
      [(zero? (car ls)) (* 2 (binary->natural (cdr ls)))]
      [(eqv? 1 (car ls)) (+ 1 (* 2 (binary->natural (cdr ls))))]
      )))

;;15. minus
;; num num -> num
(define minus
  (lambda (x y)
    (cond
      [(zero? y) x]
      [else (minus (sub1 x) (sub1 y))]
       )))

;;16. div
;; num num -> num
(define div
  (lambda (x y)
    (cond
      [(zero? x) 0]
      [else (+ 1 (div (- x y) y))])))


;;17. append-map
;; p ls -> ls
(define append-map
  (lambda (p ls)
    (cond
      [(empty? ls) '()]
      [else (append (p (car ls)) (append-map p (cdr ls)))])))

;;18. set-difference
;; set set -> ls

(define set-difference
  (lambda (s1 s2)
    (cond
      [(empty? s1) '()]
      [(member (car s1) s2) (set-difference (cdr s1) s2)]
      [else (cons (car s1) (set-difference (cdr s1) s2))]))) 

