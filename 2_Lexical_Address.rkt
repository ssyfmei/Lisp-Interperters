#lang racket
(define union
  (lambda(lst1 lst2)
    (cond
      [(null? lst1) lst2]
      [(memq (car lst1) lst2) (union (cdr lst1) lst2)]
      [else (union (cdr lst1)
                   (cons (car lst1) lst2))])))

(define extend
  (lambda(v pred)
    (lambda (a)
      (if (eqv? a v)
          #t
          (pred a)))))
(define walk-symbol
  (lambda(a ls)
    (let ([b (assv a ls)])
      (if b
          (walk-symbol (cdr b) ls)
          a))))

(define lambda->lumbda
  (lambda (exp)
    (match exp
      [(? symbol? x) x]
      [`(,function ,input)
       (list (lambda->lumbda function) (lambda->lumbda input))]
      [`(,name ,paralst ,body)
       (list 'lumbda 
             paralst
             (lambda->lumbda body))])))

(define var-occurs?
  (lambda(var exp)
    (match exp
      [(? symbol? x) (eqv? var x)]
      [`(,lam ,vars ,body)
       (var-occurs? var body)]
      [`(,exp1 ,exp2)
       (or (var-occurs? var exp1)
           (var-occurs? var exp2))])))

(define vars
  (lambda(exp)
    (match exp
      [(? symbol? x) (list x)]
      [`(,function ,input)
       (append (vars function) (vars input))]
      [`(,name ,paralst ,body)
       (vars body)])))

(define unique-vars
  (lambda(exp)
    (match exp
      [(? symbol? x) (list x)]
      [`(,function ,input)
       (union (unique-vars function) (unique-vars input))]
      [`(,name ,paralst ,body)
       (unique-vars body)])))

(define var-occurs-free?
  (lambda(var exp)
    (match exp
      [(? symbol? x) (eqv? var x)]
      [`(,function ,input)
       (or (var-occurs-free? var function)
           (var-occurs-free? var input))]
      [`(,lam ,paralst ,body)
       (and (not (eqv? (car paralst) var))
            (var-occurs-free? var body))])))

(define var-occurs-bound?
  (lambda(var exp)
    (match exp
      [(? symbol? x) #f]
      [`(,function ,input)
       (or (var-occurs-bound? var function)
           (var-occurs-bound? var input))]
      [`(,lam ,paralst ,body)
       (or (var-occurs-bound? var body)
           (and (eqv? (car paralst) var)
                (var-occurs-free? var body)))])))

(define select
  (lambda(pred lst)
    (cond 
      ((null? lst) '())
      ((pred (car lst)) 
       (cons (car lst)(select pred (cdr lst))))
      (else (select pred (cdr lst))))))
(define unique-free-vars
  (lambda (exp)
    (select 
     (lambda(var) (var-occurs-free? var exp))
     (unique-vars exp))))

(define unique-bound-vars
  (lambda (exp)
    (select 
     (lambda(var) (var-occurs-bound? var exp))
     (unique-vars exp))))

(define lex
  (lambda (exp env)
    (match exp
      [(? symbol? x) (if (member x env)
                         (list 'var (index-of env x))
                         (list 'free-var x))]
      [`(lambda (,arg) ,body)
       (list 'lambda (lex body (cons arg env)))]
      [`(,rator ,rand)
       (list (lex rator env) (lex rand env))])))

(define walk-symbol-update
  (lambda(var var-list)
    (if (number? var)
        var
        (let* [(tar-box (cdr (assv var var-list))) 
               (content (unbox tar-box))]
          (let [(ans (walk-symbol-update content var-list))]
            (set-box! tar-box ans)
            ans)))))

(define var-occurs-both?
  (lambda (var exp)
     (match exp
      [(? symbol? id) (values (eqv? var id) #f)]
      [`(,function ,input)
       (let-values (((f1 f2) (var-occurs-both? var function))
                    ((i1 i2) (var-occurs-both? var input)))
         (values (or f1 i1) (or f2 i2)))]
      [`(,name ,paralst ,body)
       (if (eqv? var (car paralst))
           (values #f 
                   (call-with-values (lambda ()(var-occurs-both? var body)) 
                                     (lambda(x y) (or x y))))
           (var-occurs-both? var body))])))


      
      