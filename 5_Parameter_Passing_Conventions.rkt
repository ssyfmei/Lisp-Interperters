#lang racket
;;funtional presentation Setting
(define empty-env
  (lambda ()
    (lambda (x) (error "trying to apply non-literal" x))))
(define extend-env
  (lambda (var val env)
    (lambda (x) (if (eqv? x var) val (env x)))))
(define apply-env
  (lambda (env var)
    (env var)))
(define make-closure
  (lambda (x body env)
    (vector x body env)))
(define apply-closure-cbv
  (lambda (rator rand)
    (let ((env  (vector-ref rator 2))
          (body (vector-ref rator 1))
          (x    (vector-ref rator 0)))
      (val-of-cbv body (extend-env x rand env)))))

(define val-of-cbv
  (λ (exp env)
    (match exp 
      [`() '()]
      [ (? boolean? b) b]
      [ (? number?  n) n]
      [`(quote ,a) (val-of-cbv a env)]
      [`(zero? ,n) (zero? (val-of-cbv n env))]
      [`(sub1 ,n) (sub1 (val-of-cbv n env))]
      [`(add1 ,n) (add1 (val-of-cbv n env))]
      [`(null? ,lst) (null? lst)]
      [`(let ((,key ,val)) ,body)
        (val-of-cbv body (extend-env key (box (val-of-cbv val env)) env))]
      [`(car^ ,lst) ((car (val-of-cbv lst env)))]
      [`(cdr^ ,lst) ((cdr (val-of-cbv lst env)))]
      [`(cons^ ,n1 ,n2) (cons (λ()(val-of-cbv n1 env)) (λ()(val-of-cbv n2 env)))]
      [`(car ,n) (car (val-of-cbv n env))]
      [`(cdr ,n) (cdr (val-of-cbv n env))]
      [`(cons ,n1 ,n2) (cons (val-of-cbv n1 env) (val-of-cbv n2 env))]
      [`(* ,n1 ,n2) (* (val-of-cbv n1 env) (val-of-cbv n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbv test env)
                                    (val-of-cbv conseq env)
                                    (val-of-cbv alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbv e1 env) (val-of-cbv e2 env))]
      [`(set! ,var ,val)(set-box! (apply-env env var) (val-of-cbv val env))] 
      [ (? symbol? y) (unbox (apply-env env y))]
      [`(random ,n) (random (val-of-cbv n env))]
      [`(lambda (,x) ,body) (make-closure x body env)]
      [`(,rator ,(? symbol? rand))
       (apply-closure-cbv (val-of-cbv rator env) (box (unbox (apply-env env rand))) )]
      [`(,rator ,rand) 
       (apply-closure-cbv (val-of-cbv rator env) (box (val-of-cbv rand env)))])))

(define apply-closure-cbr
  (lambda (rator rand)
    (let ((env  (vector-ref rator 2))
          (body (vector-ref rator 1))
          (x    (vector-ref rator 0)))
      (val-of-cbr body (extend-env x rand env)))))

(define val-of-cbr
  (λ (exp env)
    (match exp
      [ (? boolean? b) b]
      [ (? number?  n) n]
      [`(zero? ,n) (zero? (val-of-cbr n env))]
      [`(sub1 ,n) (sub1 (val-of-cbr n env))]
      [`(* ,n1 ,n2) (* (val-of-cbr n1 env) (val-of-cbr n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbr test env)
                                    (val-of-cbr conseq env)
                                    (val-of-cbr alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbr e1 env) (val-of-cbr e2 env))]
      [`(set! ,var ,val)(set-box! (apply-env env var) (val-of-cbr val env))] 
      [ (? symbol? y) (unbox (apply-env env y))]
      [`(random ,n) (random (val-of-cbr n env))]
      [`(lambda (,x) ,body) (make-closure x body env)]
      [`(,rator ,(? symbol? rand)) (apply-closure-cbr (val-of-cbr rator env) 
                                                      (apply-env env rand))]
      [`(,rator ,rand) (apply-closure-cbr (val-of-cbr rator env)
                                          (box (val-of-cbr rand env)))])))


(define apply-closure-cbname
  (lambda (rator rand)
    (let ((env  (vector-ref rator 2))
          (body (vector-ref rator 1))
          (x    (vector-ref rator 0)))
      (val-of-cbname body (extend-env x rand env)))))

(define val-of-cbname
  (λ (exp env)
    (match exp
      [ (? boolean? b) b]
      [ (? number?  n) n]
      [`(zero? ,n) (zero? (val-of-cbname n env))]
      [`(sub1 ,n) (sub1 (val-of-cbname n env))]
      [`(* ,n1 ,n2) (* (val-of-cbname n1 env) (val-of-cbname n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbname test env)
                                    (val-of-cbname conseq env)
                                    (val-of-cbname alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbname e1 env) (val-of-cbname e2 env))]
      [`(set! ,var ,val) (set-box! (apply-env env var) (val-of-cbname val env))] 
      [ (? symbol? y) ((unbox (apply-env env y)))]
      [`(random ,n) (random (val-of-cbname n env))]
      [`(lambda (,x) ,body) (make-closure x body env)]
      [`(,rator ,(? symbol? var)) (apply-closure-cbname (val-of-cbname rator env) 
                                                        (apply-env env rator))]
      [`(,rator ,rand) (apply-closure-cbname (val-of-cbname rator env) 
                                             (box (λ() (val-of-cbname rand env))))])))

(define apply-closure-cbneed
  (lambda (rator rand)
    (let ((env  (vector-ref rator 2))
          (body (vector-ref rator 1))
          (x    (vector-ref rator 0)))
      (val-of-cbneed body (extend-env x rand env)))))

(define unbox/need
  (λ (b)
    (let [(val ((unbox b)))]
      (set-box! b (λ () val))
      val)))

(define val-of-cbneed
  (λ (exp env)
    (match exp
      [ (? boolean? b) b]
      [ (? number?  n) n]
      [`(zero? ,n) (zero? (val-of-cbneed n env))]
      [`(sub1 ,n) (sub1 (val-of-cbneed n env))]
      [`(* ,n1 ,n2) (* (val-of-cbneed n1 env) (val-of-cbneed n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbneed test env)
                                    (val-of-cbneed conseq env)
                                    (val-of-cbneed alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbneed e1 env) (val-of-cbneed e2 env))]
      [`(set! ,var ,val)(set-box! (apply-env env var) (val-of-cbneed val env))]
      [ (? symbol? y) (unbox/need (apply-env env y))]
      [`(random ,n) (random (val-of-cbneed n env))]
      [`(lambda (,x) ,body) (make-closure x body env)]
      [`(,rator ,(? symbol? var))
        (apply-closure-cbneed (val-of-cbneed rator env) (apply-env env var))]
      [`(,rator ,rand) 
        (apply-closure-cbneed (val-of-cbneed rator env) (box (λ() (val-of-cbneed rand env))))])))