#lang racket
(define empty-aenv
  (λ ()
    (λ (env)
      (λ (x)
        (env x)))))
(define extend-aenv
  (λ (var val aenv)
    (λ (env)
      (λ (x)
        (if (eqv? x var) val ((aenv env) x))))))

(define value-of
  (let ([aenv (empty-aenv)])              ;; Brain Teasers
    (λ (exp env)
      (match exp
        [(? number? x)  x]
        [(? boolean? x) x]
        [`(sub1  ,rand) (- (value-of rand env) 1)]
        [`(zero? ,rand) (= (value-of rand env) 0)]
        [`(* ,n1 ,n2 )  (* (value-of n1 env)
                           (value-of n2 env))]
        [`(if ,pred ,cosq ,altr)
         (if (value-of pred env)
             (value-of cosq env)
             (value-of altr env))]
        [ (? symbol? y) ((aenv env) y)]
        [`(begin2 ,e1 ,e2) (begin (value-of e1 env) (value-of e2 env))]
        [`(set! ,var ,val)
         (begin (env var)
                (set! aenv (extend-aenv var (value-of val env) aenv)))]
        [`(let ([,var ,val]) ,body)
         (value-of body (λ(x) (if (eqv? x var) (value-of val env) (env x))))]
        [`(lambda (,arg) ,body) `(closure ,arg ,body ,env)]
        [`(,rator ,rand)
         (match (value-of rator env)
           [`(closure ,arg ,body ,envp) 
            (value-of body 
                      (λ(x) (if (eqv? x arg) (value-of rand env) (envp x))))]
           [else (error "unmatched app pattern")])]
        [else (error "unmatched exp pattern ~s" exp)]))))


(define empty-env-fn
  (lambda()
    (lambda (y) (error 'value-of "unbound variable ~s" y))))
(define extend-env-fn
  (λ (var val env)
    (λ (x) (if (eqv? x var) val (env x)))))
(define apply-env-fn
  (λ (env var)
    (env var)))
(define value-of-fn
  (λ (exp env)
    (match exp
      [(? number? x)  x]
      [(? symbol? x) (apply-env-fn env x)]
      [(? boolean? x) x]
      [`(sub1  ,rand) (- (value-of-fn rand env) 1)]
      [`(zero? ,rand) (= (value-of-fn rand env) 0)]
      [`(* ,n1 ,n2 )  (* (value-of-fn n1 env)
                         (value-of-fn n2 env))]
      [`(if ,pred ,cosq ,altr)
       (if (value-of-fn pred env)
           (value-of-fn cosq env)
           (value-of-fn altr env))]
      [`(let ([,var ,val]) ,body)
       (value-of-fn body (extend-env-fn var (value-of-fn val env) env))]
      [`(lambda (,arg) ,body) `(closure ,arg ,body ,env)]
      [`(,rator ,rand)
       (match (value-of-fn rator env)
         [`(closure ,arg ,body ,envp) 
          (value-of-fn body (extend-env-fn arg (value-of-fn rand env) envp))]
         [else (error "unmatched app pattern")])]
      [else (error "unmatched exp pattern ~s" exp)])))

(define empty-env-ds
  (λ ()
    '()))
(define extend-env-ds
  (λ (var val env)
    (cons (cons var val) env)))
(define apply-env-ds
  (λ (env var)
    (let ([key (assv var env)])
      (if key
          (cdr key)
          (error 'value-of "unbound variable ~s" var)))))
(define set-env-ds
  (λ (env var val)
    (cond
      [(null? env) (cons (cons var val) env)]
      [(eqv? var (caar env)) (cons (cons var val) (cdr env))]
      [else (cons (car env) (set-env-ds (cdr env) var val))])))
     
(define value-of-ds
  (λ (exp env)
    (match exp
      [(? number? x)  x]
      [(? symbol? x) (apply-env-ds env x)]
      [(? boolean? x) x]
      [`(sub1  ,rand)  (- (value-of-ds rand env) 1)]
      [`(set! ,var ,val) (set! env (set-env-ds env var (value-of-ds val env)))]
      [`(begin2 ,bra1 ,bra2) (begin (value-of-ds bra1 env)
                                    (value-of-ds bra2 env))]
      [`(zero? ,rand) (= (value-of-ds rand env) 0)]
      [`(* ,n1 ,n2 )  (* (value-of-ds n1 env)
                         (value-of-ds n2 env))]
      [`(+ ,n1 ,n2 )  (+ (value-of-ds n1 env)
                         (value-of-ds n2 env))]
      [`(if ,pred ,cosq ,altr)
       (if (value-of-ds pred env)
           (value-of-ds cosq env)
           (value-of-ds altr env))]
      [`(let ([,var ,val]) ,body)
       (value-of-ds body (extend-env-ds var (value-of-ds val env) env))]
      [`(lambda (,arg) ,body) `(closure ,arg ,body ,env)]
      [`(,rator ,rand)
       (match (value-of-ds rator env)
         [`(closure ,arg ,body ,envp) 
          (value-of-ds body (extend-env-ds arg (value-of-ds rand env) envp))]
         [else (error "unmatched app pattern")])]
      [else (error "unmatched exp pattern ~s" exp)])))
  
(define empty-env
  (lambda()
    (lambda (y) (error 'value-of "unbound variable ~s" y))))

(define fo-eulav
  (λ (exp env)
    (match exp
      [(? number? x)  x]
      [(? symbol? x) (env x)]
      [(? boolean? x) x]
      [`(,rand  1bus) (- (fo-eulav rand env) 1)]
      [`(,rand  ?orez) (= (fo-eulav rand env) 0)]
      [`(,n1 ,n2 *)  (* (fo-eulav n1 env)
                        (fo-eulav n2 env))]
      [`(,altr ,cosq ,pred fi)
       (if (fo-eulav pred env)
           (fo-eulav cosq env)
           (fo-eulav altr env))]
      [`(,body ([,var ,val]) tel)
       (fo-eulav body (λ(x) (if (eqv? x var) (fo-eulav val env) (env x))))]
      [`(,body (,arg) adbmal) `(closure ,arg ,body ,env)]
      [`(,rand ,rator)
       (match (fo-eulav rator env)
         [`(closure ,arg ,body ,envp) 
          (fo-eulav body 
                    (λ(x) (if (eqv? x arg) (fo-eulav rand env) (envp x))))]
         [else (error "unmatched app pattern")])]
      [else (error "unmatched exp pattern ~s" exp)])))


(define value-of-lex
  (lambda (exp env)
    (match exp
      [`(const ,expr) expr]
      [`(mult ,x1 ,x2) (* (value-of-lex x1 env) (value-of-lex x2 env))]
      [`(zero ,x) (zero? (value-of-lex x env))]
      (`(sub1 ,body) (sub1 (value-of-lex body env)))
      (`(if ,t ,c ,a) (if (value-of-lex t env) (value-of-lex c env) (value-of-lex a env)))
      (`(var ,num) (apply-env-lex env num))
      (`(lambda ,body) (lambda (a) (value-of-lex body (extend-env-lex a env))))
      (`(,rator ,rand) ((value-of-lex rator env) (value-of-lex rand env))))))
 
(define empty-env-lex 
  (lambda () '()))
(define apply-env-lex list-ref)
(define extend-env-lex cons)


(define c0 (λ (f) (λ (x) x)))
(define c5 (λ (f) (lambda (x) (f (f (f (f (f x))))))))

(define csub1
  (λ (m)
    (λ (f)
      (λ (x)
        (((m (λ(b) 
               (λ (h) 
                 (h (b f)))))
          (λ (b) x))
         (λ (z) z))))))