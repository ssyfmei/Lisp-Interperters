#lang racket
(define last-non-zero
  (λ (ls)
    (let/cc k
      (letrec 
          ((last-non-zero
            (λ(ls)
              (cond
                [(null? ls) '()]
                [(zero? (car ls)) (k (last-non-zero (cdr ls)))]
                [else (cons (car ls)
                            (last-non-zero (cdr ls)))]))))
        (last-non-zero ls)))))
(define (lex exp acc)
  (lex1 exp acc #f))

(define lex1
  (lambda (exp acc kk)
    (let ((n (length acc)))
      (match exp
        [ (? symbol? x) (if  (assv x acc)
                            `(var ,(- n 1 (cdr (assv x acc))))
                            `(free-var ,x))]
        [ (? number? num) `(const ,num)]
        [ (? boolean? bl) `(const ,bl)]
        [`(if ,pre ,cons ,alt) `(if ,(lex1 pre acc kk) ,(lex1 cons acc kk) ,(lex1 alt acc kk))]
        [`(zero?   ,nexp) `(zero ,(lex1 nexp acc kk))]
        [`(sub1    ,nexp) `(sub1 ,(lex1 nexp acc kk))]
        [`(throw ,k ,v)   `(throw ,(lex1 k acc kk) ,(lex1 v acc kk))]          
        [`(* ,nexp1 ,nexp2) `(mult ,(lex1 nexp1 acc kk) ,(lex1 nexp2 acc kk))]
        [`(let [(,key ,val)] ,body) `(let ,(lex1 val acc kk) ,(lex1 body (cons (cons key n) acc) kk))]
        [`(let/cc ,k ,body) `(letcc ,(lex1 body (cons (cons k n) acc) k))] 
        [`(,rator ,rand) (if  (eqv? rator kk)
                             `(throw ,(lex1 rator acc kk) ,(lex1 rand acc kk))
                             `(app   ,(lex1 rator acc kk) ,(lex1 rand acc kk)))]
        [`(lambda (,arg) ,body) `(lambda ,(lex1 body (cons (cons arg n) acc) kk))]))))



(define value-of
  (lambda (expr env)
    (match expr
      [`(const ,expr) expr]
      [`(mult ,x1 ,x2) (* (value-of x1 env) (value-of x2 env))]
      [`(sub1 ,x) (sub1 (value-of x env))]
      [`(zero ,x) (zero? (value-of x env))]
      [`(if ,test ,conseq ,alt) (if (value-of test env)
                                    (value-of conseq env)
                                    (value-of alt env))]
      [`(letcc ,body) (let/cc k
                         (value-of body (lambda (y) (if (zero? y) k (env (sub1 y))))))]
      [`(throw ,k-exp ,v-exp) ((value-of k-exp env) (value-of v-exp env))]
      [`(let ,e ,body) (let ((a (value-of e env)))
                         (value-of body (lambda (y) (if (zero? y) a (env (sub1 y))))))]
      [`(var ,expr) (env expr)]
      [`(lambda ,body) (lambda (a) (value-of body (lambda (y) (if (zero? y) a (env (sub1 y))))))]
      [`(app ,rator ,rand) ((value-of rator env) (value-of rand env))])))


(define empty-env
  (lambda ()
    (lambda (y)
      (error 'value-of "unbound identifier"))))
(define empty-k
  (lambda ()
    (lambda (v) v)))

(define apply-k
  (λ (k ans)
    (k ans))) 
(define apply-env
  (λ (env y k^)
     (apply-k k^ (env y))))

(define apply-closure
  (λ (clo y k^)
     (clo y k^)))
(define extend-env
  (λ (env val)
    (λ (y)
      (if (zero? y) val (env (sub1 y))))))
(define make-closure
  (λ (body env)
    (λ (y k^)
      (value-of-cps body (extend-env env y) k^))))

(define value-of-cps
  (lambda (expr env k^)
    (match expr
      [`(var ,expr)   (apply-env env expr k^)]
      [`(const ,expr) (apply-k k^ expr)]
      [`(mult ,x1 ,x2) (value-of-cps x1 env 
                                     (λ (v1) (value-of-cps x2 env 
                                                           (λ (v2) (apply-k k^ (* v1 v2))))))] 
      [`(sub1 ,x) (value-of-cps x env (λ (v) (apply-k k^ (sub1 v))))]
      [`(zero ,x) (value-of-cps x env (λ (v) (apply-k k^ (zero? v))))]
      [`(if ,test ,conseq ,alt) (value-of-cps test env (λ(v)(if v (value-of-cps conseq env k^)
                                                                  (value-of-cps alt    env k^))))]
      [`(let ,e ,body) (value-of-cps e env (λ (v) (value-of-cps body (extend-env env v) k^)))]
      [`(letcc  ,body) (value-of-cps body  (extend-env env k^) k^)]
      [`(throw  ,k-exp ,v-exp) (value-of-cps k-exp env (λ (v2) (value-of-cps v-exp env v2)))]                                                                         
      [`(lambda ,body) (apply-k k^ (make-closure body env))]
      [`(app ,rator ,rand) (value-of-cps rator env
                                         (λ(v1) (value-of-cps rand env (λ(v2) (apply-closure v1 v2 k^)))))])))


(define-syntax cons$
  (syntax-rules ()
    ((cons$ x y) (cons x (delay y)))))

(define car$ car)
(define cdr$
  (lambda($) (force (cdr $))))

(define inf-1s (cons$ 1 inf-1s))

(define take$
  (lambda (n $)
    (cond
      [(zero? n) '()]
      [else (cons (car$ $)
                  (let  ((n- (sub1 n)))
                    (cond
                      ((zero? n-) '())
                      (else (take$ n- (cdr$ $))))))])))
(define trib$
  (letrec[(A (lambda(m n h)
               (cons$ m (A n h (+ m n h)))))]
    (A 0 1 1)))



