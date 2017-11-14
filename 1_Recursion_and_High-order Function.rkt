#lang racket
(define countdown
  (λ (n)
    (if (= 0 n)
        '(0)
        (cons n (countdown (sub1 n))))))

(define insertR
  (λ (old in lst)
    (cond
      ((null? lst) '())
      ((eq? old (car lst))
       (cons old (cons in (insertR old in (cdr lst)))))
      (else (cons (car lst) (insertR old in (cdr lst)))))))

(define remv-1st
  (λ (ato lst)
    (cond
      [(null? lst) '()]
      [(eq? (car lst) ato) (cdr lst)]
      [else (cons (car lst)
                  (remv-1st ato (cdr lst)))])))
(define list-index-ofv?
  (lambda (a ls)
    (cond
      [(null? ls) 0]
      [(eq? a (car ls)) 0]
      [ else (add1 (list-index-ofv? a (cdr ls)))])))

(define count-?s
  (λ (lst)
    (cond
      ([null? lst] 0)
      ([eq? '? (car lst)] [add1 (count-?s (cdr lst))])
      (else [count-?s (cdr lst)]))))
(define filter-even
  (λ (lst)
    (cond
      ([null? lst] '())
      ([even? (car lst)] [filter-even (cdr lst)])
      (else [cons (car lst) (filter-even (cdr lst))]))))
(define zip
  (λ (ls1 ls2)
    (cond
      ([or (null? ls1) (null? ls2)] '())
      (else [cons (cons (car ls1) (car ls2))
                  (zip  (cdr ls1) (cdr ls2))]))))
(define map
  (λ (fun lst)
    (if [null? lst]
        '()
        [cons (fun (car lst))
              (map fun (cdr lst))])))

(define append
  (λ (ls1 ls2)
    (if (null? ls1)
        ls2
        (cons (car ls1) 
              (append (cdr ls1) ls2)))))

(define reverse
  (λ (lst)
    (letrec
        ([A (λ (ls ls1)
               (if [null? ls]
                   ls1
                   (A (cdr ls) (cons (car ls) ls1))))])
      (A lst '()))))

(define fact
  (λ (n)
    (letrec ([A (λ (n acc)
                  (if (= n 0)
                      acc
                      (A (sub1 n) (* n acc))))])
      (A n 1))))
         
(define member-?*
  (λ (lst)
    (call/cc
     (λ (k)
       (cond
         ([null? lst] #f)
         ([pair? (car lst)][or (member-?* (car lst))
                               (member-?* (cdr lst))])
         (else [if [eq? '? (car lst)] 
                   [k #t]
                   [member-?* (cdr lst)]]))))))          

(define fib
  (λ (n)
    (letrec ([A (λ (ac1 ac2 sum)
                   (cond
                     ([= 0 sum] ac1)
                     ([= 1 sum] ac2)
                     (else (A ac2 (+ ac1 ac2) (sub1 sum)))))])
      (A 0 1 n))))

;;(equal? '((w x) y (z)) '((w . (x . ())) . (y . ((z . ()) .()))))

(define binary->natural
  (λ (lst)
    (cond
      ([null? lst] 0)
      ([eq? 1 (car lst)] [add1 (* 2 (binary->natural (cdr lst)))])
      ([eq? 0 (car lst)] [* 2 (binary->natural (cdr lst))])
      (else (error "illegal")))))
(define minus
  (λ (n m)
    (cond
      ([= 0 m] n)
      (else (minus (sub1 n) (sub1 m))))))
(define div
  (λ (n d)
    (cond
      ([= 0 n] 0)
      ([> 0 n] error "not diviable")
      (else [add1 (div (- n d) d)]))))
(define append-map
  (λ (fun lst)
    (cond
      ([null? lst] '())
      (else (append (fun (car lst)) (append-map fun (cdr lst)))))))
(define set-difference
  (λ (s1 s2)
    (define member?
    (λ (a lst)
      (call/cc 
       (λ (k)
         (cond
           ([null? lst] #f)
           ([eq? (car lst) a][k #t]) ;;;[set-cdr ]
           (else (member a (cdr lst))))))))
    (cond
      ([null? s2] s1)
      ([null? s1] '())
      ((member? (car s1) s2)
       (set-difference (cdr s1) s2))
      (else (cons (car s1)
                  (set-difference (cdr s1) s2))))))

(define powerset
  (λ (lst)
    (cond
      ([null? lst] '(()))
      (else
       [let ([hd (car lst)] [rec (powerset (cdr lst))])
         (append (map [λ (x) (cons hd x)] rec) rec)]))))

(define cartesian-product
  (λ (lst)
    (let ([lx (car lst)] [ly (cadr lst)])
      (cond
        ([null? lx] '())
        (else [append (map (λ(x) (list (car lx) x))
                           ly)
                      (cartesian-product (list (cdr lx) ly))])))))

(define insertR-fr
  (λ (old in lst)
     (foldr (λ (a b) (if (eq? a old) (cons old (cons in b)) (cons a b))) 
            '() 
            lst)))
(define count-?s-fr
  (λ (lst)
    (foldr (λ (a b) (if (eq? a '?) (add1 b) b)) 0 lst)))
(define filter-fr
  (λ (fun ls)
    (foldr (λ (a b) (if (fun a) (cons a b) b)) '() ls)))
(define zip-fr
  (λ (ls1 ls2)
    (foldr (λ (a b c) (cons (cons a b) c)) '() ls1 ls2)))
(define map-fr
  (λ (fun ls)
    (foldr (λ (a b) (cons (fun a) b)) '() ls)))
(define append-fr
  (λ (ls1 ls2)
    (foldr cons ls2 ls1)))
(define reverse-fr
  (λ (ls)
    (foldr (λ (a b) (append-fr b (list a))) '() ls)))
(define reverse-fl
  (λ (ls)
    (foldl cons '() ls)))
(define binary->natural-fr
  (λ (ls)
    (foldr (λ (a b) (+ a (* 2 b))) 0 ls)))
(define append-map-fr
  (λ (fun ls)
    (foldr (λ (a b) (append (fun a) b)) '() ls)))
(define set-difference-fr
  (λ (ls1 ls2)
    (foldr (λ (a b) 
             (if (foldr (λ (a1 b1) (or (eq? a a1) b1))  #f ls2) 
                 b 
                 (cons a b)))
           '()
           ls1)))
(define powerset-fr
  (λ (ls)
    (foldr (λ (a b)
             (append-fr b
                        (map-fr (λ (a1) (cons a a1)) b))) '(()) ls)))

(define cartesian-product-fr
  (λ (lst)
    (let ([lx (car lst)] [ly (cadr lst)])
      (foldr (lambda(a ls)
               (append (foldr 
                      (lambda (b lss) (cons (cons a (list b)) lss)) 
                      '() ly) ls)) 
             '() lx))))

(define collatz
  (letrec
      ([odd-case
        (λ (recur)
          (λ (x)
            (cond
              ((and (positive? x) (odd? x)) (collatz (add1 (* x 3))))
              (else (recur x)))))]
       [even-case
        (λ (recur)
          (λ (x)
            (cond
              ((and (positive? x) (even? x)) (collatz (/ x 2)))
              (else (recur x)))))]
       [one-case
        (λ (recur)
          (λ (x)
            (cond
              ((zero? (sub1 x)) 1)
              (else (recur x)))))]
       [base
        (λ (x)
          (error 'error "INvalid value ~s~n" x))])
    (λ(x)
      ((one-case (even-case (odd-case base))) x))
    ))

(define quine
  '((lambda (x y) (list y (list 'quote y) (list 'quote y)))
    '(lambda (x y) (list y (list 'quote y) (list 'quote y)))
    '(lambda (x y) (list y (list 'quote y) (list 'quote y)))))
