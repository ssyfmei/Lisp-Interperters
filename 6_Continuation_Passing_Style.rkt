#lang racket
(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only
	    (error 'empty-k "You can only invoke the empty continuation once")
	    (begin (set! once-only #t) v))))))

(define binary-to-decimal-cps
  (λ (n k)
    (cond
      [(null? n) (k 0)]
      [ else (binary-to-decimal-cps
              (cdr n) (λ (v)
                        (k (+ (car n) (* 2 v)))))])))
(define binary-to-decimal
  (λ (n)
    (binary-to-decimal-cps n (empty-k))))

(define times-cps
  (λ (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) 0]
      [ else  (times-cps (cdr ls)
                         (λ (v) (k (* (car ls) v))))])))
(define times
  (λ (ls)
    (times-cps ls (empty-k))))

(define plus-cps
  (λ (m k)
    (λ (n)
      (k (+ m n)))))
(define plus
  (λ (m)
    (plus-cps m (empty-k))))

(define remv-first-9*-cps
  (λ (ls k)
    (cond
      [(null? ls) (k '())]
      [(pair? (car ls))
       (remv-first-9*-cps 
        (car ls)
        (λ (v) (cond
                 [(equal? (car ls) v)
                  (remv-first-9*-cps 
                   (cdr ls)
                   (λ (w) (k (cons (car ls) w))))]
                 [else (k (cons v (cdr ls)))])))]
      [(eqv? (car ls) '9) (k (cdr ls))]
      [else  (remv-first-9*-cps 
              (cdr ls)
              (λ (v) (k (cons (car ls) v))))])))
(define remv-first-9*
  (λ (ls)
    (remv-first-9*-cps  ls (empty-k))))

(define cons-cell-count-cps
  (λ (ls k)
    (cond
      [(pair? ls)
       (cons-cell-count-cps 
        (car ls)
        (λ(v)
          (cons-cell-count-cps 
           (cdr ls) 
           (λ(w)
             (k (add1 (+ v w)))))))]
      [else (k 0)])))
(define cons-cell-count
  (λ (ls)
    (cons-cell-count-cps ls (empty-k))))

(define find-cps
  (λ (u s k)
    (let ((pr (assv u s)))
      (if pr
          (find-cps (cdr pr) s k)
          (k u)))))
(define find
  (λ (u s)
    (find-cps u s (empty-k))))

(define ack-cps
  (λ (m n k)
    (cond
      [(zero? m) (k (add1 n))]
      [(zero? n) (ack-cps (sub1 m) 1 k)]
      [ else (ack-cps m (sub1 n)
                  (λ (v) (ack-cps (sub1 m) v k)))])))
(define ack
  (λ (m n)
    (ack-cps m n (empty-k))))     

(define fib-cps
  (λ (n k)
    ((λ(fib)
       (fib fib n k))
     (λ(fib n k)
       (cond
         [(zero? n) (k 0)]
         [(zero? (sub1 n)) (k 1)]
         [else (fib fib (sub1 n) 
                    (λ (v) (fib fib (sub1 (sub1 n))(λ (w)
                                                     (k (+ v w))))))])))))
(define fib
  (λ (n)
    (fib-cps n (empty-k))))
(define unfold-cps
  (λ (p f g seed k)
    ((lambda (h)
       ((h h) seed '() k))
     (lambda (h)
       (lambda (seed ans k)
         (p seed (λ (pv)
                   (if pv
                       (k ans)
                       (g seed (λ(gv)
                                 (f seed (λ(fv)
                                           ((h h) gv (cons fv ans) k)))))))))))))
(define null?-cps
    (lambda (ls k)
      (k (null? ls))))
 (define car-cps
    (lambda (pr k)
      (k (car pr))))
 (define cdr-cps
    (lambda (pr k)
      (k (cdr pr))))
(define empty-s
  (lambda () '()))
(define unify-cps
  (λ (u v s k)
    (cond
      ((eqv? u v) (k s))
      ((number? u) (k (cons (cons u v) s)))
      ((number? v) (unify-cps v u s k))
      ((pair? u)
       (if (pair? v)
           (find-cps 
            (car u) s 
            (λ(v1) (find-cps
                    (car v) s
                    (λ (w) 
                      (unify-cps v1 w s (λ (uv)
                                        (let ((s uv))
                                          (if s
                                              (find-cps 
                                               (cdr u) s
                                               (λ (dv)
                                                 (find-cps
                                                  (cdr v) s
                                                  (λ (dw)
                                                    (unify-cps dv dw s k)))))
                                              #f))))))))
           (k #f)))
      (else (k #f)))))
(define unify
  (λ(u v s)
    (unify-cps u v s (empty-k))))

(define M-cps
  (λ(f)
    (λ(ls)
      (λ(k)
        (cond
          ((null? ls) (k '()))
          (else ((f (car ls)) 
                 (λ(v)
                   (((M-cps f) (cdr ls))
                    (λ(w)
                      (k (cons v w))))))))))))
  
(define use-of-M-cps
  (((M-cps (λ(n)(λ(k)(k (add1 n)))))
    '(1 2 3 4 5))
   (empty-k)))

(define strange
  (lambda (x)
    ((lambda (g) (lambda (x) (g g)))
     (lambda (g) (lambda (x) (g g))))))

(define strange-cps
  (λ (x k)
    ((λ (g k) (k (λ (x k) (g g k))))
     (λ (g k) (k (λ (x k) (g g k))))
     k)))
  
(define use-of-strange
  (let ([strange^ (((strange 5) 6) 7)])
    (((strange^ 8) 9) 10)))

(define use-of-strange-cps '())

(define almost-length
  (λ(f)
    (λ(ls k)
      (if (null? ls)
          (k 0)
          (f (cdr ls) (λ(v) (k (add1 v))))))))

(define why
  (λ(al)
    ((λ(g) (al (λ (x) ((g g) x))))
     (λ(g) (al (λ (x) ((g g) x)))))))

(define why-cps
  (λ(a1 k)
    ((λ(g k) (a1 (λ (x k)(g g (λ(v) (v x k)))) k))
     (λ(g k) (a1 (λ (x k)(g g (λ(v) (v x k)))) k))
     k)))

(define why-cps-cps
  (λ(f k1 k2)
    ((λ(g k1 k2) (f (λ (x k1 k2) (g g (λ(v kk) (v x k1 kk)) k2)) k1 k2))
     (λ(g k1 k2) (f (λ (x k1 k2) (g g (λ(v kk) (v x k1 kk)) k2)) k1 k2))
     k1
     k2)))