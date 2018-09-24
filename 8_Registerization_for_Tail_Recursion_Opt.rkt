#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Thread simulation. Three function executed concurrently. 
; The first to complete will be returned
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define inner-k
  (lambda (v k)
    `(inner-k ,v ,k)))
(define outer-k
  (lambda (n k)
    `(outer-k ,n ,k)))
(define ramp-empty-k
  (lambda (jumpout) `(ramp-empty-k ,jumpout)))

(define rampoline
  (lambda (th1 th2 th3)
    (let [(n (random 3))]
      (cond
        ((= n 0) (rampoline (th1) th2 th3))
        ((= n 1) (rampoline th1 (th2) th3))
        ((= n 2) (rampoline th1 th2 (th3)))))))

(define fib-ramp-driver
  (lambda (n1 n2 n3)
    (let/cc jumpout
      (rampoline
        (lambda ()
          (fib n1 (ramp-empty-k jumpout)))
        (lambda ()
          (fib n2 (ramp-empty-k jumpout)))
        (lambda ()
          (fib n3 (ramp-empty-k jumpout)))))))

(define fib
  (lambda (n k)
    (cond
      [(= n 0) (lambda () (ramp-apply-k k 0))]
      [(= n 1) (lambda () (ramp-apply-k k 1))]
      [else (lambda () (fib (- n 2) (outer-k n k)))])))


(define ramp-apply-k
  (lambda (k v)
    (match k
      [`(ramp-empty-k ,jumpout) (lambda () (jumpout v))]
      [`(inner-k ,w ,k) (lambda () (ramp-apply-k k (+ v w)))]
      [`(outer-k ,w ,k) (lambda () (fib (- w 1) (inner-k v k)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Thread simulation. Two function executed concurrently. 
; Result are collected in a list.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define bi-tramp-empty-k
  (lambda (jumpout res) `(bi-tramp-empty-k ,jumpout ,res)))
(define venti-k
  (lambda (n k) `(venti-k ,n ,k)))
(define grande-k
  (lambda (arg-1 n k) `(grande-k ,arg-1 ,n ,k)))
(define tall-k
  (lambda (arg-1 arg-2 k) `(tall-k ,arg-1 ,arg-2 ,k)))

(define pc
  (lambda() pc))
(define bi-rampoline
  (lambda (th1 th2 )
    (bi-rampoline (th1) (th2))))

(define bi-tramp-driver 
  (lambda (n1 n2)
    (define res (mcons #f '()))
    (let/cc jumpout
      (bi-rampoline
        (lambda ()
          (trib-k n1 (bi-tramp-empty-k jumpout res)))
        (lambda ()
          (trib-k n2 (bi-tramp-empty-k jumpout res)))))))

(define bi-apply-k
  (lambda (k v^)
    (match k
      [`(bi-tramp-empty-k ,jumpout ,res) (lambda () (if (not (mcar res))
                                                    (begin (set-mcar! res v^) pc)
                                                    (jumpout (list (mcar res) v^))))]
      [`(venti-k ,n ,k)     (lambda ()(trib-k (- n 2) (grande-k v^ n k)))]
      [`(grande-k ,w ,n ,k) (lambda ()(trib-k (- n 1) (tall-k w v^ k)))]
      [`(tall-k   ,w ,u ,k) (lambda () (bi-apply-k k (+ w u v^)))])))
    
(define trib-k
  (lambda (n k)
    (cond
      [(< n 3) (lambda () (bi-apply-k k 1))]
      [else (lambda () (trib-k (- n 3) (venti-k n k)))])))