#lang racket

(define (totalCost n)
  (cond ((<= n 0) 0)
        ((<= n 10) (* 5 n))
        ((<= n 20) (+ (* 4 (- n 10)) (totalCost 10)))
        ((<= n 30) (+ (* 3.5 (- n 20)) (totalCost 20)))
        ((<= n 50) (+ (* 3 (- n 30)) (totalCost 30)))
        (else (+ -10 (* 3 (- n 30)) (totalCost 30)))
  )
)

(define (dot l1 l2)
  (cond ((or (null? l1) (null? l2)) 0)
        (else (+ (* (car l1) (car l2)) (dot (cdr l1) (cdr l2) )))
  )
 )

(define (adjDiff ls)
  (cond ((or (null? ls) (null? (cdr ls))) '())
        (else (cons (- (cadr ls) (car ls)) (adjDiff (cdr ls))))
  )
 )

(define (collatz-length n)
  (cond ((<= n 1) 0)
        ((even? n) (+ 1 (collatz-length (/ n 2))))
        (else (+ 1 (collatz-length (+ 1 (* n 3)))))
        )
  )

(define (trunc-helper a b)
  (lambda (n)
    (cond ((< n a) a)
          ((> n b) b)
          (else n))
  )
)

(define (trunc a b x)
  (map (trunc-helper a b) x)
  )

(define (leq l1 l2)
  (foldl (lambda (a b) (and a b)) #t (map (lambda (x y) (<= x y)) l1 l2))
 )

