#lang racket

; Takes the coefficients of a quadratic equation and returns a list of its roots.

(define (solve-quadratic a b c)
  (let ((discriminant (- (* b b) (* (* 4 a) c))))
    (cond
      ((< discriminant 0) '())
      ((= discriminant 0) (cons (/ (* -1 b) (* 2 a)) '()))
      (#t (cons (/ (+ (* -1 b) (sqrt discriminant)) (* 2 a)) (cons (/ (- (* -1 b) (sqrt discriminant)) (* 2 a)) '())) 
      )
    )
  )
)

(solve-quadratic 4 5 -10) ; '(1.075 -2.235)
(solve-quadratic 1 -2 1) ; '(1)
(solve-quadratic 3 2 1) ; '()