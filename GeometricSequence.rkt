#lang racket

; Returns a list that contains the first n numbers in the sequence without using recursion.

(define (geometric-sequence-loop a r n)
  (let* ((seq '()))
    (do ((i (- n 1) (- i 1))) ((= i -1) seq)
      (set! seq (cons (* a (expt r i)) seq))
    )
  )
)

(geometric-sequence-loop 7 3 5) ; '(7 21 63 189 567)
(geometric-sequence-loop 9 -1 7) ; '(9 -9 9 -9 9 -9 9)