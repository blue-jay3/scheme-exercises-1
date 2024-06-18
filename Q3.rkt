#lang racket

; Question 3

; A geometric sequence is a sequence that each number is multiplied by the same ratio to generate
; the next number in the sequence.
; For example,
; 7, 21, 63, 189, ... is a geometric sequence whose ratio is 3.
; Write a function that receives two real numbers, which are the first number in the sequence and
; the ratio and an integer n, which is the number of numbers in the sequence, and returns a list that
; contains the first n numbers in the sequence. Your function should use a loop and must not be
; recursive.

(define (geometric-sequence-loop a r n)
  (let* ((seq '()))
    (do ((i (- n 1) (- i 1))) ((= i -1) seq)
      (set! seq (cons (* a (expt r i)) seq))
    )
  )
)

(geometric-sequence-loop 7 3 5) ; '(7 21 63 189 567)
(geometric-sequence-loop 9 -1 7) ; '(9 -9 9 -9 9 -9 9)