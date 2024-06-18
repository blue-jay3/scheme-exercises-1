#lang racket

; Question 2

; Write a function that takes a string and a list of strings and finds all the strings in the given list,
; which are hidden in the given string. A string is hidden in another string if its letters appear in that
; string in the same order. For example, “cat” is hidden in the string “carter” but “cat” is not hidden
; in “trace”.

(define (hidden-helper? str word)
  (cond
    ((= (string-length str) 0) #f)
    ((= (string-length word) 0) #t)
    ((> (string-length word) (string-length str)) #f)
    ((equal? (string-ref str 0) (string-ref word 0)) (hidden-helper? (substring str 1 (string-length str)) (substring word 1 (string-length word))))
    (#t (hidden-helper? (substring str 1 (string-length str)) word))
  )
)

(define (hidden-words str wordList)
  (cond
    ((null? wordList) '())
    ((hidden-helper? str (car wordList)) (cons (car wordList) (hidden-words str (cdr wordList))))
    (#t (hidden-words str (cdr wordList)))
  )
)

(hidden-words "subdermatoglyphic" '("set" "graphic" "drama" "toy" "brag")) ; '("set" "toy" "brag")