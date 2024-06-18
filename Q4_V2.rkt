#lang racket

; from lab 5

(define (swap heap j k) ; swaps element x with element y in the heap
  (display j)
  (newline)
  (let ((vec (vector-copy heap)))
    (do ((i 0 (+ i 1))) ((= i (vector-length heap)) vec)
      (cond
        ((= i j) (vector-set! vec i (vector-ref heap k)))
        ((= i k) (vector-set! vec i (vector-ref heap j)))
      )
    )
  )
)

(define (remove-root heap)
  (if (= (vector-length heap) 1) '#() (newline))
  (let ((newHeap (swap heap 0 (- (vector-length heap) 1)))) ; swap the root with the last element in the tree
    (list->vector (reverse (cdr (reverse (vector->list newHeap)))))
  )
)

(define (downheap heap index) ; modified for maxheap
  (let ((lchild (+ (* 2 index) 1)) (rchild (+ (* 2 index) 2))) ; find indexes of left child and right child
    (cond
      ((>= lchild (vector-length heap)) heap)
      ((>= rchild (vector-length heap))
       (if (> (vector-ref heap lchild) (vector-ref heap index))
           (downheap (swap heap lchild index) lchild)
           heap
       )
      )

      ((or (> (vector-ref heap lchild) (vector-ref heap index)) (> (vector-ref heap rchild) (vector-ref heap index)))
       (cond
         ((>= (vector-ref heap lchild) (vector-ref heap rchild)) (downheap (swap heap lchild index) lchild))
         (#t (downheap (swap heap rchild index) rchild))
        )
      )
      (#t heap)
    )
  )
)

(define (remove heap)
  (downheap (remove-root heap) 0)
)

; Question 4

; In class, you learned about two sorting methods, which are merge sort and quick sort. In this
; problem, you should implement two other sorting methods.
; a) Using the heap data structure that you implemented in the lab, write a function that receives a
; vector and sorts the vector using heap sort.
; b) Write a function that receives a vector and implements the insertion sort.

; N - N/2 = num leaves, then everything less then heap length - num leaves has two children

(define (to-max-heap arr i)
  (cond
    ((< i 0) arr)
    ((or
      (< (vector-ref arr i) (vector-ref arr (+ (* 2 i) 1)))
      (< (vector-ref arr i) (vector-ref arr (+ (* 2 i) 2)))
     )
     (to-max-heap (downheap arr i) (- i 1))
    )
    (#t (to-max-heap arr (- i 1)))
  )
)

(define (heapify arr)
  (to-max-heap arr (- (vector-length arr) (quotient (vector-length arr) 2) 2))
)

(heapify '#(2 6 1 7 3 9 10 4 11)) ; '#(11 7 10 6 3 9 1 4 2)

(define (heap-sort-helper arr)
  (if (= (vector-length arr) 0) '() (cons (vector-ref arr 0) (heap-sort-helper (remove arr))))
)

(define (heap-sort arr)
  (list->vector (reverse (heap-sort-helper (heapify arr))))
)

(heap-sort '#(10 18 3 22 9 31 45 28)) ; '#(3 9 10 18 22 28 31 45)
;(insertion-sort '#(10 18 3 22 9 31 45 28)) ; '#(3 9 10 18 22 28 31 45)
