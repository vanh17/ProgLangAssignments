#lang racket
;; Programming Languages, Assignment 9
;;
;; This assignment is meant to help you catch up on the basic Racket syntax.
;;
;; Use `define` for all your function definitions. You can use either the `lambda`
;; version of define or the syntactic sugar'ed version.
(provide (all-defined-out))

;; Write a function `add-nums`. It takes as input a list and it adds up those elements
;; in the list that are actually numbers. You can check if something is a number via
;; `number?`. The result for an empty list should be 0.
;; The reference solution is 5 lines.
(define (add-nums ls)
    (if (null? ls)
        0
        (if (number? (car ls))
               (+ (car ls)(add-nums (cdr ls)))
               (add-nums (cdr ls)))))

;; Write a function `length`. It takes as input a list and returns the length of the
;; list.
;; The reference solution is 4 lines.
(define (length ls)
  (if (null? ls)
      0
      (+ 1 (length (cdr ls)))))




;; Write a function `get-nth`. It takes as input a list and an integer, and it returns
;; the n-th element in the list, starting at index 0. If the integer is negative it
;; should return `(error "negative index")`. If the list is not long enough it should
;; return `(error "list too short")`.
;; The reference solution is 5 lines.
(define (get-nth ls n)
  (cond
    [(< n 0) (error "negative index")]
    [(>= n (length ls)) (error "list too short")]
    [(= n 0) (car ls)]
    [(> n 0) (get-nth (cdr ls) (- n 1))]))


;; Write a function `every-other`. It takes as input a list, and it returns a new list
;; where every other term is skipped. So applied to the list `'(1 2 3)` it should return
;; `'(1 3)`, and the same for the list `'(1 2 3 4)`.
;; The reference solution is 5 lines.
(define (every-other ls)
  (cond
    [(null? ls) null]
    [(null? (cdr ls)) ls]
    [true (append (list (car ls)) (every-other (cdr (cdr ls))))]))


;; Write a function `map`. It takes two arguments: a function and a list. It then
;; returns a new list of the result of applying the function on each element.
;; The reference solution is 5 lines.
(define (map f ls)
  (if (null? ls)
      null
      (cons (f (car ls))
            (map f (cdr ls)))))

;; Write a function `map2`. It takes three arguments: a function that takes two inputs
;; and two lists. It then creates a single new list by applying the function to pairs
;; of values one from each list. The process stops when one of the lists is empty.
;; The reference solution is 5 lines.
(define (map2 f ls1 ls2)
  (if (or (null? ls1) (null? ls2))
      null
      (cons (f (car ls1) (car ls2))
            (map2 f (cdr ls1) (cdr ls2)))))


;; Write a function `filter`. It takes as input a function and a list and returns
;; a new list consisting of those elements for which the function does not return #f
;; The reference solution is 5 lines.


;; Write a function `call-all`. It takes as input a list of "thunks", and returns a
;; list of the results of calling those thunks. To call a function, you put it as the
;; first entry in parentheses, followed by any arguments it may have.
;; The reference solution is 4 lines.

