#lang racket
;; Programming Languages, Assignment 9 tests
;;
;; This file uses the rackunit testing framework. Read more at:
;; https://docs.racket-lang.org/rackunit/index.html
;; To test, simply run this file in DrRacket.

(require "assignment9sub.rkt")
(require rackunit)

(test-equal? "add-nums: empty list" (add-nums (list)) 0)
(test-equal? "add-nums: non-number" (add-nums (list 1 2 'a 3)) 6)

(test-equal? "length: empty list" (length (list)) 0)

(test-exn "get-nth: negative index"
          #rx"negative index"
          (lambda () (get-nth null -2)))

(test-equal? "every-other: even length"
             (every-other (list 1 2 3 4))
             (list 1 3))
(test-equal? "every-other: odd length"
             (every-other (list 1 2 3))
             (list 1 3))

(test-equal? "map: squaring"
             (map (lambda (x) (* x x)) (list 1 2 3))
             (list 1 4 9))

(test-equal? "map2: multiply"
             (map2 (lambda (x y) (* x y)) (list 1 2 3) (list 2 3 4))
             (list 2 6 12))

(test-equal? "filter: odd"
             (filter (lambda (x) (= (modulo x 2) 1))
                     (list 1 2 3 4))
             (list 1 3))

(test-equal? "call-all: one-element"
             (call-all (list (lambda () 2)))
             (list 2))