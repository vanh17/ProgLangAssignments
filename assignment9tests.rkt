#lang racket
;; Programming Languages, Assignment 9 tests
;;
;; This file uses a basic testing mechanism similar to what we did with OCAML.
;; To test, simply run this file in DrRacket.

(require "assignment9sub.rkt")

;; add-nums
(equal? (add-nums (list)) 0) ;;empty list
(equal? (add-nums (list 1 2 'a 3)) 6) ;; non-number
(equal? (add-nums (list 'a 'b 'c)) 0) ;; all non-number
(equal? (add-nums (list 0 0 0)) 0) ;; all zero
(equal? (add-nums (list -1 0 1)) 0) ;; negative number
(equal? (add-nums (list (cons 1 2) (cons 3 4) (cons 'a 'b))) 0) ;;list of pair

;; length
(equal? (length (list)) 0) ;; empty list
(equal? (length (list 1 2 3 'a 'b 'c)) 6) ;; non-numbers and numbers
(equal? (length (list "abc" '3 '3' 4' 5)) 5) ;; string and other elements
(equal? (length (list (cons 1 2) (cons 3 4) (cons 4 5))) 3) ;; pair list

;; get-nth
(with-handlers ([exn:fail? (lambda (exn) (equal? (exn-message exn)
                                                 "negative index"))])
    (get-nth null -2))   ;;negative index
(with-handlers ([exn:fail? (lambda (exn) (equal? (exn-message exn)
                                                 "list too short"))])
    (get-nth null 2))   ;;out of bound
(with-handlers ([exn:fail? (lambda (exn) (equal? (exn-message exn)
                                                 "negative index"))])
    (get-nth (list 1 2 3) -10))   ;;negative index
(with-handlers ([exn:fail? (lambda (exn) (equal? (exn-message exn)
                                                 "list too short"))])
    (get-nth (list 1) 2))   ;;out of bound
(with-handlers ([exn:fail? (lambda (exn) (equal? (exn-message exn)
                                                 "negative index"))])
    (get-nth (list 1 2 3) -4))   ;;negative index
(with-handlers ([exn:fail? (lambda (exn) (equal? (exn-message exn)
                                                 "list too short"))])
    (get-nth (list 1 2 'a 'b) 10))   ;;out of bound
(with-handlers ([exn:fail? (lambda (exn) (equal? (exn-message exn)
                                                 "list too short"))])
    (get-nth (list 1 2 'a 'b) 4))   ;;out of bound
(equal? (get-nth (list 1 2 'a 'b) 3) 'b) ;;last element
(equal? (get-nth (list 1 2 'a 'b) 0) 1) ;;first element
(equal? (get-nth (list 1 2 'a 'b 3 4 5) 4) 3) ;;middle element

;; every-other
(equal? (every-other (list 1 2 3 4)) (list 1 3)) ;; even length
(equal? (every-other null) null)   ;;null
(equal? (every-other (list 1 2 3 4 5 6)) (list 1 3 5)) ;; even length
(equal? (every-other (list 1 3)) (list 1))   ;; two element
(equal? (every-other (list 'a)) (list 'a)) ;; one element
(equal? (every-other (list 1)) (list 1)) ;; one number

;; map
(equal? (map (lambda (x) (* x x)) (list 1 2 3))
     (list 1 4 9))       ;; squaring
(equal? (map (lambda (x) (* 2 x)) (list 1 2 3))
     (list 2 4 6))       ;; double
(equal? (map (lambda (x) null) (list 1 2 3))
     (list null null null))       ;; map to null
(equal? (map (lambda (x) (< 2 x)) (list 1 2 3))
     (list #f #f #t))       ;; filter
(equal? (map (lambda (x) (* x x)) (list 1))
     (list 1))       ;; squaring, one element
(equal? (map (lambda (x) (* 2 x)) (list 1))
     (list 2))       ;; double, one element
(equal? (map (lambda (x) null) (list 1))
     (list null))       ;; map to null, one element
(equal? (map (lambda (x) (< 2 x)) (list 1))
     (list #f))       ;; filter, one element
(equal? (map (lambda (x) (* x x)) null)
     null)       ;; squaring, null list 
(equal? (map (lambda (x) (* 2 x)) null)
     null)       ;; double, null list
(equal? (map (lambda (x) null) null)
     null)       ;; map to null, null list
(equal? (map (lambda (x) (< 2 x)) null)
     null)       ;; filter null list

;; map2
(equal? (map2 (lambda (x y) (* x y)) (list 1 2 3) (list 2 3 4))
     (list 2 6 12))      ;; multiply
(equal? (map2 (lambda (x y) (* x y)) (list 1 2 3 4) (list 2 3 4))
     (list 2 6 12))      ;; multiply 1st list longer
(equal? (map2 (lambda (x y) (* x y)) (list 1 2 3) (list 2 3 4 5 6))
     (list 2 6 12))      ;; multiply 2nd list longer
(equal? (map2 (lambda (x y) (* x y)) null (list 2 3 4 5 6))
     null)      ;; multiply 2nd list longer
(equal? (map2 (lambda (x y) (* x y)) (list 1 2 3) null)
     null)      ;; multiply 1st list longer
(equal? (map2 (lambda (x y) (* x y)) (list 1 2) (list 2 3 4 5 6))
     (list 2 6))      ;; multiply 2nd list longer



;; filter
(equal? (filter (lambda (x) (= (modulo x 2) 1))
               (list 1 2 3 4))
     (list 1 3))      ;; odd

;; call-all
(equal? (call-all (list (lambda () 2)))
     (list 2))        ;; one-element
