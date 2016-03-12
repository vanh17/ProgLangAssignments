#lang racket
;; Programming Languages, Assignment 10 tests
;;
;; This file uses a basic testing mechanism similar to what we did with OCAML.
;; To test, simply run this file in DrRacket.

(require "assignment10sub.rkt")

;; bind
(define sample-env1 (list (binding 'x (num 4)) (binding 'y (num 5))))
(define sample-env2 (list (binding 'x (num 4)) (binding 'x (num 5))))

(equal? (bind 'x (num 4) empty) (list (binding 'x (num 4))))
(equal? (bind 'x (num 4) (bind 'y (num 5) empty)) sample-env1)
(equal? (bind 'x (num 4) (bind 'x (num 5) empty)) sample-env2)

;; lookup
(with-handlers ([exn:fail? (lambda (exn) #t)])
  (lookup 'x empty)) 
(with-handlers ([exn:fail? (lambda (exn) #f)])
  (equal? (lookup 'x sample-env1) (num 4)))
(with-handlers ([exn:fail? (lambda (exn) #f)])
  (equal? (lookup 'y sample-env1) (num 5)))
(with-handlers ([exn:fail? (lambda (exn) #t)])
  (equal? (lookup 'z sample-env1) (num 4)))


;; valid-program?
(valid-program? (num 5))
(not (valid-program? (num "f")))

;; value?
(value? (num 5))
(value? (bool #t))
(not (value? (pair-e (arith '+ (num 2) (num 3)) (num 2))))

;; value-eq?
(value-eq? (num 5) (num 5))
(not (value-eq? (num 5) (bool #t)))
(not (value-eq? (num 5) (num 3.2)))

;; interp / evaluate
(equal? (evaluate (num 3))
        (num 3))
(equal? (evaluate (arith '* (num 3) (num 2)))
        (num 6))
;; We are using a try-catch form here because comp isn't
;; implemented yet. Your tests probably don't need to do that.
;; Similarly for subsequent uses of with-handlers.
(with-handlers ([exn:fail? (lambda (exn) #f)])
  (equal? (evaluate (comp '< (num 3) (num 2)))
          (bool #f)))


;; neq
(with-handlers ([exn:fail? (lambda (exn) #f)])
  (equal? (evaluate (neq (arith '+ (num 2) (num 3))
                         (num 5)))
          (bool #t)))

;; or2
(with-handlers ([exn:fail? (lambda (exn) #f)])
  (equal? (evaluate (or2 (comp '> (num 2) (num 3))
                         (bool #t)))
          (bool #t)))


;; and2
(with-handlers ([exn:fail? (lambda (exn) #f)])
  (equal? (evaluate (and2 (comp '> (num 2) (num 3))
                         (bool #t)))
          (bool #f)))


;; or-e
(with-handlers ([exn:fail? (lambda (exn) #f)])
  (equal? (evaluate (or-e (comp '> (num 2) (num 3))
                          (bool #f)
                          (bool #t)))
          (bool #t)))


;; and-e
(with-handlers ([exn:fail? (lambda (exn) #f)])
  (equal? (evaluate (and-e (comp '> (num 2) (num 3))
                           (bool #f)
                           (bool #t)))
          (bool #f)))


;; let-e*
(with-handlers ([exn:fail? (lambda (exn) #f)])
  (equal? (evaluate
           (let-e* (['x (num 2)]
                    ['y (arith '+ (var 'x) (num 4))])
                   (arith '+ (var 'x) (var 'y))))
          (num 8)))


;; plus / times
;; Test commented out until you implement plus

;(equal? (evaluate (plus (num 2) (num 5) (num 3)))
;        (num 10))


;; minus

;(equal? (evaluate (minus (num 10) (num 5) (num 3)))
;        (num 2))


;; racketlist->sourcelist
(equal? (racketlist->sourcelist (list (num 2) (num 5)))
        (pair-e (num 2) (pair-e (num 5) (nul))))

;; map-e
(with-handlers ([exn:fail? (lambda (exn) #f)])
  (equal? (evaluate
           (call (call map-e (fun #f 'x (plus2 (var 'x) (num 2))))
                 (pair-e (num 2) (pair-e (num 5) (nul)))))
          (pair-e (num 4) (pair-e (num 7) (nul)))))
         

