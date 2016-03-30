#lang racket
;; Programming Languages, Assignment 10 tests
;;
;; This file uses a basic testing mechanism similar to what we did with OCAML.
;; To test, simply run this file in DrRacket.

(require "assignment10sub.rkt")

;; bind
(displayln "bind tests")
(define sample-env1 (list (binding 'x (num 4)) (binding 'y (num 5))))
(define sample-env2 (list (binding 'x (num 4)) (binding 'x (num 5))))
(define sample-env3 (list (binding 'y (num 8)) (binding 'z (num 10)) (binding 'x (num 4)) (binding 't (num 5))))

(equal? (bind 'x (num 4) empty) (list (binding 'x (num 4))))
(equal? (bind 'x (num 4) (bind 'y (num 5) empty)) sample-env1)
(equal? (bind 'y (num 8) (bind 'z (num 10) (bind 'x (num 4) (bind 't (num 5) empty)))) sample-env3)

;; lookup
(displayln "lookup tests")
(with-handlers ([exn:fail? (lambda (exn) #t)])
  (lookup 'x empty)) 
(with-handlers ([exn:fail? (lambda (exn) #f)])
  (equal? (lookup 'x sample-env1) (num 4)))
(with-handlers ([exn:fail? (lambda (exn) #f)])
  (equal? (lookup 'y sample-env1) (num 5)))
(with-handlers ([exn:fail? (lambda (exn) #t)])
  (equal? (lookup 'z sample-env1) (num 4)))
(with-handlers ([exn:fail? (lambda (exn) #t)])
  (lookup 't empty)) 
(with-handlers ([exn:fail? (lambda (exn) #f)])
  (equal? (lookup 't sample-env3) (num 5)))
(with-handlers ([exn:fail? (lambda (exn) #f)])
  (equal? (lookup 'y sample-env3) (num 8)))
(with-handlers ([exn:fail? (lambda (exn) #t)])
  (equal? (lookup 'z sample-env3) (num 10)))
(with-handlers ([exn:fail? (lambda (exn) #t)])
  (lookup 'a sample-env3))


;; valid-program?
(displayln "valid-program? tests")
(define example1
  (call (fun #f 'x
             (if-e (isnul (var 'x))
                   (nul)
                   (arith '+ (fst (var 'x)) (snd (var 'x)))))
        (pair-e (num 5) (num 6))))
             
(valid-program? (num 5))
(not (valid-program? (num "f")))
(valid-program? example1)
(valid-program? (var 's))
(not (valid-program? (var "f")))
(not (valid-program? (var '5)))
(valid-program? (var 's))
(valid-program? (var 'f))
(not (valid-program? (var '"f'")))
(not (valid-program? (bool '5)))
(valid-program? (bool #f))
(valid-program? (bool #t))
(not (valid-program? (bool '"f'")))
(valid-program? (bool (valid-program? (var '"f"))))
(valid-program? (arith '+ (num 3) (num 4)))
(valid-program? (arith '- (var 'f) (var 'd)))
(not (valid-program? (arith 'd (num 3) (num 10))))
(not (valid-program? (arith '/ (arith '** (num 3) (num 4)) (num 3))))
(valid-program? (arith '* (arith '+ (arith '/ (var 'f) (bool (valid-program? (var '"f")))) (num 3)) (var 'd)))
(valid-program? (arith '+ (comp '< (arith '+ (num 3) (bool #f)) (comp '>= (num 3) (var 'd))) (num 10)))
(valid-program? (if-e (var 't) (bool #t) (num 5)))
(not (valid-program? (if-e (var '#f) (bool #f) (num 5))))
(not (valid-program? (if-e (var 'f) (bool 'f) (num 5))))
(not (valid-program? (if-e (var 'f) (bool #f) (num 'd5))))
(valid-program? (eq-e (var 't) (bool #f)))
(not (valid-program? (eq-e (var '#f) (num 5))))
(not (valid-program? (eq-e (var 'f) (bool 'f))))
(not (valid-program? (eq-e (bool #f) (num 'd5))))
(valid-program? (let-e 's (var 'd) (num 5)))
(not (valid-program? (let-e '#f (bool #f) (num 5))))
(not (valid-program? (let-e 'f (bool 'f) (num 5))))
(not (valid-program? (let-e 'f (bool #f) (num 'd5))))
(valid-program? (fun #f 'x
             (if-e (isnul (var 'x))
                   (nul)
                   (arith '+ (fst (var 'x)) (snd (var 'x))))))
(not (valid-program? (fun #f '#f (bool (valid-program? (num 5))))))
(not (valid-program? (fun 'd 'd (bool 'f))))
(not (valid-program? (fun #t 't (bool #f))))
(valid-program? (fun #f 't (fun 't 'd (fun #f 'd (num 3)))))
(valid-program? (call (fun #f 't (fun 't 'd (fun #f 'd (num 3))))
                      (num 3)))
(not (valid-program? (call (fun 'd 'd (bool 'f)) (num 3))))
(not (valid-program? (call (num 3) (fun #t 't (bool #f)))))
(valid-program? (nul))
(valid-program? (fun #f 't (nul)))
(valid-program? (call (nul) (nul)))
(valid-program? (arith '+ (nul) (nul)))
(valid-program? (isnul (var 'e)))
(not (valid-program? (isnul (var '#f))))
(valid-program? (isnul (nul)))
(valid-program? (isnul (if-e (var 's) (nul) (nul))))
(valid-program? (pair-e (var 'd) (nul)))
(valid-program? (pair-e (var'e) (bool #f)))
(valid-program? (fst (if-e (var 's) (nul) (nul))))
(valid-program? (fst (pair-e (var 'd) (nul))))
(valid-program? (snd (pair-e (var'e) (bool #f))))
(valid-program? (fst (if-e (var 's) (nul) (nul))))
(not (valid-program? (fst (pair-e (var '#f) (nul)))))
(valid-program? (snd (pair-e (var 'e) (bool '#f))))
(not (valid-program? (clos (var 'd) (pair-e (num 3) (nul)))))

;; value?
(displayln "value? tests")
(value? (num 5))
(value? (bool #t))
(not (value? (pair-e (arith '+ (num 2) (num 3)) (num 2))))
(value? (num 'd))
(value? (bool 'f))
(value? (pair-e (nul) (num 2)))
(value? (nul))

;; value-eq?
(displayln "value-eq? tests")
(value-eq? (num 5) (num 5))
(not (value-eq? (num 5) (num #t)))
(not (value-eq? (num 5) (bool 5)))
(value-eq? (num 0.0) (num 0.0))
(not (value-eq? (num 5) (nul)))
(not (value-eq? (nul) (bool #f)))
(value-eq? (bool #t) (bool #t))
(not (value-eq? (pair-e (nul) (bool #f)) (pair-e (nul) (bool #t))))
(not (value-eq? (pair-e (nul) (arith '+ (nul) (nul))) (pair-e (nul) (arith '+ (nul) (nul)))))
(not (value-eq? (var 'd) (var 'd)))
(value-eq? (nul) (nul))


;; interp / evaluate
(displayln "interp/evaluate tests")
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
(displayln "neq tests")
(with-handlers ([exn:fail? (lambda (exn) #f)])
  (equal? (evaluate (neq (arith '+ (num 2) (num 3))
                         (num 6)))
          (bool #t)))

;; or2
(displayln "or2 tests")
(with-handlers ([exn:fail? (lambda (exn) #f)])
  (equal? (evaluate (or2 (comp '> (num 2) (num 3))
                         (bool #t)))
          (bool #t)))


;; and2
(displayln "and2 tests")
(with-handlers ([exn:fail? (lambda (exn) #f)])
  (equal? (evaluate (and2 (comp '> (num 2) (num 3))
                         (bool #t)))
          (bool #f)))


;; or-e
(displayln "or-e tests")
(with-handlers ([exn:fail? (lambda (exn) #f)])
  (equal? (evaluate (or-e (comp '> (num 2) (num 3))
                          (bool #f)
                          (bool #t)))
          (bool #t)))


;; and-e
(displayln "and-e tests")
(with-handlers ([exn:fail? (lambda (exn) #f)])
  (equal? (evaluate (and-e (comp '> (num 2) (num 3))
                           (bool #f)
                           (bool #t)))
          (bool #f)))


;; let-e*
(displayln "let-e* tests")
(with-handlers ([exn:fail? (lambda (exn) #f)])
  (equal? (evaluate
           (let-e* (['x (num 2)]
                    ['y (arith '+ (var 'x) (num 4))])
                   (arith '+ (var 'x) (var 'y))))
          (num 8)))


;; plus / times
(displayln "plus/times tests")
;; Test commented out until you implement plus

;(equal? (evaluate (plus (num 2) (num 5) (num 3)))
;        (num 10))
;(equal? (evaluate (mult (num 2) (num 5) (num 3)))
;        (num 30))


;; minus
;(displayln "minus tests")
;(equal? (evaluate (minus (num 10) (num 5) (num 3)))
;        (num 2))


;; racketlist->sourcelist
(displayln "racketlist->sourcelist tests")
(equal? (racketlist->sourcelist (list (num 2) (num 5)))
        (pair-e (num 2) (pair-e (num 5) (nul))))

;; map-e
(displayln "map-e tests")
(with-handlers ([exn:fail? (lambda (exn) #f)])
  (equal? (evaluate
           (call (call map-e (fun #f 'x (plus2 (var 'x) (num 2))))
                 (pair-e (num 2) (pair-e (num 5) (nul)))))
          (pair-e (num 4) (pair-e (num 7) (nul)))))
         

