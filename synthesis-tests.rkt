#lang racket
(require "faster-miniKanren/mk.rkt" "evalo.rkt" "parseo.rkt" "js-structures.rkt" "helpers.rkt")

(define (PBE make-js examples)
  (foldl (lambda (example prev-goal)
           (define args            (car  example))
           (define expected-result (cadr example))
           (fresh (code store)
             prev-goal
             (parseo/readable (apply make-js args) code)
             (evalo code expected-result store)))
         (== #t #t) examples))

(module+ test
  (require rackunit)
  (define-syntax-rule (test= name expr output)
    (test-equal? name
                 (begin (printf "Running: ~s\n" name)
                        (time expr))
                 output))
  (test= "4 quines"
         (length (run 4 (r) (fresh (code store ) (evalo code r store) (parse-topo r code))))
         4)
  (test= "5 ways to say hello"
         (length (map humanize (run 5 (out store) (evalo out (jstr "Hello") store))))
         5)
  #;(test= "3 ways to code hello"
         (length (run 3 (out) (fresh (code store) (parse-topo out code) (evalo code (jstr "Hello") store))))
         3)
  (test= "6 ways to break down hello"
         (length (run 5 (out) (fresh (code store) (parse-topo `(op . ,out) code)  (evalo code (jstr "Hello") store))))
         5)
  (test= "Echo"
         (run 1 (out) (fresh (c1 c2 s1 s2)
                             (parseo/readable `(call (function (x) ,out) 5) c1)
                             (evalo c1 (jnum 5) s1)
                             (parseo/readable `(call (function (x) ,out) "Hello World!") c2)
                             (evalo c2 (jstr "Hello World!") s2)
                             ))
         `((return x)))
  (test= "3+x=7"
         (run 1 (r) (fresh (store) (evalo (jdelta `+ `(,(jnum 3) ,r)) (jnum 7) store)))
         `(,(jnum 4)))
  (test= "x+3=7"
         (run 1 (r) (fresh (store) (evalo (jdelta `+ `(,r ,(jnum 3))) (jnum 7) store)))
         `(,(jnum 4)))
  (test= "2?2=4"
         (run 2 (r) (fresh (store) (evalo (jdelta r `(,(jnum 2) ,(jnum 2))) (jnum 4) store)))
         `(+ *))
  (test= "if/var case"
         (length (run 1 (inside) (fresh (code store)
                                        (parseo/readable `(call (function (x) (return (call (function () (if #f (var x) (return x)))))) 42)
                                                  code)
                                        (evalo code (jundef) store)
                                        )))
         1)
;    (test= "Range of 3"
;           (run 1 (res) (fresh (func code i obj public temp store)
;                                           (parseo/readable `(call (function ()
;                                                                      (var (range (function (i acc)
;                                                                                            (if (op === i 0)
;                                                                                                (return acc)
;                                                                                                (begin
;                                                                                                  (:= (@ acc (op nat->char ,res)) i)
;                                                                                                  (call range (op - i 1) acc)))))
;                                                                           (temp (object)))
;                                                                      (call range 2 temp)
;                                                                      (if (op === (@ temp "1") 1)
;                                                                          (if (op === (@ temp "2") 2)
;                                                                              (return #t)
;                                                                              #f)
;                                                                          #f)
;                                                                      (return temp)
;                                                                      )) code)
;                                           (evalo code (jbool #t) store)))
;           1)
  (test= "Base case synthesis for fibonacci sequence"
         (run 1 (base) (PBE (lambda (n)
                             `(call (function ()
                                              (var (fib (function (x)
                                                                  (if (op < x 2)
                                                                      ,base
                                                                      (return (op +
                                                                                  (call fib (op - x 1))
                                                                                  (call fib (op - x 2))))))))
                                              (return (call fib ,n)))))
                           `(((2) ,(jnum 1))
                             ((5) ,(jnum 5)))))
         '((return x)))

  (test= "For loop range sum (~550 milliseconds)"
         (run 1 (fragment)
           (PBE (lambda (n)
                  `(call (function ()
                                   (var (total 0))
                                   (for ((var (i 0)) (op < i ,n) (:= i (op + i 1)))
                                     (:= total . ,fragment))
                                   (return total))))
                `(((3) ,(jnum 3))
                  ((4) ,(jnum 6)))))
         '(((op + total i))))
#|
  (test= "For loop range sum (slow, ~27 seconds)"
         (run 1 (fragment)
           (PBE (lambda (n)
                  `(call (function ()
                                   (var (total 0))
                                   (for ((var (i 0)) (op < i ,n) (:= i (op + i 1)))
                                     (:= . ,fragment))
                                   (return total))))
                `(((3) ,(jnum 3))
                  ((4) ,(jnum 6)))))
         '((total (op + total i))))

  (test= "For loop range sum (very slow, ~12 minutes)"
         (run 1 (fragment)
           (PBE (lambda (n)
                  `(call (function ()
                                   (var (total 0))
                                   (for ((var (i 0)) (op < i ,n) (:= i (op + i 1)))
                                     ,fragment)
                                   (return total))))
                `(((3) ,(jnum 3))
                  ((4) ,(jnum 6)))))
         '((var (total (op + i total)))))
|#
  )
