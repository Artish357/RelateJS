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
  (test= "Puzzle"
         (run 5
           (BLANK)
           (fresh (code store)
             (parseo/readable
               `(call (function (x)
                                (return (call (function ()
                                                        (if #f    ; condition
                                                          ,BLANK  ; then-branch statement
                                                          #f)     ; else-branch statement
                                                        (return x)))))
                      42)
               code)
             (evalo code (jundef) store)))
         '(((var (x _.0)) (sym _.0))
           (var (x #t))
           (var (x #f))
           (var x)
           (var (x (op _.0)))))

  (test= "Fibonacci recursive (~125 milliseconds)"
         (run 1 (BLANK) (PBE (lambda (n)
                               `(call (function ()
                                                (var (fib (function (x)
                                                                    (if (op < x 2)
                                                                      (return x)
                                                                      (return (op +
                                                                                  (call fib (op - x 1))
                                                                                  (call fib (op - x 2))))))))
                                                (return (call fib ,n)))))
                             `(((2) ,(jnum 1))
                               ((5) ,(jnum 5)))))
         '(_.0))
  (test= "Fibonacci recursive, base case (~270 milliseconds)"
         (run 1 (BLANK) (PBE (lambda (n)
                               `(call (function ()
                                                (var (fib (function (x)
                                                                    (if (op < x 2)
                                                                      ,BLANK
                                                                      (return (op +
                                                                                  (call fib (op - x 1))
                                                                                  (call fib (op - x 2))))))))
                                                (return (call fib ,n)))))
                             `(((2) ,(jnum 1))
                               ((5) ,(jnum 5)))))
         '((return x)))
  (test= "Fibonacci recursive, first subtraction operation description (~68 seconds)"
         (run 1 (BLANK) (PBE (lambda (n)
                               `(call (function ()
                                                (var (fib (function (x)
                                                                    (if (op < x 2)
                                                                      (return x)
                                                                      (return (op +
                                                                                  (call fib (op . ,BLANK))
                                                                                  (call fib (op - x 2))))))))
                                                (return (call fib ,n)))))
                             `(((2) ,(jnum 1))
                               ((5) ,(jnum 5)))))
         '((- x (number (1)))))

  (test= "Range sum (~35 milliseconds)"
         (run 1 (_)
           (PBE (lambda (n)
                  `(call (function (n)
                                   (var (total 0))
                                   (for ((var (i 0)) (op < i n) (:= i (op + i 1)))
                                     (:= total (op + total i)))
                                   (return total))
                         ,n))
                `(((3) ,(jnum 3))
                  ((4) ,(jnum 6)))))
         '(_.0))
  (test= "Range sum, total declaration and initialization (~600 milliseconds)"
         (run 1 (BLANK)
           (PBE (lambda (n)
                  `(call (function (n)
                                   (var (total 0))
                                   (for ((var (i 0)) (op < i n) (:= i (op + i 1)))
                                     (:= total . ,BLANK))
                                   (return total))
                         ,n))
                `(((3) ,(jnum 3))
                  ((4) ,(jnum 6)))))
         '(((op + i total))))
  (test= "Range sum, assignment right-hand-side (~600 milliseconds)"
         (run 1 (BLANK)
           (PBE (lambda (n)
                  `(call (function (n)
                                   (var (total 0))
                                   (for ((var (i 0)) (op < i n) (:= i (op + i 1)))
                                     (:= total . ,BLANK))
                                   (return total))
                         ,n))
                `(((3) ,(jnum 3))
                  ((4) ,(jnum 6)))))
         '(((op + i total))))
  (test= "Range sum, entire loop body (~5 minutes)"
         (run 1 (BLANK)
           (PBE (lambda (n)
                  `(call (function (n)
                                   (var (total 0))
                                   (for ((var (i 0)) (op < i n) (:= i (op + i 1)))
                                     ,BLANK)
                                   (return total))
                         ,n))
                `(((3) ,(jnum 3))
                  ((4) ,(jnum 6)))))
         '((var (total (op + total i)))))
  )
