#lang racket
(require "faster-miniKanren/mk.rkt" "evalo.rkt" "parseo.rkt" "js-structures.rkt")
(module+ test
  ;(run 1 (r) (fresh (code) (parseo-h r code) (evalo code r)))
  (require rackunit)
  (define-syntax-rule (test= name expr output)
    (test-equal? name
                 (begin (printf "Running: ~s\n" name)
                        (time expr))
                 output))
  (test= "4 quines"
         (length (run 4 (r) (fresh (code) (evalo code r) (parseo-nh r code))))
         4)
  (test= "5 ways to say hello"
         (length (map humanize (run 5 (out) (evalo out (jstr "Hello")))))
         5)
  (test= "1 way to code hello"
         (length (run 1 (out) (fresh (code) (evalo code (jstr "Hello")) (parseo-nh out code))))
         1)
  (test= "3+x=7"
         (run 1 (r) (evalo (jdelta `+ `(,(jnum 3) ,r)) (jnum 7)))
         `(,(jnum 4)))
  (test= "x+3=7"
         (run 1 (r) (evalo (jdelta `+ `(,r ,(jnum 3))) (jnum 7)))
         `(,(jnum 4)))
  (test= "2?2=4"
         (run 2 (r) (evalo (jdelta r `(,(jnum 2) ,(jnum 2))) (jnum 4)))
         `(+ *))
  (test= "Base case synthesis for fibonacci sequence"
         (run 1 (a b) (fresh (code code2 a^ b^)
                           (parseo-h `(begin (var (fib (function (x)
                                                      (switch x
                                                              (1 (return ,a))
                                                              (2 (return ,b)))
                                                      (return (op +
                                                                  (call fib (op - x 1))
                                                                  (call fib (op - x 2)))))))
                                            (call fib 3))
                                     code)
                           (parseo-h `(begin (var (fib (function (x)
                                                      (switch x
                                                              (1 (return ,a))
                                                              (2 (return ,b)))
                                                      (return (op +
                                                                  (call fib (op - x 1))
                                                                  (call fib (op - x 2)))))))
                                            (call fib 4))
                                     code2)
                           (== a (jrawnum a^))
                           (== b (jrawnum b^))
                           (evalo code (jnum 1))
                           (evalo code2 (jnum 2))))
         `((,(jnum 0) ,(jnum 1))))
  )
