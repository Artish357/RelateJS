#lang racket
(require "faster-miniKanren/mk.rkt" "js-structures.rkt" "parseo.rkt" "evalo.rkt" "helpers.rkt")

(module+ test
  (require rackunit)
  (define-syntax-rule (test= name expr output)
    (test-equal? name
                 (begin (printf "Running: ~s\n" name)
                        (time expr))
                 output))
  (test= "Hoist vars, all undefined"
         (run* (pairs) (hoist-varo `(begin (var a b c) (if () (var q) (var y))) pairs))
         '((a b c q y)))
  (test= "Hoist vars, some assigned"
         (run* (pairs) (hoist-varo `(begin (var (a 1) b (c 2)) (if () (var (q 3)) (var y))) pairs))
         '((a b c q y)))
  (test= "Hoist vars, nested functions"
         (run* (pairs) (hoist-varo `(begin (var (a 1) b (c (function (x) (var should-not-pop-up)))) (if () (var (q 3)) (var y))) pairs))
         '((a b c q y)))
  (test= "Object creation"
         (run* (r) (fresh (code i store)
                          (parseo/readable `(@ (object ("1" 1) ("2" 2)) "1") code)
                          (evalo code r store)))
         `(,(jnum 1)))
  (test= "Object field setting"
         (map humanize (run* (r) (fresh (code store i)
                                        (parseo/readable  `(call (function () (var (x (object))) (:= (@ x "3") 3) (return x))) code)
                                        (evalo code (jref i) store)
                                        (indexo store i r))))
         '((object (("private" object ()) ("public" object (("3" . 3)))))))
  (test= "Object field updating"
         (map humanize (run* (r) (fresh (code store) (parseo/readable  `(call (function () (var (x (object ("3" 2)))) (:= (@ x "3") 3) (return (@ x "3")))) code) (evalo code r store))))
         '(3))
  (test= "Human interface functions"
         (humanize (dehumanize (list 1 "hello" 1337)))
         (list 1 "hello" 1337))
  (let ([f! (lambda (x) `(call (function () (var (f! (function (x) (if (op === x 1) (return 1) (return (op * x (call f! (op - x 1)))))))) (return (call f! ,x)))))])
    (test= "Factorial of 4" (run* (r) (fresh (code store) (parseo/readable (f! 4) code) (evalo code r store))) (list (dehumanize 24)))
    (test= "Factorial of 5" (run* (r) (fresh (code store) (parseo/readable (f! 5) code) (evalo code r store))) (list (dehumanize 120)))
    )
  (test= "Range of 3"
         (run 1 (res) (fresh (func code i obj public temp store)
                                           (parseo/readable `(call (function ()
                                                                      (var (range (function (i acc)
                                                                                            (if (op === i 0)
                                                                                                (return acc)
                                                                                                (begin
                                                                                                  (:= (@ acc (op nat->char (op + 48 i))) i)
                                                                                                  (call range (op - i 1) acc)))))
                                                                           (temp (object)))
                                                                      (call range 2 temp)
                                                                      (if (op === (@ temp "1") 1)
                                                                          (if (op === (@ temp "2") 2)
                                                                              (return #t)
                                                                              #f)
                                                                          #f)
                                                                      (return temp)
                                                                      )) code)
                                           (evalo code res store)))
         `(,(jbool #t)))
  )
