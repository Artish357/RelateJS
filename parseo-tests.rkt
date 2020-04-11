#lang racket
(require "faster-miniKanren/mk.rkt" "js-structures.rkt" "parseo.rkt" "evalo.rkt")

(module+ test
  (require rackunit)
  (define-syntax-rule (test= name expr output)
    (test-equal? name
                 (begin (printf "Running: ~s\n" name)
                        (time expr))
                 output))
  (test= "Pull vars, all undefined"
         (run* (pairs) (pull-names-listo `(begin (var a b c) (if () (var q) (var y))) pairs))
         '((a b c q y)))
  (test= "Pull vars, some assigned"
         (run* (pairs) (pull-names-listo `(begin (var (a 1) b (c 2)) (if () (var (q 3)) (var y))) pairs))
         '((a b c q y)))
  (test= "Pull vars, nested functions"
         (run* (pairs) (pull-names-listo `(begin (var (a 1) b (c (function (x) (var should-not-pop-up)))) (if () (var (q 3)) (var y))) pairs))
         '((a b c q y)))
  (test= "Object creation"
         (map humanize (run* (c) (fresh (code) (parseo-h `(@ (object ("1" 1) ("2" 2)) "1") code) (evalo code c))))
         `(1))
  (test= "Object field setting"
         (map humanize (run* (r) (fresh (code) (parseo-h  `(begin (var (x (object))) (:= (@ x "3") 3)) code) (evalo code r))))
         '((object (("public" object (("3" . 3))) ("private" object ())))))
  (test= "Object field updating"
         (map humanize (run* (r) (fresh (code) (parseo-h`(begin (var (x (object ("3" 2)))) (:= (@ x "3") 3)) code) (evalo code r))))
         '((object (("public" object (("3" . 3))) ("private" object ())))))
  (test= "Human interface functions"
         (humanize (dehumanize (list 1 "hello" 1337)))
         (list 1 "hello" 1337))
  (let ([f! (lambda (x) `(begin (var (f! (function (x) (if (op === x 1) (return 1) (return (op * x (call f! (op - x 1)))))))) (call f! ,x)))])
    (test= "Factorial of 4" (run* (r) (fresh (code) (parseo-h (f! 4) code) (evalo code r))) (list (dehumanize 24)))
    (test= "Factorial of 5" (run* (r) (fresh (code) (parseo-h (f! 5) code) (evalo code r))) (list (dehumanize 120)))
    )
  )