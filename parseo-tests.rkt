#lang racket
(require "faster-miniKanren/mk.rkt" "js-structures.rkt" "parseo.rkt")

(module+ test
  (require rackunit)
  (define-syntax-rule (test= name expr output)
    (test-equal? name
                 (begin (printf "Running: ~s\n" name)
                        (time expr))
                 output))
  (test= "Pull vars, all undefined"
         (run* (pairs) (pull-var-nameso `(begin (var a b c) (if () (var q) (var y))) pairs))
         '((a b c q y)))
  (test= "Pull vars, some assigned"
         (run* (pairs) (pull-var-nameso `(begin (var (a 1) b (c 2)) (if () (var (q 3)) (var y))) pairs))
         '((a b c q y)))
  (test= "Pull vars, nested functions"
         (run* (pairs) (pull-var-nameso `(begin (var (a 1) b (c (function (x) (var should-not-pop-up)))) (if () (var (q 3)) (var y))) pairs))
         '((a b c q y)))
  (test= "Human interface functions"
         (humanize (dehumanize (list 1 "hello" 1337)))
         (list 1 "hello" 1337))
  )