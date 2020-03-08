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
         (run* (pairs) (pull-varo-pairs `(begin (var a b c) (if () (var q) (var y))) pairs))
         '(((a . undefined) (b . undefined) (c . undefined) (q . undefined) (y . undefined))))
  (test= "Pull vars, some assigned"
         (run* (pairs) (pull-varo-pairs `(begin (var (a 1) b (c 2)) (if () (var (q 3)) (var y))) pairs))
         '(((a . 1) (b . undefined) (c . 2) (q . 3) (y . undefined))))
  (test= "Pull vars, nested functions"
         (run* (pairs) (pull-varo-pairs `(begin (var (a 1) b (c (function (x) (var should-not-pop-up)))) (if () (var (q 3)) (var y))) pairs))
         '(((a . 1) (b . undefined) (c . (function (x) (var should-not-pop-up))) (q . 3) (y . undefined))))
  (test= "Human interface functions"
         (humanize (dehumanize (list 1 "hello" 1337)))
         (list 1 "hello" 1337))
  )