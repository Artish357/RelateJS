#lang racket
(require "evalo.rkt" "faster-miniKanren/mk.rkt" "js-structures.rkt")

(module+ test
  (require rackunit)
  (time (test-equal? "Get #1"
               (run* (res) (evalo (jget `((1 . 2) (3 . 4) (5 . 6)) 1) res))
               `(2))
  (test-equal? "Get #2"
               (run* (res) (evalo (jget `((1 . 2) (3 . 4) (5 . 6)) 3) res))
               `(4))
  (test-equal? "Get #3"
               (run* (res) (evalo (jget `((1 . 2) (3 . 4) (5 . 6)) 5) res))
               `(6))
  (test-equal? "Get, empty"
               (run* (res) (evalo (jget `() 1) res))
               `(undefined))
  (test-equal? "Get, not found"
               (run* (res) (evalo (jget `((1 . 2) (3 . 4) (5 . 6)) 4) res))
               `(undefined))
  (test-equal? "Update #1"
               (run* (res) (evalo (jset `((1 . 2) (3 . 4) (5 . 6)) 1 100) res))
               `(((1 . 100) (3 . 4) (5 . 6))))
  (test-equal? "Update #2"
               (run* (res) (evalo (jset `((1 . 2) (3 . 4) (5 . 6)) 3 100) res))
               `(((1 . 2) (3 . 100) (5 . 6))))
  (test-equal? "Update #3"
               (run* (res) (evalo (jset `((1 . 2) (3 . 4) (5 . 6)) 5 100) res))
               `(((1 . 2) (3 . 4) (5 . 100))))
  (test-equal? "Create"
               (run* (res) (evalo (jset `((1 . 2) (3 . 4) (5 . 6)) 0 100) res))
               `(((0 . 100) (1 . 2) (3 . 4) (5 . 6))))
  (test-equal? "Create, empty object"
               (run* (res) (evalo (jset `() 0 100) res))
               `(((0 . 100))))
  (test-equal? "Delete #1"
               (run* (res) (evalo (jdel `((1 . 2) (3 . 4) (5 . 6)) 1) res))
               `(((3 . 4) (5 . 6))))
  (test-equal? "Delete #2"
               (run* (res) (evalo (jdel `((1 . 2) (3 . 4) (5 . 6)) 3) res))
               `(((1 . 2) (5 . 6))))
  (test-equal? "Delete #3"
               (run* (res) (evalo (jdel `((1 . 2) (3 . 4) (5 . 6)) 5) res))
               `(((1 . 2) (3 . 4))))
  (test-equal? "Delete, not found"
               (run* (res) (evalo (jdel `((1 . 2) (3 . 4) (5 . 6)) 0) res))
               `(((1 . 2) (3 . 4) (5 . 6))))
  ))
