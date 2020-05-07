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
  (test= "Alpha equivalence 1"
         (run* (res)
           (fresh (code store)
             (parseo/readable
               '(call (function ()
                                (var (a 1) (b 2))
                                (return (call (function (b) (return b)) a))))
               code)
             (evalo code res store)))
         `(,(jnum 1)))
  (test= "Alpha equivalence 2"
         (run* (res)
           (fresh (code store)
             (parseo/readable
               '(call (function ()
                                (var (a 1) (b 2))
                                (return (call (function (a) (return a)) a))))
               code)
             (evalo code res store)))
         `(,(jnum 1)))
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
  (test= "While loop sum"
         (run* (res) (fresh (code store)
                            (parseo/readable
                             '(call (function ()
                                              (var (total 0) (i 0))
                                              (while (op < i 11)
                                                     (:= total (op + total i))
                                                     (:= i (op + i 1)))
                                              (return total)))
                             code)
                            (evalo code res store)))
         `(,(jnum 55)))
  (test= "For loop sum"
         (run* (res) (fresh (code store)
                            (parseo/readable
                             '(call (function ()
                                              (var (total 0))
                                              (for ((var (i 0)) (op < i 11) (:= i (op + i 1)))
                                                (:= total (op + total i)))
                                              (return total)))
                             code)
                            (evalo code res store)))
         `(,(jnum 55)))

  (test= "Object packing/unpacking 1"
         (run* (res)
           (fresh (code store)
             (parseo/readable
               '(call (function () (return (@ (object ("0" (op + 3 1))
                                                      ("1" (op + 4 1))) "0"))))
               code)
             (evalo code res store)))
         `(,(jnum 4)))

  (test= "Object packing/unpacking 2"
         (map humanize (run* (res)
                         (fresh (code store)
                           (parseo/readable
                             '(call (function ()
                                              (var (compute (function (n) (return (op + n 1))))
                                                   (obj (object ("0" (call compute 3))
                                                                ("1" (call compute 4)))))
                                              (return obj)))
                             code)
                           (evalo (jderef code) res store))))
         '((object
             (("private" object ())
              ("public" object (("0" . 4) ("1" . 5)))))))

  (test= "Object packing/unpacking 3"
         (run* (res)
           (fresh (code store)
             (parseo/readable
               '(call (function ()
                                (var (compute (function (n) (return (op + n 1)))))
                                (var (result (call compute 4)))
                                (return result)))
               code)
             (evalo code res store)))
         `(,(jnum 5)))
  (test= "Basic try/catch/finally"
         (run* (res)
                              (fresh (code store)
                                     (parseo/readable
                                      '(call
                                        (function (x)
                                                  (try 10 catch e 11 finally 13)
                                                  (return x))
                                        1024)
                                      code)
                                     (evalo code res store)
                                     ))
         `(,(jnum 1024))
         )
  (test= "Basic try/catch/finally with internal statements"
         (run* (res)
               (fresh (store code)
                      (parseo/readable
                       '(call
                         (function (x)
                                   (try (throw x)
                                        catch e
                                        (var (err (op + e 10)))
                                        finally
                                        (var (fin (op * 2 err))))
                                   (return fin))
                         1024)
                       code)
                      (evalo code res store)
                      ))
         `(,(jnum 2068))
         )
  )
