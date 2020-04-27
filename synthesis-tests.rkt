#lang racket
(require "faster-miniKanren/mk.rkt" "evalo.rkt" "parseo.rkt" "js-structures.rkt")
(module+ test
  (require rackunit)
  (define-syntax-rule (test= name expr output)
    (test-equal? name
                 (begin (printf "Running: ~s\n" name)
                        (time expr))
                 output))
  (test= "4 quines"
         (length (run 4 (r) (fresh (code store ) (evalo code r store) (parseo-nh r code))))
         4)
  (test= "5 ways to say hello"
         (length (map humanize (run 5 (out store) (evalo out (jstr "Hello") store))))
         5)
  (test= "1 way to code hello"
         (length (run 1 (out) (fresh (code store) (evalo code (jstr "Hello") store) (parseo-nh out code))))
         1)
  (test= "6 ways to break down hello"
         (length (run 5 (out) (fresh (code store) (parseo-nh `(op . ,out) code)  (evalo code (jstr "Hello") store))))
         5)
  (test= "Echo"
         (run 1 (out) (fresh (c1 c2 s1 s2)
                             (parseo-h `(call (function (x) ,out) 5) c1)
                             (evalo c1 (jnum 5) s1)
                             (parseo-h `(call (function (x) ,out) "Hello World!") c2)
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
  (test= "Base case synthesis for fibonacci sequence"
         (run 1 (a b) (fresh (inside code code2 s1 s2)
                             (parseo-h `(call (function () (var (fib (function (x)
                                                                               (switch x
                                                                                       (1 (return ,a))
                                                                                       (2 (return ,b)))
                                                                               (return (op +
                                                                                           (call fib (op - x 1))
                                                                                           (call fib (op - x 2)))))))
                                                        (return (call fib 3))))
                                       code)
                             (evalo code (jnum 1) s1)
                             (parseo-h `(call (function () (var (fib (function (x)
                                                                               (switch x
                                                                                       (1 (return ,a))
                                                                                       (2 (return ,b)))
                                                                               (return (op +
                                                                                           (call fib (op - x 1))
                                                                                           (call fib (op - x 2)))))))
                                                        (return (call fib 4))))
                                       code2)
                             (evalo code2 (jnum 2) s2)))
         `((,(jnum 0) ,(jnum 1))))
  (test= "if/var case"
         (length (run 1 (inside) (fresh (code store)
                                        (parseo-h `(call (function (x) (if #f ,inside (return x))) 3)
                                                  code)
                                        (evalo code (jundef) store)
                                        )))
         1)
;  (test= "Range of 3"
;         (map humanize (run 1 (res) (fresh (func code store i obj public)
;                             (parseo-h `(call (function ()
;                                                        (var (range (function (i)
;                                                                              (if (op === i 0)
;                                                                                  (return (object))
;                                                                                  (begin
;                                                                                    (var (res (call range (op - i 1))))
;                                                                                    (:= (@ res (op nat->char (op + 48 i))) i)
;                                                                                    (return res)
;                                                                                    )))))
;                                                        (return (call range 2)))) code)
;                             (evalo code (jref i) store)
;                             (indexo store i (jobj obj))
;                             (lookupo (jstr "public") obj (jobj `((,(jstr "2") . ,(jnum 2)) (,(jstr "1") . ,(jnum 1)))))
;                             )))
;         1)
  )
