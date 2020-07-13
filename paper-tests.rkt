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

(define (PBE-evalo make-js examples)
  (foldl (lambda (example prev-goal)
           (define args            (car  example))
           (define expected-result (cadr example))
           (fresh (store)
                  prev-goal
                  (evalo (apply make-js args) expected-result store)))
         (== #t #t) examples))

(module+ test
  (require rackunit)
  (define-syntax-rule (test= name expr output)
    (test-equal? name
                 (begin (printf "Running: ~s\n" name)
                        (time expr))
                 output))

  (define (PBE/schema example)
    (PBE (lambda x
           `(call (function (obj schema)
                            (var (not (function (b) (if b (return #f) (return #t)))))
                            (while (call not (op === schema (null)))
                                   (var (field_spec (@ schema "car")))
                                   (var (field (@ obj (@ field_spec "name"))))
                                   (if (op === field (undefined))
                                     (return #f) #t)
                                   (if (call not (op === (op typeof field) (@ field_spec "type")))
                                     (return #f) #t)
                                   (var (min (@ field_spec "minimum"))
                                        (max (@ field_spec "maximum")))
                                   (if (call not (op === min (undefined)))
                                     (if (call not (op < min (op + field 1)))
                                       (return #f) #t)
                                     #t)
                                   (if (call not (op === max (undefined)))
                                     (if (call not (op < field (op + max 1)))
                                       (return #f) #t)
                                     #t)
                                   (:= schema (@ schema "cdr")))
                            (return #t))
                  . ,x))
         `(,example)))
(test= "Range sum i, no declaration, initialization ???"
        (run 1 (INIT BLANK)
            (PBE-evalo (lambda (n)
                `(app
                    (get
                        (get
                        (deref
                        (allocate
                        (set
                            (object ((,(jstr "public") object ())))
                            ,(jstr "private")
                            (set
                            (object ())
                            ,(jstr "call")
                            (fun
                            (n)
                            (catch
                            return
                            (begin
                                (let n (allocate (var n))
                                (let total (allocate (undefined))
                                    (let i (allocate (undefined))
                                    (begin
                                        (begin (assign (var total) ,(jnum 0)) (undefined))
                                        (begin
                                        (begin
                                            (begin ,BLANK (undefined))
                                            (catch
                                            break
                                            (while
                                            (delta < ((deref (var i)) (deref (var n))))
                                            (begin
                                                (assign
                                                (var total)
                                                (delta + ((deref (var total)) (deref (var i)))))
                                                (assign (var i) (delta + ((deref (var i)) ,(jnum 1))))))
                                            e
                                            (undefined)))
                                        (throw return (deref (var total))))))))
                                (undefined))
                            result
                            (var result)))))))
                        ,(jstr "private"))
                        ,(jstr "call"))
                    (,(jnum n))))
                `(((3) ,(jnum 3))
                ((4) ,(jnum 6)))))
        `(assign (var i) ,(jnum 0))
        )
  ;(test= "Schema validator as generator 1 (?)"
         ;(map humanize
              ;(run 1 (INPUT1 INPUT2)
                ;(PBE/schema
                  ;`(((object (,INPUT1 ,INPUT2)
                             ;("age" 14))
                     ;(object ("car" (object ("name" "name")
                                            ;("type" "string")))
                             ;("cdr" (object ("car" (object ("name"    "age")
                                                           ;("type"    "number")
                                                           ;("minimum" 14)
                                                           ;("maximum" 100)))
                                            ;("cdr" (null))))))
                    ;,(jbool #t)))))
         ;'(("name" (string _.0))))

  ;(test= "Schema validator as generator 2 (?)"
         ;(map humanize
              ;(run 1 (INPUT1 INPUT2)
                ;(PBE/schema
                  ;`(((object (,INPUT1 "Bob")
                             ;("age" ,INPUT2))
                     ;(object ("car" (object ("name" "name")
                                            ;("type" "string")))
                             ;("cdr" (object ("car" (object ("name"    "age")
                                                           ;("type"    "number")
                                                           ;("minimum" 14)
                                                           ;("maximum" 15)))
                                            ;("cdr" (null))))))
                    ;,(jbool #t)))))
         ;'(("name" 14)))

  ;(test= "Schema validator as generator 3 (?)"
         ;(map humanize
              ;(run 1 (INPUT1 INPUT2)
                ;(PBE/schema
                  ;`(((object ("name" "Robert")
                             ;("age" 25))
                     ;(object ("car" (object ("name" "name")
                                            ;("type" "string")))
                             ;("cdr" (object ("car" (object ("name"    "age")
                                                           ;("type"    ,INPUT1)
                                                           ;("minimum" ,INPUT2)
                                                           ;("maximum" 100)))
                                            ;("cdr" (null))))))
                    ;,(jbool #t)))))
         ;'(("number" 0)))

  (test= "Modulo operator as generator (24 seconds)"
         (map humanize
              (run 10 (INPUT)
                (PBE (lambda x
                       `(call (function (n m)
                                        (while (op < m n)
                                               (:= n (op - n m)))
                                        (return n))
                              . ,x))
                     `(((,INPUT 3) ,(jnum 1))
                       ((,INPUT 5) ,(jnum 2))))))

         '(7
           37
           22
           (op char->nat "\a")
           (op char->nat "%")
           (op char->nat "\u0016")
           67
           52
           (op char->nat "C")
           127))

   (test= "Puzzle (~150 milliseconds)"
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
   (test= "Fibonacci recursive, condition (~32 seconds)"
          (map humanize
               (run 1 (BLANK) (PBE (lambda (n)
                                     `(call (function ()
                                                      (var (fib (function (x)
                                                                          (if ,BLANK
                                                                            (return x)
                                                                            (return (op +
                                                                                        (call fib (op - x 1))
                                                                                        (call fib (op - x 2))))))))
                                                      (return (call fib ,n)))))
                                   `(((2) ,(jnum 1))
                                     ((5) ,(jnum 5))))))
          '((op < x 2)))
    (test= "Fibonacci recursive, condition, from LJS(?)"
        (humanize (run 1 (BLANK)
            (PBE-evalo (lambda (n) `(app
                (get
                    (get
                        (deref
                        (allocate
                        (set
                        (object ((,(jstr "public") object ())))
                        ,(jstr "private")
                        (set
                            (object ())
                            ,(jstr "call")
                            (fun
                            ()
                            (catch
                            return
                            (begin
                                (let fib (allocate (undefined))
                                (begin
                                    (begin
                                    (assign
                                    (var fib)
                                    (allocate
                                        (set
                                        (object ((,(jstr "public") object ())))
                                        ,(jstr "private")
                                        (set
                                        (object ())
                                        ,(jstr "call")
                                        (fun
                                        (x)
                                        (catch
                                            return
                                            (begin
                                            (let x (allocate (var x))
                                                (begin
                                                (if ,BLANK
                                                    (throw return (deref (var x)))
                                                    (throw return
                                                    (delta
                                                    +
                                                    ((app (get (get
                                                        (deref (deref (var fib)))
                                                        ,(jstr "private"))
                                                        ,(jstr "call"))
                                                        ((delta - ((deref (var x)) ,(jnum 1)))))
                                                    (app (get (get
                                                        (deref (deref (var fib)))
                                                        ,(jstr "private"))
                                                        ,(jstr "call"))
                                                        ((delta - ((deref (var x)) ,(jnum 2)))))))))
                                                (undefined)))
                                            (undefined))
                                            result
                                            (var result)))))))
                                    (undefined))
                                    (throw
                                    return
                                    (app
                                    (get (get (deref (deref (var fib))) ,(jstr "private")) ,(jstr "call"))
                                    (,(jnum n))))))
                                (undefined))
                            result
                            (var result)))))))
                        ,(jstr "private"))
                ,(jstr "call")) ()))
            `(((2) ,(jnum 1))
              ((5) ,(jnum 5)))
            ))) `(delta < ((deref (var x)) ,(jnum 2))))
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
    (test= "Fibonacci recursive, base case, from LJS(?)"
        (humanize (run 1 (BLANK)
            (PBE-evalo (lambda (n) `(app
                (get
                    (get
                        (deref
                        (allocate
                        (set
                        (object ((,(jstr "public") object ())))
                        ,(jstr "private")
                        (set
                            (object ())
                            ,(jstr "call")
                            (fun
                            ()
                            (catch
                            return
                            (begin
                                (let fib (allocate (undefined))
                                (begin
                                    (begin
                                    (assign
                                    (var fib)
                                    (allocate
                                        (set
                                        (object ((,(jstr "public") object ())))
                                        ,(jstr "private")
                                        (set
                                        (object ())
                                        ,(jstr "call")
                                        (fun
                                        (x)
                                        (catch
                                            return
                                            (begin
                                            (let x (allocate (var x))
                                                (begin
                                                (if (delta < ((deref (var x)) ,(jnum 2)))
                                                    ,BLANK
                                                    (throw return
                                                    (delta
                                                    +
                                                    ((app (get (get
                                                        (deref (deref (var fib)))
                                                        ,(jstr "private"))
                                                        ,(jstr "call"))
                                                        ((delta - ((deref (var x)) ,(jnum 1)))))
                                                    (app (get (get
                                                        (deref (deref (var fib)))
                                                        ,(jstr "private"))
                                                        ,(jstr "call"))
                                                        ((delta - ((deref (var x)) ,(jnum 2)))))))))
                                                (undefined)))
                                            (undefined))
                                            result
                                            (var result)))))))
                                    (undefined))
                                    (throw
                                    return
                                    (app
                                    (get (get (deref (deref (var fib))) ,(jstr "private")) ,(jstr "call"))
                                    (,(jnum n))))))
                                (undefined))
                            result
                            (var result)))))))
                        ,(jstr "private"))
                ,(jstr "call")) ()))
            `(((2) ,(jnum 1))
              ((5) ,(jnum 5)))
            ))) `(throw return (deref (var x))))
   (test= "Fibonacci recursive, first subtraction operation description (~68 seconds)"
          (map humanize
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
                                     ((5) ,(jnum 5))))))
          '((- x 1)))
    (test= "Fibonacci recursive, first subtraction operation description, from LJS(?)"
        (humanize (run 1 (BLANK)
            (PBE-evalo (lambda (n) `(app
                (get
                    (get
                        (deref
                        (allocate
                        (set
                        (object ((,(jstr "public") object ())))
                        ,(jstr "private")
                        (set
                            (object ())
                            ,(jstr "call")
                            (fun
                            ()
                            (catch
                            return
                            (begin
                                (let fib (allocate (undefined))
                                (begin
                                    (begin
                                    (assign
                                    (var fib)
                                    (allocate
                                        (set
                                        (object ((,(jstr "public") object ())))
                                        ,(jstr "private")
                                        (set
                                        (object ())
                                        ,(jstr "call")
                                        (fun
                                        (x)
                                        (catch
                                            return
                                            (begin
                                            (let x (allocate (var x))
                                                (begin
                                                (if (delta < ((deref (var x)) ,(jnum 2)))
                                                    (throw return (deref (var x)))
                                                    (throw return
                                                    (delta
                                                    +
                                                    ((app (get (get
                                                        (deref (deref (var fib)))
                                                        ,(jstr "private"))
                                                        ,(jstr "call"))
                                                        ((delta . ,BLANK)))
                                                    (app (get (get
                                                        (deref (deref (var fib)))
                                                        ,(jstr "private"))
                                                        ,(jstr "call"))
                                                        ((delta - ((deref (var x)) ,(jnum 2)))))))))
                                                (undefined)))
                                            (undefined))
                                            result
                                            (var result)))))))
                                    (undefined))
                                    (throw
                                    return
                                    (app
                                    (get (get (deref (deref (var fib))) ,(jstr "private")) ,(jstr "call"))
                                    (,(jnum n))))))
                                (undefined))
                            result
                            (var result)))))))
                        ,(jstr "private"))
                ,(jstr "call")) ()))
            `(((2) ,(jnum 1))
              ((5) ,(jnum 5)))
            ))) `(- ((deref (var x)) ,(jnum 1))))
   (test= "Fibonacci recursive, second subtraction operation description (~10 seconds)"
          (map humanize
               (run 1 (BLANK) (PBE (lambda (n)
                                     `(call (function ()
                                                      (var (fib (function (x)
                                                                          (if (op < x 2)
                                                                            (return x)
                                                                            (return (op +
                                                                                        (call fib (op - x 1))
                                                                                        (call fib (op . ,BLANK))))))))
                                                      (return (call fib ,n)))))
                                   `(((2) ,(jnum 1))
                                     ((5) ,(jnum 5))))))
          '((- x 2))
           )

    (test= "Fibonacci recursive, second subtraction operation description, from LJS(?)"
                (humanize (run 1 (BLANK)
            (PBE-evalo (lambda (n) `(app
                (get
                    (get
                        (deref
                        (allocate
                        (set
                        (object ((,(jstr "public") object ())))
                        ,(jstr "private")
                        (set
                            (object ())
                            ,(jstr "call")
                            (fun
                            ()
                            (catch
                            return
                            (begin
                                (let fib (allocate (undefined))
                                (begin
                                    (begin
                                    (assign
                                    (var fib)
                                    (allocate
                                        (set
                                        (object ((,(jstr "public") object ())))
                                        ,(jstr "private")
                                        (set
                                        (object ())
                                        ,(jstr "call")
                                        (fun
                                        (x)
                                        (catch
                                            return
                                            (begin
                                            (let x (allocate (var x))
                                                (begin
                                                (if (delta < ((deref (var x)) ,(jnum 2)))
                                                    (throw return (deref (var x)))
                                                    (throw return
                                                    (delta
                                                    +
                                                    ((app (get (get
                                                        (deref (deref (var fib)))
                                                        ,(jstr "private"))
                                                        ,(jstr "call"))
                                                        ((delta - ((deref (var x)) ,(jnum 1)))))
                                                    (app (get (get
                                                        (deref (deref (var fib)))
                                                        ,(jstr "private"))
                                                        ,(jstr "call"))
                                                        ((delta . ,BLANK)))))))
                                                (undefined)))
                                            (undefined))
                                            result
                                            (var result)))))))
                                    (undefined))
                                    (throw
                                    return
                                    (app
                                    (get (get (deref (deref (var fib))) ,(jstr "private")) ,(jstr "call"))
                                    (,(jnum n))))))
                                (undefined))
                            result
                            (var result)))))))
                        ,(jstr "private"))
                ,(jstr "call")) ()))
            `(((2) ,(jnum 1))
              ((5) ,(jnum 5)))
            ))) `(- ((deref (var x)) ,(jnum 2))))
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
   (test= "Range sum, total declaration and initialization (~390 milliseconds)"
          (run 1 (BLANK)
            (PBE (lambda (n)
                   `(call (function (n)
                                    ,BLANK
                                    (for ((var (i 0)) (op < i n) (:= i (op + i 1)))
                                      (:= total (op + total i)))
                                    (return total))
                          ,n))
                 `(((3) ,(jnum 3))
                   ((4) ,(jnum 6)))))
          '((var (total (number ())))))
   (test= "Range sum, i declaration and initialization (~160 milliseconds)"
          (run 1 (BLANK)
            (PBE (lambda (n)
                   `(call (function (n)
                                    (var (total 0))
                                    (for (,BLANK (op < i n) (:= i (op + i 1)))
                                      (:= total (op + total i)))
                                    (return total))
                          ,n))
                 `(((3) ,(jnum 3))
                   ((4) ,(jnum 6)))))
          '((var (i total))))
    (test= "Range sum i, no declaration, initialization ???"
        (run 1 (INIT BLANK)
            (PBE-evalo (lambda (n)
                `(app
                    (get
                        (get
                        (deref
                        (allocate
                        (set
                            (object ((,(jstr "public") object ())))
                            ,(jstr "private")
                            (set
                            (object ())
                            ,(jstr "call")
                            (fun
                            (n)
                            (catch
                            return
                            (begin
                                (let n (allocate (var n))
                                (let total (allocate (undefined))
                                    (let i (allocate (undefined))
                                    (begin
                                        (begin (assign (var total) ,(jnum 0)) (undefined))
                                        (begin
                                        (begin
                                            (begin ,BLANK (undefined))
                                            (catch
                                            break
                                            (while
                                            (delta < ((deref (var i)) (deref (var n))))
                                            (begin
                                                (assign
                                                (var total)
                                                (delta + ((deref (var total)) (deref (var i)))))
                                                (assign (var i) (delta + ((deref (var i)) ,(jnum 1))))))
                                            e
                                            (undefined)))
                                        (throw return (deref (var total))))))))
                                (undefined))
                            result
                            (var result)))))))
                        ,(jstr "private"))
                        ,(jstr "call"))
                    (,(jnum n))))
                `(((3) ,(jnum 3))
                ((4) ,(jnum 6)))))
        `(assign (var i) ,(jnum 0))
        )
   (test= "Range sum, end condition (~540 milliseconds)"
          (run 1 (BLANK)
            (PBE (lambda (n)
                   `(call (function (n)
                                    (var (total 0))
                                    (for ((var (i 0)) ,BLANK (:= i (op + i 1)))
                                      (:= total (op + total i)))
                                    (return total))
                          ,n))
                 `(((3) ,(jnum 3))
                   ((4) ,(jnum 6)))))
          '((op < i n)))
    (test= "Range sum i, end condition ???"
        (run 1 (INIT BLANK)
            (PBE-evalo (lambda (n)
                `(app
                    (get
                        (get
                        (deref
                        (allocate
                        (set
                            (object ((,(jstr "public") object ())))
                            ,(jstr "private")
                            (set
                            (object ())
                            ,(jstr "call")
                            (fun
                            (n)
                            (catch
                            return
                            (begin
                                (let n (allocate (var n))
                                (let total (allocate (undefined))
                                    (let i (allocate (undefined))
                                    (begin
                                        (begin (assign (var total) ,(jnum 0)) (undefined))
                                        (begin
                                        (begin
                                            (begin (assign (var i) ,(jnum 0)) (undefined))
                                            (catch
                                            break
                                            (while
                                            ,BLANK
                                            (begin
                                                (assign
                                                (var total)
                                                (delta + ((deref (var total)) (deref (var i)))))
                                                (assign (var i) (delta + ((deref (var i)) ,(jnum 1))))))
                                            e
                                            (undefined)))
                                        (throw return (deref (var total))))))))
                                (undefined))
                            result
                            (var result)))))))
                        ,(jstr "private"))
                        ,(jstr "call"))
                    (,(jnum n))))
                `(((3) ,(jnum 3))
                ((4) ,(jnum 6)))))
       `(delta < ((deref (var i)) (deref (var n))))
        )
;   #;(test= "Range sum, increment (?)"
;          (run 1 (BLANK)
;            (PBE (lambda (n)
;                   `(call (function (n)
;                                    (var (total 0))
;                                    (for ((var (i 0)) (op < i n) ,BLANK)
;                                      (:= total (op + total i)))
;                                    (return total))
;                          ,n))
;                 `(((3) ,(jnum 3))
;                   ((4) ,(jnum 6)))))
;          '((:= i (op + i 1))))
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
  )
