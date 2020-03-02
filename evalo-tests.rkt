#lang racket
(require "evalo.rkt" "faster-miniKanren/mk.rkt" "js-structures.rkt")

(module+ test
  (require rackunit)
  (define-syntax-rule (test= name expr output)
    (test-equal? name
                 (begin (printf "Running: ~s\n" name)
                        (time expr))
                 output))
  (let ([1to6zip `((,(jnum 1) . ,(jnum 2)) (,(jnum 3) . ,(jnum 4)) (,(jnum 5) . ,(jnum 6)))]
        [1to3 (map jnum `(1 2 3))])
    (test= "Get #1"
           (run* (res) (evalo (jget (jobj 1to6zip) (jnum 1)) res))
           `(,(jnum 2)))
    (test= "Get #2"
           (run* (res) (evalo (jget (jobj 1to6zip) (jnum 3)) res))
           `(,(jnum 4)))
    (test= "Get #3"
           (run* (res) (evalo (jget (jobj 1to6zip) (jnum 5)) res))
           `(,(jnum 6)))
    (test= "Get, empty"
           (run* (res) (evalo (jget (jobj `()) (jnum 1)) res))
           `(,(jundef)))
    (test= "Get, not found"
           (run* (res) (evalo (jget (jobj 1to6zip) (jnum 4)) res))
           `(,(jundef)))
    (test= "Update #1"
           (run* (res) (evalo (jset (jobj 1to6zip) (jnum 1) (jnum 100)) res))
           `(,(jobj `((,(jnum 1) . ,(jnum 100)) . ,(cdr 1to6zip)))))
    (test= "Update #2"
           (run* (res) (evalo (jset (jobj 1to6zip) (jnum 3) (jnum 100)) res))
           `(,(jobj `((,(jnum 1) . ,(jnum 2)) (,(jnum 3) . ,(jnum 100)) (,(jnum 5) . ,(jnum 6))))))
    (test= "Update #3"
           (run* (res) (evalo (jset (jobj 1to6zip) (jnum 5) (jnum 100)) res))
           `(,(jobj `((,(jnum 1) . ,(jnum 2)) (,(jnum 3) . ,(jnum 4)) (,(jnum 5) . ,(jnum 100))))))
    (test= "Create"
           (run* (res) (evalo (jset (jobj 1to6zip) (jnum 0) (jnum 100)) res))
           `(,(jobj `((,(jnum 0) . ,(jnum 100)) . ,1to6zip))))
    (test= "Create, empty object"
           (run* (res) (evalo (jset (jobj `()) (jnum 0) (jnum 100)) res))
           `(,(jobj `((,(jnum 0) . ,(jnum 100))))))
    (test= "Delete #1"
           (run* (res) (evalo (jdel (jobj 1to6zip) (jnum 1)) res))
           `(,(jobj (cdr 1to6zip))))
    (test= "Delete #2"
           (run* (res) (evalo (jdel (jobj 1to6zip) (jnum 3)) res))
           `(,(jobj `((,(jnum 1) . ,(jnum 2)) (,(jnum 5) . ,(jnum 6))))))
    (test= "Delete #3"
           (run* (res) (evalo (jdel (jobj 1to6zip) (jnum 5)) res))
           `(,(jobj `((,(jnum 1) . ,(jnum 2)) (,(jnum 3) . ,(jnum 4))))))
    (test= "Delete, not found"
           (run* (res) (evalo (jdel (jobj 1to6zip) (jnum 0)) res))
           `(,(jobj 1to6zip)))
    (test= "Variable reference #1"
           (run* (res) (evalo-env (jvar `x) `((x . 1) (y . 2) (z . 3)) res `() `() `() `()))
           `(1))
    (test= "Variable reference #2"
           (run* (res) (evalo-env (jvar `y) `((x . 1) (y . 2) (z . 3)) res `() `() `() `()))
           `(2))
    (test= "Variable reference #3"
           (run* (res) (evalo-env (jvar `z) `((x . 1) (y . 2) (z . 3)) res `() `() `() `()))
           `(3))
    (test= "Function application, no parameters"
           (run* (res) (evalo (japp (jfun `() (jget (jobj `()) (jnum 0))) `()) res))
           `(,(jundef)))
    (test= "Function application, parameter #1"
           (run* (res) (evalo (japp (jfun `(x y z) (jvar `x)) 1to3) res))
           `(,(jnum 1)))
    (test= "Function application, parameter #2"
           (run* (res) (evalo (japp (jfun `(x y z) (jvar `y)) 1to3) res))
           `(,(jnum 2)))
    (test= "Function application, parameter #3"
           (run* (res) (evalo (japp (jfun `(x y z) (jvar `z)) 1to3) res))
           `(,(jnum 3)))
    (test= "Allocation"
           (run* (val store next-address) (evalo-env (jall (jnum 100)) `() val `() store `() next-address))
           `(( ,(jref `()) (,(jnum 100))  (()) )))
    (test= "Dereference"
           (run* (val store next-address) (evalo-env (jderef (jref `())) `() val `(,(jnum 123)) store `(()) next-address))
           `((,(jnum 123) (,(jnum 123))  (()) )))
    (test= "Assignment"
           (run* (val store next-address) (evalo-env (jass (jref `()) (jnum 5)) `() val `(,(jnum 0)) store `(()) next-address))
           `((,(jnum 5) (,(jnum 5))  (()) )))
    (test= "Combined memory functions"
           (run* (val store next-address) (evalo-env (jass (jall (jnum 66)) (jderef (jref `()))) `() val `(,(jnum 33)) store `(()) next-address))
           `((,(jnum 33) (,(jnum 33) ,(jnum 33))  ((())) )))
    (let ((testfunc (jfun `(x)
                          (jbeg
                           (jall (jvar `x))
                           (jif (jderef (jref `())) (jnum 1) (jnum 0))))))
      (test= "Control structures #1"
             (run* (val) (evalo (japp testfunc `(,(jbool #t))) val))
             `(,(jnum 1)))
      (test= "Control structures #2"
             (run* (val) (evalo (japp testfunc `(,(jbool #f))) val))
             `(,(jnum 0)))
      )
    (let ([breakval (jbrk `error `e)])
      (test= "Basic break"
             (run* (val) (evalo breakval val))
             `(,breakval))
      (test= "Catch, no break"
             (run* (val) (evalo (jcatch `error (jnum 0) `err-var (jvar `err-var)) val))
             `(,(jnum 0)))
      (test= "Basic catch"
             (run* (val) (evalo (jcatch `error breakval `err-var (jvar `err-var)) val))
             `(e))
      (test= "Basic catch, wrong label"
             (run* (val) (evalo (jcatch `loop-break breakval `err-var (jvar `err-var)) val))
             `(,breakval))
      )
    )
    (test= "typeof"
           (map (lambda (x) (run* (val) (evalo (jdelta `typeof `(,x)) val))) (list (jundef) (jnul) (jnum 42) (jstr "Hello") (jbool #f)))
           (map (lambda (x) `(,(jstr x))) (list "undefined" "object" "number" "string" "boolean")))
  (test= "+"
         (run* (res) (evalo (jdelta `+ `(,(jnum 20) ,(jnum 10))) res))
         `(,(jnum 30)))
  (test= "-"
         (run* (res) (evalo (jdelta `- `(,(jnum 20) ,(jnum 10))) res))
         `(,(jnum 10)))
  (test= "*"
         (run* (res) (evalo (jdelta `* `(,(jnum 20) ,(jnum 10))) res))
         `(,(jnum 200)))
  (test= "/"
         (run* (res) (evalo (jdelta `/ `(,(jnum 20) ,(jnum 10))) res))
         `(,(jnum 2)))
  (test= "< #1"
         (run* (res) (evalo (jdelta `< `(,(jnum 20) ,(jnum 10))) res))
         `(,(jbool #f)))
  (test= "< #2"
         (run* (res) (evalo (jdelta `< `(,(jnum 10) ,(jnum 20))) res))
         `(,(jbool #t)))
  (test= "string-+"
         (run* (res) (evalo (jdelta `string-+ `(,(jstr "Hello") ,(jstr "World"))) res))
         `(,(jstr "HelloWorld")))
  (test= "string-< #1"
         (run* (res) (evalo (jdelta `string-< `(,(jstr "Hell") ,(jstr "Hello"))) res))
         `(,(jbool #t)))
  (test= "string-< #2"
         (run* (res) (evalo (jdelta `string-< `(,(jstr "Helloo") ,(jstr "Hello"))) res))
         `(,(jbool #f)))
  (test= "string-< #3"
         (run* (res) (evalo (jdelta `string-< `(,(jstr "Helal") ,(jstr "Helbl"))) res))
         `(,(jbool #t)))
  (test= "string-< #4"
         (run* (res) (evalo (jdelta `string-< `(,(jstr "Helbl") ,(jstr "Helal"))) res))
         `(,(jbool #f)))
  (test= "string-< #5"
         (run* (res) (evalo (jdelta `string-< `(,(jstr "H") ,(jstr "H"))) res))
         `(,(jbool #f)))
  (test= "Combined delta test" ;; chr(ord('a')+2) -> 'c' 
         (run* (res) (evalo (jdelta `nat->char `(,(jdelta `+ `(,(jdelta `char->nat `(,(jstr "a"))) ,(jnum 2))))) res))
         `(,(jstr "c")))
  )
