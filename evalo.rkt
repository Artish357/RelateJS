#lang racket
(require "faster-miniKanren/mk.rkt" "js-structures.rkt" "faster-miniKanren/numbers.rkt" "helpers.rkt")
(provide evalo eval-envo)

;; Entry point for the LambdaJS interpreter
(define (evalo expr val store)
  (fresh (next-address^)
         (eval-envo expr `() val `() store `() next-address^)))

;; LambdaJS interpreter with store
(define (eval-envo expr env value
                   store store~                ; mutable store before/after
                   next-address next-address~) ; index counter for the store
  (conde
    ;; Atomic values (Section 2.2.1)
    ((== expr value)
     (== store store~)
     (== next-address next-address~)
     (conde ((fresh (payload) (== expr `(number ,payload))))
            ((fresh (binds) (== expr `(object ,binds))))
            ((fresh (b) (== expr (jbool b))))
            ((fresh (payload) (== expr `(string ,payload))))
            ((== expr (jundef)))
            ((== expr (jnul)))))
    ;; Let expressions (Section 2.2.2)
    ((fresh (lhs-var          ; variable being bound
             rhs-expr rhs-val ; right-hand side expression & value
             store^ next-address^ ; store produced by evaluating the rhs
             body
             let-env)         ; environment after the let binding
       (== expr (jlet lhs-var rhs-expr body))
       (eval-envo rhs-expr env rhs-val store store^ next-address next-address^)
       (effect-propagateo rhs-val value store^ store~ next-address^ next-address~
         (== let-env `((,lhs-var . ,rhs-val) . ,env))
         (eval-envo body let-env value store^ store~ next-address^ next-address~))))
    ;; Immutable variable lookup (Section 2.2.2)
    ((fresh (var) ;; Look up a variable
       (== expr (jvar var))
       (== store store~)
       (== next-address next-address~)
       (lookupo var env value)))
    ;; Function definition (Section 2.2.4)
    ((fresh (body params)
       (== expr (jfun params body))
       (== value (jclo params body env))
       (== store store~)
       (== next-address next-address~)))
    ;; Function application (Section 2.2.4)
    ((fresh (func params rands args body
             param-arg-bindings
             cenv cenv^ ; closure environment before/after adding param-arg-bindings
             value^ store^ next-address^)
       (== expr (japp func rands))
       (eval-env-listo `(,func . ,rands) env value^ store store^ next-address next-address^)
       (effect-propagateo value^ value store^ store~ next-address^ next-address~
         (== value^ (value-list `(,(jclo params body cenv) . ,args)))
         (zipo param-arg-bindings params args)
         (appendo param-arg-bindings cenv cenv^)
         (eval-envo body cenv^ value store^ store~ next-address^ next-address~))))
    ;; Object field retrieval (2.2.4)
    ((fresh (obj-expr obj-bindings key-expr key-val value^)
       (== expr (jget obj-expr key-expr))
       (eval-env-listo `(,key-expr ,obj-expr) env value^ store store~ next-address next-address~)
       (effect-propagateo value^ value store~ store~ next-address~ next-address~
         (== value^ (value-list `(,key-val ,(jobj obj-bindings))))
         (typeofo key-val (jstr "string") store~)
         (conde ((membero `(,key-val . ,value) obj-bindings)) ; found
                ((== value (jundef))                          ; not found
                 (absent-keyso key-val obj-bindings))))))
    ;; Object field create/update (2.2.4)
    ((fresh (obj-expr obj-bindings obj-bindings^
             key-expr key-val rhs-expr rhs-val value^)
       (== expr (jset obj-expr key-expr rhs-expr))
       (eval-env-listo `(,obj-expr ,key-expr,rhs-expr) env value^ store store~ next-address next-address~)
       (effect-propagateo value^ value store~ store~ next-address~ next-address~
         (== value^ (value-list `(,(jobj obj-bindings) ,key-val ,rhs-val)))
         (== value (jobj obj-bindings^))
         (typeofo key-val (jstr "string") store~)
         (updateo obj-bindings key-val rhs-val obj-bindings^))))
    ;; Object field delete (Section 2.2.4)
    ((fresh (obj-expr obj-bindings obj-bindings^
             key-expr key-val value^)
       (== expr (jdel obj-expr key-expr))
       (eval-env-listo `(,obj-expr ,key-expr) env value^ store store~ next-address next-address~)
       (effect-propagateo value^ value store~ store~ next-address~ next-address~
         (== value^ (value-list `(,(jobj obj-bindings) ,key-val)))
         (== value (jobj obj-bindings^))
         (typeofo key-val (jstr "string") store~)
         (deleteo obj-bindings key-val obj-bindings^))))
    ;; Mutable references: memory allocation (Section 2.2.3)
    ((fresh (stored-expr stored-val next-address^ store^)
       (== expr (jall stored-expr))
       (eval-envo stored-expr env stored-val store store^ next-address next-address^)
       (effect-propagateo stored-val value store^ store~ next-address^ next-address~
         (== value (jref next-address^))
         (appendo store^ `(,stored-val) store~)
         (incremento next-address^ next-address~))))
    ;; Mutable references: dereferencing (Section 2.2.3)
    ((fresh (addr-expr addr-val value^)
       (== expr (jderef addr-expr))
       (eval-envo addr-expr env value^ store store~ next-address next-address~)
       (effect-propagateo value^ value store~ store~ next-address~ next-address~
         (== value^ (jref addr-val))
         (indexo store~ addr-val value))))
    ;; Mutable references: assignment (Section 2.2.3)
    ((fresh (addr-expr addr-val stored-expr stored-val store^ value^)
       (== expr (jassign addr-expr stored-expr))
       (eval-env-listo `(,addr-expr ,stored-expr) env value^ store store^ next-address next-address~)
       (effect-propagateo value^ value store^ store~ next-address~ next-address~
         (== value^ (value-list `(,(jref addr-val) ,stored-val)))
         (== value stored-val)
         (set-indexo store^ addr-val stored-val store~))))
    ;; Begin expression (Section 2.2.5)
    ((fresh (first-expr second-expr first-val value^)
       (== expr (jbeg first-expr second-expr))
       (eval-env-listo `(,first-expr ,second-expr) env value^ store store~ next-address next-address~)
       (effect-propagateo value^ value store~ store~ next-address~ next-address~
         (== value^ (value-list `(,first-val ,value))))))
    ;; If expression (Section 2.2.5)
    ((fresh (cond-expr cond-val then-expr else-expr store^ next-address^)
       (== expr (jif cond-expr then-expr else-expr))
       (eval-envo cond-expr env cond-val store store^ next-address next-address^)
       (effect-propagateo cond-val value store^ store~ next-address^ next-address~
         (conde ((== cond-val (jbool #f))
                 (eval-envo else-expr env value store^ store~ next-address^ next-address~))
                ((== cond-val (jbool #t))
                 (eval-envo then-expr env value store^ store~ next-address^ next-address~))))))
    ;; While expression (Section 2.2.5)
    ((fresh (cond-expr cond-val body-expr store^ next-address^)
       (== expr (jwhile cond-expr body-expr))
       (eval-envo cond-expr env cond-val store store^ next-address next-address^)
       (effect-propagateo cond-val value store^ store~ next-address^ next-address~
         (conde ((== cond-val (jbool #f))
                 (== value (jundef))
                 (== store^ store~)
                 (== next-address^ next-address~))
                ((== cond-val (jbool #t))
                 (eval-envo (jbeg body-expr (jwhile cond-expr body-expr)) ; next step of while loop
                            env value store^ store~ next-address^ next-address~))))))
    ;; Try/finally expression (Section 2.2.5)
    ((fresh (try-expr try-val finally-expr finally-val store^ next-address^)
       (== expr (jfin try-expr finally-expr))
       (eval-envo try-expr env try-val store store^ next-address next-address^)
       (eval-envo finally-expr env finally-val store^ store~ next-address^ next-address~)
       (effect-propagateo finally-val value store~ store~ next-address~ next-address~
         (== value try-val))))
    ;; Try/catch expression (Section 2.2.5)
    ((fresh (try-expr try-val try-val-tag try-val-payload
             catch-label catch-var catch-expr
             break-label break-val
             store^ next-address^ env^)
       (== expr (jcatch catch-label try-expr catch-var catch-expr))
       (eval-envo try-expr env try-val store store^ next-address next-address^)
       (conde ((== try-val (jbrk break-label break-val)) ;; Break does not match label
               (== `(,value ,store~ ,next-address~) `(,try-val ,store^ ,next-address^))
               (=/= break-label catch-label))
              ((== try-val `(,try-val-tag . ,try-val-payload)) ;; No break was caught
               (== `(,value ,store~ ,next-address~) `(,try-val ,store^ ,next-address^))
               (=/= try-val-tag `break))
              ((== try-val (jbrk catch-label break-val)) ;; Exception was caught
               (appendo `((,catch-var . ,break-val)) env env^)
               (eval-envo catch-expr env^ value store^ store~ next-address^ next-address~)))))
    ;; Throw expressions (Section 2.2.5)
    ((fresh (label thrown-expr thrown-val) ;; Throw
       (== expr (jthrow label thrown-expr))
       (eval-envo thrown-expr env thrown-val store store~ next-address next-address~)
       (effect-propagateo thrown-val value store~ store~ next-address~ next-address~
         (== value (jbrk label thrown-val)))))
    ;; Builtin operations (Section 3.2.5)
    ((fresh (rator rands args value^)
       (== expr (jdelta rator rands))
       (eval-env-listo rands env value^ store store~ next-address next-address~)
       (effect-propagateo value^ value store~ store~ next-address~ next-address~
         (== value^ (value-list args))
         (conde
           ;; typeof
           ((fresh (v1)
              (== `(,rator (,v1)) `(typeof ,args))
              (typeofo v1 value store~)))
           ;; char->nat
           ((fresh (str char)
              (== `(,str) args)
              (typeofo str (jstr "string") store~)
              (== `(,(jrawstr `(,char))) args)
              (== rator `char->nat)
              (== value (jrawnum char))))
           ;; nat->char
           ((fresh (num digits)
              (== `(,num) args) ;; Nat -> char
              (typeofo num (jstr "number") store~)
              (== `(,(jrawnum digits)) args)
              (== rator `nat->char)
              (== value (jrawstr `(,digits)))))
           ;; ===
           ((fresh (v1 v2)
              (== `(,rator ,args) `(=== (,v1 ,v2)))
              (conde ((== value (jbool #t)) (== v1 v2))
                     ((== value (jbool #f)) (=/= v1 v2)))))
           ;; numeric operations (+, -, *, /, <)
           ((fresh (v1 v2 digits1 digits2 result remainder)
              (== `(,v1 ,v2) args) ;; Number operations
              (typeofo v1 (jstr "number") store~)
              (typeofo v2 (jstr "number") store~)
              (== `(,(jrawnum digits1) ,(jrawnum digits2)) args)
              (conde ((== rator `+)
                      (== value (jrawnum result))
                      (pluso digits1 digits2 result))
                     ((== rator `-)
                      (== value (jrawnum result))
                      (minuso digits1 digits2 result))
                     ((== rator `*)
                      (== value (jrawnum result))
                      (*o digits1 digits2 result))
                     ((== rator `/)
                      (== value (jrawnum result))
                      (/o digits1 digits2 result remainder))
                     ((== rator `<)
                      (conde ((== value (jbool #t)) (<o digits1 digits2))
                             ((== value (jbool #f)) (<=o digits2 digits1)))))))
           ;; string operations (string-+, string-<)
           ((fresh (v1 v2 chars1 chars2 result)
              (== `(,v1 ,v2) args) ;; String operations
              (typeofo v1 (jstr "string") store~)
              (typeofo v2 (jstr "string") store~)
              (== `(,(jrawstr chars1) ,(jrawstr chars2)) args)
              (conde ((== rator `string-+)
                      (== value (jrawstr result))
                      (appendo chars1 chars2 result))
                     ((== rator `string-<)
                      (string-lesso chars1 chars2 value)))))))))))

;; Sequentially evaluate a list of LambdaJS expressions
(define (eval-env-listo exprs env vals store store~ next-address next-address~)
  (conde
    ; base case: empty list of expressions
    ((== exprs `())
     (== vals (value-list `()))
     (== store store~)
     (== next-address next-address~))
    ; non-empty list of expressions
    ((fresh (expr expr-rest val val-rest val-rest-payload store^ next-address^)
       (== exprs `(,expr . ,expr-rest))
       (eval-envo expr env val store store^ next-address next-address^)
       (effect-propagateo val vals store^ store~ next-address^ next-address~
         (eval-env-listo expr-rest env val-rest store^ store~ next-address^ next-address~)
         (effect-propagateo val-rest vals store~ store~ next-address~ next-address~
           (== val-rest (value-list val-rest-payload))
           (== vals (value-list `(,val . ,val-rest-payload)))))))))

; For propagating control effects (Section 2.2.6)
(define-syntax-rule (effect-propagateo value^        value~
                                       store^        store~
                                       next-address^ next-address~
                                       cont ...)
  (fresh (label bval tag rest)
    (conde ((== (jbrk label bval) value^)
            (== `(,value~ ,store~ ,next-address~)
                `(,value^ ,store^ ,next-address^)))
           ((== `(,tag . ,rest) value^)
            (=/= tag `break)
            cont ...))))

(define (typeofo value type store)
  (fresh (temp)
    (conde ((== value (jundef))
            (== type (jstr "undefined")))
           ((== value (jnul))
            (== type (jstr "object")))
           ((== value `(string . ,temp))
            (== type (jstr "string")))
           ((== value  `(number . ,temp))
            (== type (jstr "number")))
           ((== value  `(boolean . ,temp))
            (== type (jstr "boolean")))
           ((fresh (fields priv call)
              (== value (jref temp))
              (conde ((== type (jstr "object"))
                      (indexo store temp `(object ,fields))
                      (lookupo (jstr "private") fields (jobj priv))
                      (absent-keyso (jstr "call") priv))
                     ((== type (jstr "function"))
                      (indexo store temp `(object ,fields))
                      (lookupo (jstr "private") fields (jobj priv))
                      (lookupo (jstr "call") priv call))))))))

(define (string-lesso s1 s2 value)
  (fresh (x x^ y y^ rest)
    (conde ((== s1 `())
            (== s2 `(,x . ,rest))
            (== value (jbool #t)))
           ((== s2 `())
            (== value (jbool #f)))
           ((== s1 `(,x . ,x^))
            (== s2 `(,x . ,y^))
            (string-lesso x^ y^ value))
           ((== s1 `(,x . ,x^))
            (== s2 `(,y . ,y^))
            (== value (jbool #t))
            (=/= x y)
            (<o x y))
           ((== s1 `(,x . ,x^))
            (== s2 `(,y . ,y^))
            (== value (jbool #f))
            (=/= x y)
            (<=o y x)))))

(define (value-list values)
  (cons `value-list values))

