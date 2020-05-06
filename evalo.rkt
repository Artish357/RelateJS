#lang racket
(require "faster-miniKanren/mk.rkt" "js-structures.rkt" "faster-miniKanren/numbers.rkt" "helpers.rkt")
(provide evalo eval-envo)

;; Entry point for the LambdaJS interpreter
(define (evalo exp val store)
  (fresh (next-address^)
         (eval-envo exp `() val `() store `() next-address^)))

;; LambdaJS interpreter with store
(define (eval-envo exp env value
                   store store~                ; mutable store before/after
                   next-address next-address~) ; index counter for the store
  (conde
    ;; Atomic values (Section 2.2.1)
    ((== exp value)
     (== store store~)
     (== next-address next-address~)
     (conde ((jnumbero exp)) ((objecto exp)) ((boolo exp)) ((jstro exp))
            ((== exp (jundef))) ((== exp (jnul)))))
    ;; Let expressions (Section 2.2.2)
    ((fresh (lhs-var          ; variable being bound
             rhs-expr rhs-val ; right-hand side expression & value
             store^ next-address^ ; store produced by evaluating the rhs
             body
             let-env)         ; environment after the let binding
       (== exp (jlet lhs-var rhs-expr body))
       (eval-envo rhs-expr env rhs-val store store^ next-address next-address^)
       (effect-propagateo rhs-val value store^ store~ next-address^ next-address~
         (== let-env `((,lhs-var . ,rhs-val) . ,env))
         (eval-envo body let-env value store^ store~ next-address^ next-address~))))
    ;; Immutable variable lookup (Section 2.2.2)
    ((fresh (var) ;; Look up a variable
       (== exp (jvar var))
       (== store store~)
       (== next-address next-address~)
       (lookupo var env value)))
    ;; Function definition (Section 2.2.4)
    ((fresh (body params)
       (== exp (jfun params body))
       (== value (jclo params body env))
       (== store store~)
       (== next-address next-address~)))
    ;; Function application (Section 2.2.4)
    ((fresh (func params rands args body
             param-arg-bindings
             cenv cenv^ ; closure environment before/after adding param-arg-bindings
             value^ store^ next-address^)
       (== exp (japp func rands))
       (eval-env-listo `(,func . ,rands) env value^ store store^ next-address next-address^)
       (effect-propagateo value^ value store^ store~ next-address^ next-address~
         (== value^ (value-list `(,(jclo params body cenv) . ,args)))
         (zipo param-arg-bindings params args)
         (appendo param-arg-bindings cenv cenv^)
         (eval-envo body cenv^ value store^ store~ next-address^ next-address~))))
    ;; Object field retrieval (2.2.4)
    ((fresh (obj-exp obj-bindings key-expr key-val value^)
       (== exp (jget obj-exp key-expr))
       (eval-env-listo `(,key-expr ,obj-exp) env value^ store store~ next-address next-address~)
       (effect-propagateo value^ value store~ store~ next-address~ next-address~
         (== value^ (value-list `(,key-val ,(jobj obj-bindings))))
         (typeofo key-val (jstr "string") store~)
         (conde ((membero `(,key-val . ,value) obj-bindings)) ; found
                ((== value (jundef))                          ; not found
                 (absent-keyso key-val obj-bindings))))))
    ;; Object field create/update (2.2.4)
    ((fresh (obj-exp obj-bindings obj-bindings^
             key-expr key-val rhs-expr rhs-val value^)
       (== exp (jset obj-exp key-expr rhs-expr))
       (eval-env-listo `(,obj-exp ,key-expr,rhs-expr) env value^ store store~ next-address next-address~)
       (effect-propagateo value^ value store~ store~ next-address~ next-address~
         (== value^ (value-list `(,(jobj obj-bindings) ,key-val ,rhs-val)))
         (== value (jobj obj-bindings^))
         (typeofo key-val (jstr "string") store~)
         (updateo obj-bindings key-val rhs-val obj-bindings^))))
    ;; Object field delete (Section 2.2.4)
    ((fresh (obj-exp obj-bindings obj-bindings^
             key-expr key-val value^)
       (== exp (jdel obj-exp key-expr))
       (eval-env-listo `(,obj-exp ,key-expr) env value^ store store~ next-address next-address~)
       (effect-propagateo value^ value store~ store~ next-address~ next-address~
         (== value^ (value-list `(,(jobj obj-bindings) ,key-val)))
         (== value (jobj obj-bindings^))
         (typeofo key-val (jstr "string") store~)
         (deleteo obj-bindings key-val obj-bindings^))))
    ;; Mutable references: memory allocation (Section 2.2.3)
    ((fresh (store-value store-value^ next-address^ store^)
                 (== exp (jall store-value))
                 (eval-envo store-value env store-value^ store store^ next-address next-address^)
                 (effect-propagateo store-value^ value
                                    store^ store~
                                    next-address^ next-address~
                                    (== value (jref next-address^))
                                    (appendo store^ `(,store-value^) store~)
                                    (incremento next-address^ next-address~))))
         ((fresh (addr-exp addr value^) ;; Fetch from memory
                 (== exp (jderef addr-exp))
                 (eval-envo addr-exp env value^ store store~ next-address next-address~)
                 (effect-propagateo value^ value
                                    store~ store~
                                    next-address~ next-address~
                                    (== value^ (jref addr))
                                    (indexo store~ addr value))))
         ((fresh (addr-exp val addr val^ store^ value^) ;; Assign to memory address
                 (== exp (jassign addr-exp val))
                 (eval-env-listo `(,addr-exp ,val) env value^ store store^ next-address next-address~)
                 (effect-propagateo value^ value
                                    store^ store~
                                    next-address~ next-address~
                                    (== value^ (value-list `(,(jref addr) ,val^)))
                                    (== value val^)
                                    (set-indexo store^ addr val^ store~))))
         ((fresh (first second dummy value^) ;; Begin-discard
                 (== exp (jbeg first second))
                 (eval-env-listo `(,first ,second) env value^ store store~ next-address next-address~)
                 (effect-propagateo value^ value
                                    store~ store~
                                    next-address~ next-address~
                                    (== value^ (value-list `(,dummy ,value))))))
         ((fresh (cond cond^ then else store^ next-address^) ;; If statements
                 (== exp (jif cond then else))
                 (eval-envo cond env cond^ store store^ next-address next-address^)
                 (effect-propagateo cond^ value
                                    store^ store~
                                    next-address^ next-address~
                                    (conde ((== cond^ (jbool #f)) (eval-envo else env value store^ store~ next-address^ next-address~))
                                           ((== cond^ (jbool #t)) (eval-envo then env value store^ store~ next-address^ next-address~))))))
         ((fresh (cond body store^ next-address^ cond^) ;; While
                 (== exp (jwhile cond body))
                 (eval-envo cond env cond^ store store^ next-address next-address^)
                 (effect-propagateo cond^ value
                                    store^ store~
                                    next-address^ next-address~
                                    (conde ((== cond^ (jbool #f)) (== value (jundef)) (== store^ store~) (== next-address^ next-address~))
                                           ((== cond^ (jbool #t)) (eval-envo (jbeg body (jwhile cond body)) env value store^ store~ next-address^ next-address~))))))
         ((fresh (try-exp finally-exp try-value finally-value store^ next-address^) ;; Finally
                 (== exp (jfin try-exp finally-exp))
                 (eval-envo try-exp env try-value store store^ next-address next-address^)
                 (eval-envo finally-exp env finally-value store^ store~ next-address^ next-address~)
                 (effect-propagateo finally-value value
                                    store~ store~
                                    next-address~ next-address~
                                    (== value try-value))))
         ((fresh (label label^ try-exp catch-var catch-exp try-value store^ next-address^ env^ break-value first rest) ;; Catch
                 (== exp (jcatch label try-exp catch-var catch-exp))
                 (eval-envo try-exp env try-value store store^ next-address next-address^)
                 (conde ((== try-value (jbrk label^ break-value)) ;; Break does not match label
                         (== `(,value ,store~ ,next-address~) `(,try-value ,store^ ,next-address^))
                         (=/= label^ label))
                        ((== try-value `(,first . ,rest)) ;; No break was caught
                         (== `(,value ,store~ ,next-address~) `(,try-value ,store^ ,next-address^))
                         (=/= first `break))
                        ((== try-value (jbrk label break-value)) ;; Exception was caught
                         (appendo `((,catch-var . ,break-value)) env env^)
                         (eval-envo catch-exp env^ value store^ store~ next-address^ next-address~)))))
         ((jdeltao env exp value store store~ next-address next-address~))
         ((fresh (label val val^) ;; Throw
                 (== exp (jthrow label val))
                 (eval-envo val env val^ store store~ next-address next-address~)
                 (effect-propagateo val^ value
                                    store~ store~
                                    next-address~ next-address~
                                    (== value (jbrk label val^)))))))

(define (jdeltao env exp value store store~ next-address next-address~)
  (fresh (func vals op1 op2 v1 v2 vals^ res rem value^)
         (== exp (jdelta func vals))
         (eval-env-listo vals env value^ store store~ next-address next-address~)
         (effect-propagateo value^ value
                            store~ store~
                            next-address~ next-address~
                            (== value^ (value-list vals^))
                            (conde ((== `(,func (,op1)) `(typeof ,vals^)) ;; Typeof
                                    (typeofo op1 value store~))
                                   ((== `(,v1) vals^) ;; Char -> nat
                                    (typeofo v1 (jstr "string") store~)
                                    (== `(,(jrawstr `(,op1))) vals^)
                                    (== func `char->nat)
                                    (== value (jrawnum op1)))
                                   ((== `(,v1) vals^) ;; Nat -> char
                                    (typeofo v1 (jstr "number") store~)
                                    (== `(,(jrawnum op1)) vals^)
                                    (== func `nat->char)
                                    (== value (jrawstr `(,op1))))
                                   ((== `(,func ,vals^) `(=== (,v1 ,v2)))
                                    (conde ((== v1 v2) (== value (jbool #t)))
                                           ((== value (jbool #f)) (=/= v1 v2))))
                                   ((== `(,v1 ,v2) vals^) ;; Number operations
                                    (typeofo v1 (jstr "number") store~)
                                    (typeofo v2 (jstr "number") store~)
                                    (== `(,(jrawnum op1) ,(jrawnum op2)) vals^)
                                    (conde ((== func `+) (== value (jrawnum res)) (pluso op1 op2 res))
                                           ((== func `-) (== value (jrawnum res)) (minuso op1 op2 res))
                                           ((== func `*) (== value (jrawnum res)) (*o op1 op2 res))
                                           ((== func `/) (== value (jrawnum res)) (/o op1 op2 res rem))
                                           ((== func `<) (conde ((== value (jbool #t)) (<o op1 op2))
                                                                ((== value (jbool #f)) (<=o op2 op1))))))
                                   ((== `(,v1 ,v2) vals^) ;; String operations
                                    (typeofo v1 (jstr "string") store~)
                                    (typeofo v2 (jstr "string") store~)
                                    (== `(,(jrawstr op1) ,(jrawstr op2)) vals^)
                                    (conde ((== func `string-+) (== value (jrawstr res)) (appendo op1 op2 res))
                                           ((== func `string-<) (string-lesso op1 op2 value))))))))

(define (typeofo exp value store)
  (fresh (temp)(conde ((== exp (jundef))
                       (== value (jstr "undefined")))
                      ((== exp (jnul))
                       (== value (jstr "object")))
                      ((== exp `(string . ,temp)) ;; For all of these, the prefixes are hardcoded
                       (== value (jstr "string")))
                      ((== exp  `(number . ,temp))
                       (== value (jstr "number")))
                      ((== exp  `(boolean . ,temp))
                       (== value (jstr "boolean")))
                      ((fresh (fields priv call)
                              (== exp (jref temp))
                              (conde ((== value (jstr "object"))
                                      (indexo store temp `(object ,fields))
                                      (lookupo (jstr "private") fields (jobj priv))
                                      (absent-keyso (jstr "call") priv))
                                     ((== value (jstr "function"))
                                      (indexo store temp `(object ,fields))
                                      (lookupo (jstr "private") fields (jobj priv))
                                      (lookupo (jstr "call") priv call))))))))


(define (string-lesso s1 s2 value)
  (fresh (x x^ y y^ rest)
         (conde ((== s1 `()) (== s2 `(,x . ,rest)) (== value (jbool #t)))
                ((== s2 `()) (== value (jbool #f)))
                ((== s1 `(,x . ,x^)) (== s2 `(,x . ,y^)) (string-lesso x^ y^ value))
                ((== s1 `(,x . ,x^)) (== s2 `(,y . ,y^)) (== value (jbool #t)) (=/= x y) (<o x y))
                ((== s1 `(,x . ,x^)) (== s2 `(,y . ,y^)) (== value (jbool #f)) (=/= x y) (<=o y x)))))

(define-syntax-rule (effect-propagateo value^        value~
                                       store^        store~
                                       next-address^ next-address~
                                       cont ...)
  (fresh (label bval tag rest)
    (conde ((== (jbrk label bval) value^)
            (== `(,value~ ,store~ ,next-address~) `(,value^ ,store^ ,next-address^)))
           ((== `(,tag . ,rest) value^)
            (=/= tag `break)
            cont ...))))

(define (eval-env-listo elist env vlist store store~ next-address next-address~)
  (conde ((== elist `()) (== vlist (value-list `())) (== store store~) (== next-address next-address~))
         ((fresh (exp exp-rest val value-rest value-rest^ store^ next-address^)
                 (== elist `(,exp . ,exp-rest))
                 (eval-envo exp env val store store^ next-address next-address^)
                 (effect-propagateo val vlist
                                    store^ store~
                                    next-address^ next-address~
                                    (eval-env-listo exp-rest env value-rest store^ store~ next-address^ next-address~)
                                    (effect-propagateo value-rest vlist
                                                       store~ store~
                                                       next-address~ next-address~
                                                       (== value-rest (value-list value-rest^))
                                                       (== vlist (value-list `(,val . ,value-rest^)))))))))


(define (jnumbero exp)
  (fresh (val) (== exp `(number ,val))))

(define (jstro exp)
  (fresh (val) (== exp `(string ,val))))

(define (objecto exp)
  (fresh (binds) (== exp `(object ,binds))))

(define (boolo exp)
  (fresh (b) (== exp (jbool b))))
