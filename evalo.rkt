#lang racket
(require "faster-miniKanren/mk.rkt" "js-structures.rkt" "faster-miniKanren/numbers.rkt" "helpers.rkt")
(provide evalo eval-envo)

(define (evalo exp val store)
  (fresh (next-address^) (eval-envo exp `() val `() store `() next-address^)))

(define (eval-envo exp env value store store~ next-address next-address~)
  (conde ((== exp value)
          (== store store~)
          (== next-address next-address~)
          (conde ((jnumbero exp)) ((objecto exp)) ((boolo exp)) ((jstro exp))
                 ((== exp (jundef))) ((== exp (jnul)))) ;; Values
          )
         ((fresh (k v v^ exp2 env2 store^ next-address^) ;; Let
                 (== exp (jlet k v exp2))
                 (eval-envo v env v^ store store^ next-address next-address^)
                 (effect-propagateo v^ value
                                    store^ store~
                                    next-address^ next-address~
                                    (== env2 `((,k . ,v^) . ,env))
                                    (eval-envo exp2 env2 value store^ store~ next-address^ next-address~))))
         ((fresh (var) ;; Look up a variable
                 (== exp (jvar var))
                 (== store store~)
                 (== next-address next-address~)
                 (lookupo var env value)
                 ))
         ((fresh (body params) ;; Function creation
                 (== exp (jfun params body))
                 (== value (jclo params body env))
                 (== store store~)
                 (== next-address next-address~)))
         ((fresh (func params args args-eval body cenv cenv^ zipped value^ store^ next-address^) ;; Function application
                 (== exp (japp func args))
                 (== value^ (value-list `(,(jclo params body cenv) . ,args-eval)))
                 (eval-env-listo `(,func . ,args) env value^ store store^ next-address next-address^)
                 (effect-propagateo value^ value
                                    store^ store~
                                    next-address^ next-address~
                                    (zipo zipped params args-eval)
                                    (appendo zipped cenv cenv^)
                                    (eval-envo body cenv^ value store^ store~ next-address^ next-address~))))
         ((fresh (obj-exp bindings key key^ value^) ;; Get
                 (== exp (jget obj-exp key))
                 (== value^ (value-list `(,key^ ,(jobj bindings))))
                 (eval-env-listo `(,key ,obj-exp) env value^ store store~ next-address next-address~)
                 (effect-propagateo value^ value
                                    store~ store~
                                    next-address~ next-address~
                                    (typeofo key^ (jstr "string") store~)
                                    (conde ((membero `(,key^ . ,value) bindings)) ;; found
                                           ((== value (jundef))
                                            (absent-keyso key^ bindings) ;; not found
                                            )))))
         ((fresh (obj-exp bindings bindings^ key key^ val val^ v value^) ;; Create/update field
                 (== exp (jset obj-exp key val))
                 (== value^ (value-list `(,(jobj bindings) ,key^ ,val^)))
                 (eval-env-listo `(,obj-exp ,key ,val) env value^ store store~ next-address next-address~)
                 (effect-propagateo value^ value
                                    store~ store~
                                    next-address~ next-address~
                                    (== value (jobj bindings^))
                                    (typeofo key^ (jstr "string") store~)
                                    (== value (jobj bindings^))
                                    (updateo bindings key^ val^ bindings^))))
         ((fresh (obj-exp bindings bindings^ key key^ store^ next-address^ value^) ;; Delete field
                 (== exp (jdel obj-exp key))
                 (== value^ (value-list `(,(jobj bindings) ,key^)))
                 (eval-env-listo `(,obj-exp ,key) env value^ store store~ next-address next-address~)
                 (effect-propagateo value^ value
                                    store~ store~
                                    next-address~ next-address~
                                    (== value (jobj bindings^))
                                    (typeofo key^ (jstr "string") store~)
                                    (deleto bindings key^ bindings^)
                                    (eval-envo key env key^ store^ store~ next-address^ next-address~)
                                    )))
         ((fresh (ref ref^ next-address^ store^) ;; Allocate memory
                 (== exp (jall ref))
                 (eval-envo ref env ref^ store store^ next-address next-address^)
                 (effect-propagateo ref^ value
                                    store^ store~
                                    next-address^ next-address~
                                    (== value (jref next-address))
                                    (appendo store^ `(,ref^) store~)
                                    (incremento next-address^ next-address~)
                                    )))
         ((fresh (addr-exp addr value^) ;; Fetch from memory
                 (== exp (jderef addr-exp))
                 (== value^ (jref addr))
                 (eval-envo addr-exp env value^ store store~ next-address next-address~)
                 (effect-propagateo value^ value
                                    store~ store~
                                    next-address~ next-address~
                                    (indexo store~ addr value))))
         ((fresh (var val addr val^ store^ value^) ;; Assign to memory address
                 (== exp (jassign var val))
                 (== value^ (value-list `(,(jref addr) ,val^)))
                 (eval-env-listo `(,var ,val) env value^ store store^ next-address next-address~)
                 (effect-propagateo value^ value
                                    store^ store~
                                    next-address~ next-address~
                                    (== value val^)
                                    (set-indexo store^ addr val^ store~)
                                    )))
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
                 (eval-envo finally-exp env finally-value store^ store~ next-address next-address~)
                 (effect-propagateo finally-value value
                                    store~ store~
                                    next-address~ next-address~
                                    (== value try-value))))
         ((fresh (label label^ try-exp catch-var catch-exp try-value store^ next-address^ env^ break-value first rest) ;; Catch
                 (== exp (jcatch label try-exp catch-var catch-exp))
                 (eval-envo try-exp env try-value store store^ next-address next-address^)
                 (conde ((== try-value (jbrk label^ break-value)) ;; Break does not match label
                         (== `(,value ,store~ ,next-address~) `(,try-value ,store^ ,next-address^))
                         (=/= label^ label)
                         )
                        ((== try-value `(,first . ,rest)) ;; No break was caught
                         (== `(,value ,store~ ,next-address~) `(,try-value ,store^ ,next-address^))
                         (=/= first `break)
                         )
                        ((== try-value (jbrk label break-value)) ;; Exception was caught
                         (appendo env `((,catch-var . ,break-value)) env^)
                         (eval-envo catch-exp env^ value store^ store~ next-address^ next-address~)))))
         ((jdeltao env exp value store store~ next-address next-address~))
         ((fresh (label val val^) ;; Throw
                 (== exp (jthrow label val))
                 (== value (jbrk label val^))
                 (eval-envo val env val^ store store~ next-address next-address~)
                 ))
         ))

(define (jdeltao env exp value store store~ next-address next-address~)
  (fresh (func vals op1 op2 v1 v2 vals^ res rem value^)
         (== exp (jdelta func vals))
         (== value^ (value-list vals^))
         (eval-env-listo vals env value^ store store~ next-address next-address~)
         (effect-propagateo value^ value
                            store~ store~
                            next-address~ next-address~
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
