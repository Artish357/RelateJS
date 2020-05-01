#lang racket
(require "faster-miniKanren/mk.rkt" "js-structures.rkt" "faster-miniKanren/numbers.rkt" "helpers.rkt")
(provide evalo evalo-env evalo-ns)

(define (evalo exp val store)
  (fresh (next-address^) (evalo-env exp `() val `() store `() next-address^)))

(define (evalo-ns exp val)
  (fresh (next-address^ store) (evalo-env exp `() val `() store `() next-address^)))

(define (evalo-env exp env value store store~ next-address next-address~)
  (conde ((== exp value)
          (== store store~)
          (== next-address next-address~)
          (conde ((jnumbero exp)) ((objecto exp)) ((closuro exp)) ((referenso exp)) ((breako exp)) ((boolo exp)) ((jstro exp))
                 ((== exp (jundef))) ((== exp (jnul)))) ;; Values
          )
         ((fresh (k v v^ exp2 env2 store^ next-address^) ;; Let
                 (== exp (jlet k v exp2))
                 (evalo/propagation evalo-env v env v^ value
                                    store store^ store~
                                    next-address next-address^ next-address~
                                    (== env2 `((,k . ,v^) . ,env))
                                    (evalo-env exp2 env2 value store^ store~ next-address^ next-address~))))
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
         ((fresh (func params args args-eval body cenv cenv^ zipped store^ next-address^) ;; Function application
                 (== exp (japp func args))
                 (evalo/propagation evalo-env-list `(,func . ,args) env (value-list `(,(jclo params body cenv) . ,args-eval)) value
                                    store store^ store~
                                    next-address next-address^ next-address~
                                    (zipo zipped params args-eval)
                                    (appendo cenv zipped cenv^)
                                    (evalo-env body cenv^ value store^ store~ next-address^ next-address~))))
         ((fresh (obj-exp bindings key key^) ;; Get
                 (== exp (jget obj-exp key))
                 (evalo/propagation evalo-env-list `(,key ,obj-exp) env (value-list `(,key^ ,(jobj bindings))) value
                                    store store~ store~
                                    next-address next-address~ next-address~
                                    (typeofo key^ (jstr "string") store~)
                                    (conde ((membero `(,key^ . ,value) bindings)) ;; found
                                           ((== value (jundef))
                                            (absento-keys key^ bindings) ;; not found
                                            )))))
         ((fresh (obj-exp bindings key key^ val val^) ;; Create field
                 (== exp (jset obj-exp key val))
                 (evalo/propagation evalo-env-list `(,obj-exp ,key ,val) env (value-list `(,(jobj bindings) ,key^ ,val^)) value
                                    store store~ store~
                                    next-address next-address~ next-address~
                                    (== value (jobj (cons (cons key^ val^) bindings)))
                                    (typeofo key^ (jstr "string") store~)
                                    (absento-keys key^ bindings))))
         ((fresh (obj-exp bindings bindings^ key key^ val val^ v) ;; Update field
                 (== exp (jset obj-exp key val))
                 (evalo/propagation evalo-env-list `(,obj-exp ,key ,val) env (value-list `(,(jobj bindings) ,key^ ,val^)) value
                                    store store~ store~
                                    next-address next-address~ next-address~
                                    (== value (jobj bindings^))
                                    (typeofo key^ (jstr "string") store~)
                                    (membero `(,key^ . ,v) bindings)
                                    (updato bindings key^ val^ bindings^)
                                    )))
         ((fresh (obj-exp bindings bindings^ key key^ store^ next-address^) ;; Delete field
                 (== exp (jdel obj-exp key))
                 (evalo/propagation evalo-env-list `(,obj-exp ,key) env (value-list `(,(jobj bindings) ,key^)) value
                                    store store~ store~
                                    next-address next-address~ next-address~
                                    (== value (jobj bindings^))
                                    (typeofo key^ (jstr "string") store~)
                                    (deleto bindings key^ bindings^)
                                    (evalo-env key env key^ store^ store~ next-address^ next-address~)
                                    )))
         ((fresh (ref ref^ next-address^ store^) ;; Allocate memory
                 (== exp (jall ref))
                 (evalo/propagation evalo-env ref env ref^ value
                                    store store^ store~
                                    next-address next-address^ next-address~
                                    (== value (jref next-address))
                                    (appendo store^ `(,ref^) store~)
                                    (incremento next-address^ next-address~)
                                    )))
         ((fresh (addr-exp addr) ;; Fetch from memory
                 (== exp (jderef addr-exp))
                 (evalo/propagation evalo-env addr-exp env (jref addr) value
                                    store store~ store~
                                    next-address next-address~ next-address~
                                    (indexo store~ addr value))))
         ((fresh (var val addr val^ store^) ;; Assign to memory address
                 (== exp (jassign var val))
                 (evalo/propagation evalo-env-list `(,var ,val) env (value-list `(,(jref addr) ,val^)) value
                                    store store^ store~
                                    next-address next-address~ next-address~
                                    (== value val^)
                                    (set-indexo store^ addr val^ store~)
                                    )))
         ((fresh (first second dummy) ;; Begin-discard
                 (== exp (jbeg first second))
                 (evalo/propagation evalo-env-list `(,first ,second) env (value-list `(,dummy ,value)) value
                                    store store~ store~ next-address next-address~ next-address~)))
         ((fresh (cond cond^ then else store^ next-address^) ;; If statements
                 (== exp (jif cond then else))
                 (evalo/propagation evalo-env cond env cond^ value
                                    store store^ store~
                                    next-address next-address^ next-address~
                                    (conde ((== cond^ (jbool #f)) (evalo-env else env value store^ store~ next-address^ next-address~))
                                           ((== cond^ (jbool #t)) (evalo-env then env value store^ store~ next-address^ next-address~))))))
         ((fresh (cond body store^ next-address^ cond^) ;; While
                 (== exp (jwhile cond body))
                 (evalo/propagation evalo-env cond env cond^ value
                                    store store^ store~
                                    next-address next-address^ next-address~
                                    (conde ((== cond^ (jbool #f)) (== value (jundef)) (== store^ store~) (== next-address^ next-address~))
                                           ((== cond^ (jbool #t)) (evalo-env (jbeg body (jwhile cond body)) env value store^ store~ next-address^ next-address~))))))
         ((fresh (try-exp finally-exp try-value store^ next-address^) ;; Finally
                 (== exp (jfin try-exp finally-exp))
                 (evalo-env try-exp env try-value store store^ next-address next-address^)
                 (evalo/propagation evalo-env finally-exp env (value-list value) value
                                    store^ store~ store~ next-address next-address~ next-address~)))
         ((fresh (label label^ try-exp catch-var catch-exp try-value store^ next-address^ env^ break-value first rest) ;; Catch
                 (== exp (jcatch label try-exp catch-var catch-exp))
                 (evalo-env try-exp env try-value store store^ next-address next-address^)
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
                         (evalo-env catch-exp env^ value store^ store~ next-address^ next-address~)))))
         ((jdeltao env exp value store store~ next-address next-address~))
         ((fresh (label val val^) ;; Throw
                 (== exp (jthrow label val))
                 (== value (jbrk label val^))
                 (evalo-env val env val^ store store~ next-address next-address~)
                 ))
         ))

(define (jdeltao env exp value store store~ next-address next-address~)
  (fresh (func vals op1 op2 v1 v2 vals^ value^ rem)
         (== exp (jdelta func vals))
         (evalo/propagation evalo-env-list vals env (value-list vals^) value
                            store store~ store~
                            next-address next-address~ next-address~
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
                                    (conde ((== func `+) (== value (jrawnum value^)) (pluso op1 op2 value^))
                                           ((== func `-) (== value (jrawnum value^)) (minuso op1 op2 value^))
                                           ((== func `*) (== value (jrawnum value^)) (*o op1 op2 value^))
                                           ((== func `/) (== value (jrawnum value^)) (/o op1 op2 value^ rem))
                                           ((== func `<) (conde ((== value (jbool #t)) (<o op1 op2))
                                                                ((== value (jbool #f)) (<=o op2 op1))))))
                                   ((== `(,v1 ,v2) vals^) ;; String operations
                                    (typeofo v1 (jstr "string") store~)
                                    (typeofo v2 (jstr "string") store~)
                                    (== `(,(jrawstr op1) ,(jrawstr op2)) vals^)
                                    (conde ((== func `string-+) (== value (jrawstr value^)) (appendo op1 op2 value^))
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
                                      (absento-keys (jstr "call") priv))
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

(define-syntax-rule (evalo/propagation evalo-func exp env val value
                                       store store^ store~
                                       next-address next-address^ next-address~
                                       cont ...)
  (fresh (intermediate-val label bval tag rest)
         (evalo-func exp env intermediate-val store store^ next-address next-address^)
         (conde ((== (jbrk label bval) intermediate-val)
                 (== `(,value ,store~ ,next-address~) `(,intermediate-val ,store^ ,next-address^)))
                ((== `(,tag . ,rest) intermediate-val)
                 (== val intermediate-val)
                 (=/= tag `break)
                 cont ...))))

(define (evalo-env-list elist env vlist store store~ next-address next-address~)
  (conde ((== elist `()) (== vlist (value-list `())) (== store store~) (== next-address next-address~))
         ((fresh (exp exp-rest val value-rest value-rest^ store^ next-address^)
                 (== elist `(,exp . ,exp-rest))
                 (evalo/propagation evalo-env exp env val vlist
                                    store store^ store~
                                    next-address next-address^ next-address~
                                    (evalo/propagation evalo-env-list exp-rest env value-rest vlist
                                                       store^ store~ store~
                                                       next-address^ next-address~ next-address~
                                                       (== value-rest (value-list value-rest^))
                                                       (== vlist (value-list `(,val . ,value-rest^)))))))))


(define (jnumbero exp)
  (fresh (val) (== exp `(number ,val))))

(define (jstro exp)
  (fresh (val) (== exp `(string ,val))))

(define (objecto exp)
  (fresh (binds) (== exp `(object ,binds))))

(define (closuro exp)
  (fresh (params body env) (== exp `(closure ,params ,body ,env))))

(define (referenso exp)
  (fresh (addr) (== exp (jref addr))))

(define (boolo exp)
  (fresh (b) (== exp (jbool b))))

(define (breako exp)
  (fresh (label value) (== exp (jbrk label value))))
