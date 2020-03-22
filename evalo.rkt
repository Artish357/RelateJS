#lang racket
(require "faster-miniKanren/mk.rkt" "js-structures.rkt" "faster-miniKanren/numbers.rkt")
(provide evalo evalo-env appendo)

(define (evalo exp val)
  (fresh (store^ next-address^) (evalo-env exp `() val `() store^ `() next-address^)))

(define (evalo-env exp env value store store~ next-address next-address~)
  (conde ((conde ((jnumbero exp)) ((objecto exp)) ((closuro exp)) ((referenso exp)) ((breako exp)) ((boolo exp)) ((jstro exp))
                 ((== exp (jundef))) ((== exp (jnul)))) ;; Values
          (== exp value)
          (== `(,store . ,next-address) `(,store~ . ,next-address~)))
         ((fresh (k v v^ exp2 env2 store^ next-address^) ;; Let
                 (== exp (jlet k v exp2))
                 (evalo-env v env v^ store store^ next-address next-address^)
                 (== env2 `((,k . ,v^) . ,env))
                 (evalo-env exp2 env2 value store^ store~ next-address^ next-address~)))
         ((fresh (var) ;; Look up a variable
                 (== exp (jvar var))
                 (lookupo var env value)
                 (== `(,store . ,next-address) `(,store~ . ,next-address~))))
         ((fresh (body params) ;; Function creation
                 (== exp (jfun params body))
                 (== value (jclo params body env))
                 (== `(,store . ,next-address) `(,store~ . ,next-address~))))
         ((fresh (closure params args args-eval body cenv cenv^ zipped store^ next-address^) ;; Function application
                 (== exp (japp closure args))
                 (evalo/propagation evalo-env-list `(,closure . ,args)  env (value-list `(,(jclo params body cenv) . ,args-eval))
                                    store store^ store~
                                    next-address next-address^ next-address~
                                    (zipo zipped params args-eval)
                                    (appendo cenv zipped cenv^)
                                    (evalo-env body cenv^ value store^ store~ next-address^ next-address~))))
         ((fresh (obj-exp bindings key key^) ;; Get
                 (== exp (jget obj-exp key))
                 (evalo/propagation evalo-env-list `(,key ,obj-exp) env (value-list `(,key^ ,(jobj bindings)))
                                    store store~ store~
                                    next-address next-address~ next-address~
                                    (conde ((absento-keys key^ bindings) ;; not found
                                            (== value (jundef)))
                                           ((membero `(,key^ . ,value) bindings)))))) ;; found
         ((fresh (obj-exp bindings key key^ val val^) ;; Create field
                 (== exp (jset obj-exp key val))
                 (evalo/propagation evalo-env-list `(,obj-exp ,key ,val) env (value-list `(,(jobj bindings) ,key^ ,val^))
                                    store store~ store~
                                    next-address next-address~ next-address~
                                    (== value (jobj `((,key^ . ,val^) . ,bindings)))
                                    (absento-keys key^ bindings))))
         ((fresh (obj-exp bindings bindings-updated key key^ val val^ v) ;; Update field
                 (== exp (jset obj-exp key val))
                 (evalo/propagation evalo-env-list `(,obj-exp ,key ,val) env (value-list `(,(jobj bindings) ,key^ ,val^))
                                    store store~ store~
                                    next-address next-address~ next-address~
                                    (membero `(,key^ . ,v) bindings)
                                    (updato bindings key^ val^ bindings-updated)
                                    (== value (jobj bindings-updated)))))
         ((fresh (obj-exp bindings-prev bindings key key^ store^ next-address^) ;; Delete field
                 (== exp (jdel obj-exp key))
                 (evalo/propagation evalo-env-list `(,obj-exp ,key) env (value-list `(,(jobj bindings-prev) ,key^))
                                    store store~ store~
                                    next-address next-address~ next-address~
                                    (evalo-env key env key^ store^ store~ next-address^ next-address~)
                                    (deleto bindings-prev key^ bindings)
                                    (== value (jobj bindings)))))
         ((fresh (ref ref^ next-address^ store^) ;; Allocate memory
                 (== exp (jall ref))
                 (evalo/propagation evalo-env ref env ref^
                                    store store^ store~
                                    next-address next-address^ next-address~
                                    (appendo store^ `(,ref^) store~)
                                    (incremento next-address^ next-address~)
                                    (== value (jref next-address)))))
         ((fresh (addr-exp addr) ;; Fetch from memory
                 (== exp (jderef addr-exp))
                 (evalo/propagation evalo-env addr-exp env (jref addr)
                                    store store~ store~
                                    next-address next-address~ next-address~
                                    (indexo store addr value))))
         ((fresh (var val addr val^ store^) ;; Assign to memory address
                 (== exp (jassign var val))
                 (evalo/propagation evalo-env-list `(,var ,val) env (value-list `(,(jref addr) ,val^))
                                    store store^ store~
                                    next-address next-address~ next-address~
                                    (set-indexo store^ addr val^ store~)
                                    (== value val^))))
         ((fresh (first second dummy) ;; Begin-discard
                 (== exp (jbeg first second))
                 (evalo/propagation evalo-env-list `(,first ,second) env (value-list `(,dummy ,value))
                                    store store~ store~ next-address next-address~ next-address~)))
         ((fresh (cond cond^ then else store^ next-address^) ;; If statements
                 (== exp (jif cond then else))
                 (evalo/propagation evalo-env cond env cond^
                                    store store^ store~
                                    next-address next-address^ next-address~
                                    (conde ((== cond^ (jbool #t)) (evalo-env then env value store^ store~ next-address^ next-address~))
                                           ((== cond^ (jbool #f)) (evalo-env else env value store^ store~ next-address^ next-address~))))))
         ((fresh (cond body) ;; While
                 (== exp (jwhile cond body))
                 (evalo-env (jif cond (jbeg body (jwhile cond body)) (jundef))
                            env value store store~ next-address next-address~)))
         ((fresh (try-exp finally-exp try-value store^ next-address^ dummy) ;; Finally
                 (== exp (jfin try-exp finally-exp))
                 (evalo-env try-exp env try-value store store^ next-address next-address^)
                 (evalo/propagation evalo-env-list `(,finally-exp ,try-value) env `(,dummy ,value)
                                    store^ store~ store~ next-address next-address~ next-address~)))
         ((fresh (label label^ try-exp catch-var catch-exp try-value store^ next-address^ env^ break-value first rest) ;; Catch
                 (== exp (jcatch label try-exp catch-var catch-exp))
                 (evalo-env try-exp env try-value store store^ next-address next-address^)
                 (conde ((== try-value (jbrk label break-value)) ;; Exception was caught
                         (appendo env `((,catch-var . ,break-value)) env^)
                         (evalo-env catch-exp env^ value store^ store~ next-address^ next-address~))
                        ((== try-value (jbrk label^ break-value)) ;; Break does not match label
                         (=/= label^ label)
                         (== `(,value ,store~ ,next-address~) `(,try-value ,store^ ,next-address^)))
                        ((== try-value `(,first . ,rest)) ;; No break was caught
                         (=/= first `break)
                         (== `(,value ,store~ ,next-address~) `(,try-value ,store^ ,next-address^))))))
         ((jdeltao env exp value store store~ next-address next-address~))
         ((fresh (label val val^)
                 (== exp (jthrow label val))
                 (evalo-env val env val^ store store~ next-address next-address~)
                 (== value (jbrk label val^))))
         ))

(define (jdeltao env exp value store store~ next-address next-address~)
  (fresh (func vals op1 op2 v1 v2 temp vals^ value^ rem)
         (== exp (jdelta func vals))
         (evalo/propagation evalo-env-list vals env (value-list vals^)
                            store store~ store~
                            next-address next-address~ next-address~
                            (conde ((== `(,func (,op1)) `(typeof ,vals^)) ;; Typeof
                                    (typeofo op1 value))
                                   ((== `(,func ,vals) `(=== (,v1 ,v2)))
                                    (conde ((== v1 v2) (== value #t))
                                           ((=/= v1 v2) (== value #f))))
                                   ((== `(,v1 ,v2) vals^) ;; Number operations
                                    (typeofo v1 (jstr "number"))
                                    (typeofo v2 (jstr "number"))
                                    (== `(,(jrawnum op1) ,(jrawnum op2)) vals^)
                                    (conde ((== func `+) (pluso op1 op2 value^) (== value (jrawnum value^)))
                                           ((== func `-) (minuso op1 op2 value^) (== value (jrawnum value^)))
                                           ((== func `*) (*o op1 op2 value^) (== value (jrawnum value^)))
                                           ((== func `/) (/o op1 op2 value^ rem) (== value (jrawnum value^)))
                                           ((== func `<) (conde ((<o op1 op2) (== value (jbool #t)))
                                                                ((<=o op2 op1) (== value (jbool #f)))))))
                                   ((== `(,v1 ,v2) vals^) ;; String operations
                                    (typeofo v1 (jstr "string"))
                                    (typeofo v2 (jstr "string"))
                                    (== `(,(jrawstr op1) ,(jrawstr op2)) vals^)
                                    (conde ((== func `string-+) (appendo op1 op2 value^) (== value (jrawstr value^)))
                                           ((== func `string-<) (string-lesso op1 op2 value))))
                                   ((== `(,v1) vals^) ;; Char -> nat
                                    (typeofo v1 (jstr "string"))
                                    (== `(,(jrawstr `(,op1))) vals^)
                                    (== func `char->nat)
                                    (== value (jrawnum op1)))
                                   ((== `(,v1) vals^) ;; Nat -> char
                                    (typeofo v1 (jstr "number"))
                                    (== `(,(jrawnum op1)) vals^)
                                    (== func `nat->char)
                                    (== value (jrawstr `(,op1))))))))

(define (typeofo exp value)
  (fresh (temp)(conde ((== exp (jundef))
                       (== value (jstr "undefined")))
                      ((== exp (jnul)) ;; According to node, it is actually "object"
                       (== value (jstr "object")))
                      ((== exp `(string . ,temp)) ;; For all of these, the prefixes are hardcoded
                       (== value (jstr "string")))
                      ((== exp  `(number . ,temp))
                       (== value (jstr "number")))
                      ((== exp  `(boolean . ,temp)) 
                       (== value (jstr "boolean")))
                      ((== exp  `(object . ,temp)) 
                       (== value (jstr "object"))))))

(define (string-lesso s1 s2 value)
  (fresh (x x^ y y^ rest)
         (conde ((== s1 `()) (== s2 `(,x . ,rest)) (== value (jbool #t)))
                ((== s2 `()) (== value (jbool #f)))
                ((== s1 `(,x . ,x^)) (== s2 `(,x . ,y^)) (string-lesso x^ y^ value))
                ((== s1 `(,x . ,x^)) (== s2 `(,y . ,y^)) (=/= x y) (<o x y) (== value (jbool #t)))
                ((== s1 `(,x . ,x^)) (== s2 `(,y . ,y^)) (=/= x y) (<=o y x) (== value (jbool #f))))))

(define-syntax-rule (evalo/propagation evalo-func exp env val
                                       store store^ store~
                                       next-address next-address^ next-address~
                                       cont ...)
  (fresh (intermediate-val label bval tag rest)
         (evalo-func exp env intermediate-val store store^ next-address next-address^)
         (conde ((== (jbrk label bval) intermediate-val)
                 (== `(,val ,store~ ,next-address~) `(,intermediate-val ,store^ ,next-address^)))
                ((== `(,tag . ,rest) intermediate-val)
                 (=/= tag `break)
                 (== val intermediate-val) cont ...))))

(define (evalo-env-list elist env vlist store store~ next-address next-address~)
  (conde ((== elist `()) (== vlist (value-list `())) (== store store~) (== next-address next-address~))
         ((fresh (exp exp-rest val value-rest value-rest^ store^ next-address^)
                 (== elist `(,exp . ,exp-rest))
                 (evalo/propagation evalo-env exp env val
                                    store store^ store~
                                    next-address next-address^ next-address~
                                    (evalo/propagation evalo-env-list exp-rest env value-rest
                                                       store^ store~ store~
                                                       next-address^ next-address~ next-address~
                                                       (== value-rest (value-list value-rest^))
                                                       (== vlist (value-list `(,val . ,value-rest^)))))))))

(define (incremento in out)
  (== out `(,in)))

(define (decremento in out)
  (incremento out in))

(define (keyso o k)
  (conde ((== o `()) (== k `()))
         ((fresh (x v o2 k2)
                 (== o `((,x . ,v) . ,o2))
                 (== k `(,x . ,k2))
                 (keyso o2 k2)))))

(define (absento-keys val obj)
  (fresh (keys)
         (keyso obj keys)
         (not-in-list val keys)))

(define (updato obj key value result)
  (conde ((== obj `())
          (== result `()))
         ((fresh (orest rrest k v v2)
                 (== obj    `((,k . ,v ) . ,orest))
                 (== result `((,k . ,v2) . ,rrest))
                 (conde ((== k key)
                         (== v2 value)
                         (== orest rrest))
                        ((=/= k key)
                         (== v v2)
                         (updato orest key value rrest)))
                 ))))

(define (deleto obj key result)
  (conde ((== obj `()) (== result `()))
         ((fresh (orest rrest k v)
                 (== obj `((,k . ,v) . ,orest))
                 (conde ((== k key)
                         (== orest result))
                        ((=/= k key)
                         (== result `((,k . ,v) . ,rrest))
                         (deleto orest key rrest)))
                 ))))

(define (appendo s t r)
  (conde ((== s `()) (== t r))
         ((fresh (r^ sel srest)
                 (== r `(,sel . ,r^))
                 (== s `(,sel . ,srest))
                 (appendo srest t r^)))))

(define (zipo l keys values)
  (conde ((== keys `()) (== values `()) (== l `()))
         ((fresh (k v l^ krest vrest)
                 (== keys `(,k . ,krest))
                 (== values `(,v . ,vrest))
                 (== l `((,k . ,v) . ,l^))
                 (zipo l^ krest vrest)))))

(define (not-in-list el list)
  (conde
   ((== list `()))
   ((fresh (x rest)
           (== `(,x . ,rest) list)
           (=/= el x)
           (not-in-list el rest)))))

(define (lookupo x env t)
  (fresh (rest y v)
         (== `((,y . ,v) . ,rest) env)
         (conde
          ((== y x) (== v t))
          ((=/= y x) (lookupo x rest t)))))

(define (indexo lst index result)
  (fresh (l lrest index^)
         (== lst `(,l . ,lrest))
         (conde ((== index `())
                 (== result l))
                ((=/= index `())
                 (decremento index index^)
                 (indexo lrest index^ result)))))

(define (set-indexo lst index value result)
  (fresh (l lrest rrest index^)
         (== lst `(,l . ,lrest))
         (conde ((== index `())
                 (== result `(,value . ,lrest)))
                ((=/= index `())
                 (decremento index index^)
                 (== result `(,l . ,rrest))
                 (set-indexo lrest index^ value rrest)))))

(define (membero item lst)
  (fresh (first rest)
         (== `(,first . ,rest) lst)
         (conde ((== first item)) ((membero item rest)))))

(define (jnumbero exp)
  (fresh (n val) (== exp (cons (car (jnum 0)) val))))

(define (jstro exp)
  (fresh (n val) (== exp (cons (car (jstr "")) val))))

(define (objecto exp)
  (fresh (binds) (== exp (jobj binds))))

(define (closuro exp)
  (fresh (params body env) (== exp (jclo params body env))))

(define (referenso exp)
  (fresh (addr) (== exp (jref addr))))

(define (boolo exp)
  (fresh (b) (== exp (jbool b))))

(define (breako exp)
  (fresh (label value) (== exp (jbrk label value))))
