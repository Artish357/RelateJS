#lang racket
(require "faster-miniKanren/mk.rkt" "js-structures.rkt")
(provide evalo evalo-env)

(define (evalo exp val)
  (fresh (store^ next-address^) (evalo-env exp `() val `() store^ `() next-address^)))

(define (evalo-env exp env value store store^ next-address next-address^)
  (conde ((conde ((numbero exp)) ((objecto exp)) ((closuro exp)) ((referenso exp))) ;; Values
          (== exp value)
          (== `(,store . ,next-address) `(,store^ . ,next-address^)))
         ((fresh (key exp2 env2) ;; Let
                 (== exp (jlet key value exp2))
                 (== env2 `((,key . ,value) . ,env))
                 (evalo-env exp2 env2 value store store^ next-address next-address^)))
         ((fresh (var) ;; Look up a variable
                 (== exp (jvar var))
                 (lookupo var env value)
                 (== `(,store . ,next-address) `(,store^ . ,next-address^))))
         ((fresh (body params) ;; Function creation
                 (== exp (jfun params body))
                 (== value (jclo params body env))
                 (== `(,store . ,next-address) `(,store^ . ,next-address^))))
         ((fresh (closure params args args-eval body cenv cenv^ zipped store^^ store^^^ next-address^^ next-address^^^) ;; Function application
                 (== exp (japp closure args))
                 (evalo-env closure env (jclo params body cenv) store store^^ next-address next-address^^)
                 (evalo-env-list args env args-eval store^^ store^^^ next-address^^ next-address^^^)
                 (zipo zipped params args-eval)
                 (extendo cenv zipped cenv^)
                 (evalo-env body cenv^ value store^^^ store^ next-address^^^ next-address^)))
         ((fresh (bindings key key^) ;; Get
                 (== exp (jget (jobj bindings) key))
                 (evalo-env key env key^ store store^ next-address next-address^)
                 (conde ((absento-keys key^ bindings) ;; not found
                         (== value `undefined))
                        ((membero `(,key^ . ,value) bindings))))) ;; found
         ((fresh (bindings key key^ val val^) ;; Create field
                 (== exp (jset (jobj bindings) key val))
                 (evalo-env-list `(,key ,val) env `(,key^ ,val^) store store^ next-address next-address^)
                 (== value (jobj `((,key^ . ,val^) . ,bindings)))
                 (absento-keys key^ bindings)))
         ((fresh (bindings bindings-updated key key^ val val^ v) ;; Update field
                 (== exp (jset (jobj bindings) key val))
                 (evalo-env-list `(,key ,val) env `(,key^ ,val^) store store^ next-address next-address^)
                 (membero `(,key^ . ,v) bindings)
                 (updato bindings key^ val^ bindings-updated)
                 (== value (jobj bindings-updated))))
         ((fresh (obj-exp bindings-prev bindings key key^ store^^ next-address^^) ;; Delete field
                 (== exp (jdel obj-exp key))
                 (evalo-env obj-exp env (jobj bindings-prev) store store^^ next-address next-address^^)
                 (evalo-env key env key^ store^^ store^ next-address^^ next-address^)
                 (deleto bindings-prev key^ bindings)
                 (== value (jobj bindings))
                 ))
         ((fresh (ref ref^ next-address^^ store^^) ;; Allocate memory
                 (== exp (jall ref))
                 (evalo-env ref env ref^ store store^^ next-address next-address^^)
                 (appendo store^^ ref^ store^)
                 (incremento next-address^^ next-address^)
                 (== value next-address))) ;; Not sure what it is supposed to evaluate to
         ((fresh (address) ;; Fetch from memory
                 (== exp (jderef address))
                 (indexo store address value)))
         ((fresh (var val addr val^ store^^) ;; Assign to memory
                 (== exp (jass var val))
                 (evalo-env-list `(,var ,val) env `(,(jref addr) ,val^) store store^^ next-address next-address^)
                 (set-indexo store^^ addr val^ store^)
                 (== value val^)
                 )) ;; Not sure what it is supposed to evaluate to
         ))

(define (evalo-env-list exp-list env value-list store store^ next-address next-address^)
  (conde ((== exp-list `()) (== value-list `()) (== store store^) (== next-address next-address^))
         ((fresh (exp exp-rest val value-rest store^^ next-address^^)
                 (== exp-list `(,exp . ,exp-rest))
                 (== value-list `(,val . ,value-rest))
                 (evalo-env exp env val store store^^ next-address next-address^^)
                 (evalo-env-list exp-rest env value-rest store^^ store^ next-address^^ next-address^)))))

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

(define (extendo s t r)
  (conde ((== t `()) (== r s))
         ((fresh (r^ tel trest)
                 (== t `(,tel . ,trest))
                 (== r `(,tel . ,r^))
                 (extendo `(,tel . ,s) trest r^)))))

(define (zipo l keys values)
  (conde ((== keys `()) (== values `()) (== l `()))
         ((fresh (k v l^ krest vrest)
                 (== keys `(,k . ,krest))
                 (== values `(,v . ,vrest))
                 (== l `((,k . ,v) . ,l^))
                 (zipo l^ krest vrest))))
  )


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

(define (appendo list item result)
  (conde ((== list `()) (== result `(,item)))
         ((fresh (l lrest rrest)
                 (== list `(,l . ,lrest))
                 (== result `(,l . ,rrest))
                 (appendo lrest item rrest)))))

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

(define (objecto exp)
  (fresh (binds) (== exp (jobj binds))))

(define (closuro exp)
  (fresh (params body env) (== exp (jclo params body env))))

(define (referenso exp)
  (fresh (addr) (== exp (jref addr))))
