#lang racket
(require "faster-miniKanren/mk.rkt" "js-structures.rkt")
(provide evalo evalo-env)
;; For now, enviroments and objects are lists of pairs. Maybe use hashes?

(define (evalo exp val)
  (fresh (store^ next-address^) (evalo-env exp `() val `() store^ 0 next-address^)))

(define (evalo-env exp env value store store^ next-address next-address^)
  (conde ((numbero exp)
          (== exp value)
          (== `(,store . ,next-address) `(,store^ . ,next-address^))) ;; temporary!
         ((fresh (params body cenv)
                 (== exp (jclo params body cenv))
                 (== value exp)
                 (== `(,store . ,next-address) `(,store^ . ,next-address^))))
         ((fresh (key exp2 env2) ;; Let
                 (== exp (jlet key value exp2))
                 (== env2 `((,key . ,value) . ,env))
                 (evalo-env exp2 env2 value store store^ next-address next-address^)))
         ((fresh (var)
                 (== exp (jvar var))
                 (lookupo var env value)
                 (== `(,store . ,next-address) `(,store^ . ,next-address^))))
         ((fresh (body params) ;; Closure
                 (== exp (jfun params body))
                 (== value (jclo params body env))
                 (== `(,store . ,next-address) `(,store^ . ,next-address^))))
         ((fresh (closure params args args-eval body cenv cenv^ zipped store^^ store^^^ next-address^^ next-address^^^) ;; Function application
                 (== exp (japp closure args))
                 (evalo-env closure env (jclo params body cenv) store store^^ next-address next-address^^)
                 (evalo-env-list args env args-eval store^^ store^^^ next-address^^ next-address^^^)
                 (zipo zipped params args-eval)
                 (extendo cenv zipped cenv^)
                 (evalo-env body cenv^ value store^^^ store^ next-address^^^ next-address^)
                 ))
         ((fresh (obj key key^) ;; Get
                 (== exp (jget obj key))
                 (evalo-env key env key^ store store^ next-address next-address^)
                 (conde ((absento-keys key^ obj) ;; not found
                         (== value `undefined))
                        ((membero `(,key^ . ,value) obj))))) ;; found
         ((fresh (obj key key^ val val^ store^^ next-address^^) ;; Create field
                 (== exp (jset obj key val))
                 (evalo-env key env key^ store store^^ next-address next-address^^)
                 (evalo-env val env val^ store^^ store^ next-address^^ next-address^)
                 (== value `((,key^ . ,val^) . ,obj))
                 (absento-keys key^ obj)))
         ((fresh (obj key key^ val val^ v store^^ next-address^^) ;; Update field
                 (== exp (jset obj key val))
                 (evalo-env key env key^ store store^^ next-address next-address^^)
                 (evalo-env val env val^  store^^ store^ next-address^^ next-address^)
                 (membero `(,key^ . ,v) obj)
                 (updato obj key^ val^ value)))
         ((fresh (obj-prev key key^) ;; Delete field
                 (== exp (jdel obj-prev key))
                 (evalo-env key env key^ store store^ next-address next-address^)
                 (deleto obj-prev key^ value)))
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
  (== `(,out) in))

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
                 (== obj `((,k . ,v ) . ,orest))
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

(define (membero item lst)
  (fresh (first rest)
         (== `(,first . ,rest) lst)
         (conde ((== first item)) ((membero item rest)))))
