#lang racket
(require "faster-miniKanren/mk.rkt" "js-structures.rkt")
(provide evalo)
;; For now, enviroments and objects are lists of pairs. Maybe use hashes?

(define (evalo exp val)
  (evalo-env exp `() val))

(define (evalo-env exp env value)
  (conde ((fresh (key exp2 env2) ;; Let
                 (== exp (jlet key value exp2))
                 (== env2 (cons (cons key value) env))
                 (evalo-env exp2 env value)))
         ((fresh (body) ;; Func, no arguments
                 (== exp (jfunc `() body `()))
                 (evalo-env body env value)))
         ((fresh (param params body arg args nenv) ;; Func, with arguments
                 (== exp (jfunc (cons param params) body (cons arg args)))
                 (== nenv `((,param . ,arg) . ,env))
                 (evalo-env (jfunc params body args) nenv value)))
         ((fresh (obj key) ;; Get field
                 (== exp (jget obj key))
                 (membero `(,key . ,value) obj)))
         ((fresh (key) ;; Get, not found, empty object
                 (== exp (jget `() key))
                 (== value `undefined)))
         ((fresh (obj obj-prev key k v) ;; Get, not found, nonempty object
                 (== exp (jget obj key))
                 (== obj `((,k . ,v) . ,obj-prev))
                 (== value `undefined)
                 (evalo-env (jget obj-prev key) env value)))
         ((fresh (key val) ;; Create field, empty object
                 (== exp (jset `() key val))
                 (== value `((,key . ,val)))))
         ((fresh (obj key val) ;; Create field, nonempty object
                 (== exp (jset obj key val))
                 (== value `((,key . ,val) . ,obj))
                 (absento-keys key obj)
          ))
         ((fresh (obj key val v) ;; Update field
                 (== exp (jset obj key val))
                 (membero `(,key . ,v) obj)
                 (updato obj key val value)
                 ))
         ((fresh (obj-prev key) ;; Delete field, found
                 (== exp (jdel obj-prev key))
                 (deleto obj-prev key value)
                 ))
         ((fresh (key) ;; Delete field, not found
                 (== exp (jdel value key))
                 (absento-keys key value)))
         ))

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
  (conde ((== obj `()) (== result `()))
         ((fresh (orest rrest k v v2)
                 (== obj    `((,k . ,v ) . ,orest))
                 (== result `((,k . ,v2) . ,rrest))
                 (conde ((== k key) (== v2 value) (== orest rrest))
                        ((=/= k key) (== v v2) (updato orest key value rrest)))
                 ))))

(define (deleto obj key result)
  (conde ((fresh (v) (== obj `((,key . ,v))) (== result `())))
         ((fresh (orest rrest k v)
                 (== obj `((,k . ,v ) . ,orest))
                 (conde ((== k key) (== orest result))
                        ((=/= k key) (== result `((,k . ,v) . ,rrest)) (deleto orest key rrest)))
                 ))))

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
