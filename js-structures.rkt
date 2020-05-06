#lang racket
(require "faster-miniKanren/numbers.rkt")
(provide jlet jfun jclo japp jget jset jdel jvar jrawnum jnum
         jobj jref jderef jassign jall jbeg jbool
         jif jundef jnul jwhile jbrk jthrow jfin jcatch
         jdelta jrawstr jstr jnul)

(define (jlet key value exp)
  `(let ,key ,value ,exp))

(define (jfun params body)
  `(fun ,params ,body))

(define (jclo params body env)
  `(jclosure ,params ,body ,env))

(define (japp closure args)
  `(app ,closure ,args))

(define (jget obj key)
  `(get ,obj ,key))

(define (jset obj key value)
  `(set ,obj ,key ,value))

(define (jdel obj key)
  `(delete ,obj ,key))

(define (jvar var)
  `(var ,var))

(define (jrawnum n)
  `(number ,n))

(define (jnum n)
  `(number ,(build-num n)))

(define (jobj bindings)
  `(object ,bindings))

(define (jall value)
  `(allocate ,value))

(define (jref value)
  `(ref ,value))

(define (jderef address)
  `(deref ,address))

(define (jassign var val)
  `(assign ,var ,val))

(define (jbeg first second)
  `(begin ,first ,second))

(define (jbool bool)
  `(boolean ,bool))

(define (jif cond then else)
  `(if ,cond ,then ,else))

(define (jundef)
  `(undefined))

(define (jnul)
  `(null))

(define (jwhile cond body)
  `(while ,cond ,body))

(define (jthrow label value)
  `(throw ,label ,value))

(define (jbrk label value)
  `(break ,label ,value))

(define (jfin try-exp fin-exp)
  `(finally ,try-exp ,fin-exp))

(define (jcatch label try-exp catch-var catch-exp)
  `(catch ,label ,try-exp ,catch-var ,catch-exp))

(define (jrawstr str)
  `(string ,str))

(define (jstr str)
  `(string ,(map (compose1 build-num char->integer) (string->list str))))

(define (jdelta fun vals)
  `(delta ,fun ,vals))

(define (jpass) `pass)
