#lang racket
(provide jlet jfun jclo japp jget jset jdel jvar
         jobj jref jderef jass jall jbeg jbool
         jif jundef jnul jwhile jbrk value-list)

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

(define (jobj bindings)
  `(object ,bindings))

(define (jall value)
  `(allocate ,value))

(define (jref value)
  `(ref ,value))

(define (jderef address)
  `(deref ,address))

(define (jass var val)
  `(assign ,var ,val))

(define (jbeg first second)
  `(begin ,first ,second))

(define (jbool bool)
  `(boolean ,bool))

(define (jif cond then else)
  `(if ,cond ,then ,else))

(define (jundef)
  `undefined)

(define (jnul)
  `null)

(define (jwhile cond body)
  `(while ,cond ,body))

(define (jbrk label value)
  `(jbreak ,label ,value))

(define (value-list . values)
  (cons `value-list values))
