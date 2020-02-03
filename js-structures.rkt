#lang racket
(provide jlet jfun jclo japp jget jset jdel jvar jobj)

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
  `(ref ,var))

(define (jobj bindings)
  `(object ,bindings))
