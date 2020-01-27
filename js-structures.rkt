#lang racket
(provide jlet jfunc jget jset jdel jref)

(define (jlet key value exp)
  `(let ,key ,value ,exp))

(define (jfunc params body args)
  `(func ,params ,body ,args))

(define (jget obj key)
  `(get ,obj ,key))

(define (jset obj key value)
  `(set ,obj ,key ,value))

(define (jdel obj key)
  `(delete ,obj ,key))

(define (jref var)
  `(ref ,var))