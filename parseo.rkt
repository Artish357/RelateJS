#lang racket
(require "js-structures.rkt" "faster-miniKanren/mk.rkt" "evalo.rkt")
(provide pull-varo pull-varo-list pull-pairso pull-var-nameso humanize dehumanize)

;; Stuff that's unclear:
;; Global scope vars
;; Object fields (as addresses?)
;; Implementation of finally (+ labels?)

(define (parseo exp jexp)
  (conde ((fresh (var val var^ val^) ;; Assignment
                 (== exp `(:= ,var ,val))
                 (lh-parseo var var^)
                 (parseo val val^)
                 (== jexp (jassign var^ val^))))
         ((varo exp jexp)) ;; Var
         ((fresh (try-exp catch-exp try-exp^ catch-exp^) ;; Try/catch
                 (== exp `(try ,try-exp catch ,catch-exp))
                 (parseo-list `(,try-exp ,catch-exp) `(,try-exp^ ,catch-exp^))
                 (== jexp (jcatch `error try-exp^ catch-exp^))))
         ((fresh (try-exp catch-exp try-exp^ catch-exp^) ;; Try/catch/finally 
                 (== exp `(try ,try-exp catch ,catch-exp finally ,finally-exp))
                 (parseo-list `(,try-exp ,catch-exp ,finally-exp) `(,try-exp^ ,catch-exp^ ,finally-exp^))
                 (== jexp (jbeg (jcatch `error try-exp^ catch-exp^) finally-exp^))) ;; QUESTIONABLE!
         ((parse-expo exp jexp)) ;; Expressions
         ))

(define (parse-expo exp jexp)
  (conde ((fresh (val index) ;; Field getter
                 (== exp `(@ ,val ,index))
                 (== jexp (jderef (jget val index)))))
         ((functiono exp jexp)) ;; Functions
         ;; different breaks
         ((fresh (val)  ;; return
                 (== exp `(return ,val))
                 (== jexp (jbrk `return val))))
         ((fresh (val)  ;; throw
                 (== exp `(throw ,val))
                 (== jexp (jbrk `error val))))
         ((fresh (val)  ;; break
                 (== exp `break)
                 (== jexp (jbrk `break `undefined))))
         ;; primitive values
         ((fresh (x)
                 (== exp (jrawnum x))
                 (== exp jexp)))
         ((fresh (x)
                 (== exp (jrawstr x))
                 (== exp jexp)))
         ((symbolo exp) (== jexp (jderef exp)))
         ))

(define (parse-foro exp jexp)
  (conde ((parse-expo exp jexp))
         ((varo exp jexp))))

(define (parseo-list lst jlst)
  (fresh (p p^ rest rest^)
         (conde ((== lst `()) (== jlst `()))
                ((== lst `(,p . ,rest))
                 (parseo p p^)
                 (parseo-list rest rest^)
                 (== jlst `(,p^ . ,rest^))))))

(define (parseo-h exp jexp)
  (parseo (dehumanize exp) jexp))

(define (lh-parseo exp jexp)
  (conde ((symbolo exp) (== jexp (jvar exp)))
         ((fresh (obj val)
                 (== exp `(@ ,obj ,val))
                 (== jexp (jget obj val))))))

(define (varo exp jexp)
  (fresh (vars pairs)
         (== exp `(var . ,vars))
         (pull-pairso vars pairs)
         (conde ((== pairs `()) (== jexp `undefined)) ;; TODO: temporary?
                ((=/= pairs `()) (pair-assigno pairs jexp)))))

(define (functiono exp jexp)
  (fresh (params body body^ body^^ body^^^ vars vars^)
         (== exp `(function ,params . ,body))
         (parseo-list body body^)
         (begino body^ body^^)
         (pull-var-nameso body vars) ;; TODO: needs to operate on a list
         (pado vars `undefined vars^)
         (conde ((== vars^ `())  (== body^^^ body^^))
                ((=/= vars^ `())
                 (leto vars^ body^^ body^^^)
                 ))
         (== jexp (jfun params body^^^))
         ))

(define (leto vars cont jexp)
  (fresh (k v rest jexp-rest)
         (== vars `((,k . ,v) . ,rest))
         (conde ((== rest `())
                 (== jexp (jlet k v cont)))
                ((=/= rest `())
                 (leto rest cont jexp-rest)
                 (== jexp (jlet k v jexp-rest))))))

(define (begino lst jexp)
  (conde ((== lst `()) (== jexp `()))
         ((fresh (e rest rest^)
                 (== lst `(,e . ,rest))
                 (conde ((== rest `()) (== jexp e))
                        ((=/= rest `()) 
                         (begino rest rest^)
                         (== jexp (jbeg e rest^))))))))

(define (pull-varo exp vars)
  (fresh (e erest)
         (conde ((== `(,exp . ,vars) `(() . ())))
                ((== exp `(function . ,erest)) (== vars `()))
                ((symbolo exp) (== vars `()))
                ((== exp `(var . ,vars)))
                ((== exp `(,e . ,erest))
                 (=/= e `var)
                 (pull-varo-list erest vars))
                )
         ))


(define (pull-varo-list exp-list vars)
  (fresh (e erest v vrest)
         (conde ((== `(,exp-list . ,vars) `(() . ())))
                ((== exp-list `(,e . ,erest))
                 (pull-varo e v)
                 (pull-varo-list erest vrest)
                 (extendo v vrest vars))
                )))

(define (pull-pairso vars pairs)
  (conde ((== `(,vars . ,pairs) `(() . ())))
         ((fresh (var val name rest v-rest)
                 (== vars `(,var . ,v-rest))
                 (conde ((== var `(,name ,val))
                         (== pairs `((,name . ,val) . ,rest)))
                        ((symbolo var)
                         (== `(,name . ,val) `(,var . undefined))
                         (== pairs rest)))
                 (pull-pairso v-rest rest)))))

(define (pull-nameso vars names)
  (conde ((== `(,vars . ,names) `(() . ())))
         ((fresh (var v-rest name val rest)
                 (== vars `(,var . ,v-rest))
                 (conde ((== var `(,name ,val))
                         (== names `(,name . ,rest)))
                        ((symbolo var)
                         (== names `(,var . ,rest))))
                 (pull-nameso v-rest rest)))))

(define (pull-varo-pairs exp pairs)
  (fresh (vars)
         (pull-varo exp vars)
         (pull-pairso vars pairs)))

(define (pull-var-nameso exp names)
  (fresh (vars)
         (pull-varo-list exp vars)
         (pull-nameso vars names)
         ))

(define (pair-assigno pairs jexp)
  (fresh (var val var^ val^ rest jexp^)
         (conde ((== pairs `((,var . ,val)))
                 (lh-parseo var var^)
                 (parseo val val^)
                 (== jexp (jassign var^ val^)))
                ((== pairs `((,var . ,val) . ,rest))
                 (=/= rest `())
                 (pair-assigno rest jexp^)
                 (lh-parseo var var^)
                 (parseo val val^)
                 (== jexp (jbeg (jassign var^ val^) jexp^))))))

(define (pado list el out)
  (fresh (a b rest rest-padded)
         (conde ((== `(,list . ,out) `(() . ())))
                ((== list `(,a . ,rest))
                 (== out `((,a . ,el) . ,rest-padded))
                 (pado rest el rest-padded)))))

(define/match (mknum->num x)
  [((list)) 0]
  [((cons d rest)) (+ d (* 2 (mknum->num rest)))])

(define/match (dehumanize exp)
  [((? string?)) (jstr exp)]
  [((? integer?)) (jnum exp)]
  [((? list?)) (map dehumanize exp)]
  [(_) exp])

(define/match (humanize exp)
  [((list `string x)) (list->string (map (compose integer->char mknum->num) x))]
  [((list `number x)) (mknum->num x)]
  [((? list?)) (map humanize exp)]
  [(_) exp])
