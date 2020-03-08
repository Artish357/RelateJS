#lang racket
(require "js-structures.rkt" "faster-miniKanren/mk.rkt" "evalo.rkt")
(provide pull-varo pull-varo-list pull-pairso pull-varo-pairs humanize dehumanize)

(define (pull-varo exp vars)
  (fresh (e erest)
         (conde ((== `(,exp . ,vars) `(() . ())))
                ((== exp `(function . ,erest)) (== vars `()))
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
                 (conde ((== var `(,name ,val)))
                        ((symbolo var)
                         (== `(,name . ,val) `(,var . undefined))))
                 (pull-pairso v-rest rest)
                 (== pairs `((,name . ,val) . ,rest))))))

(define (pull-varo-pairs exp pairs)
  (fresh (vars)
         (pull-varo exp vars)
         (pull-pairso vars pairs)))

(define/match (mknum->num x)
  [((list)) 0]
  [((cons d rest)) (+ d (* 2 (mknum->num rest)))])

(define/match (dehumanize exp)
  [((? string?)) (jstr exp)]
  [((? integer?)) (jnum exp)]
  [((? list?)) (map dehumanize exp)])

(define/match (humanize exp)
  [((list `string x)) (list->string (map (compose integer->char mknum->num) x))]
  [((list `number x)) (mknum->num x)]
  [((? list?)) (map humanize exp)])
