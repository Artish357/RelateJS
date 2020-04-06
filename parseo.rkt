#lang racket
(require "js-structures.rkt" "faster-miniKanren/mk.rkt" "evalo.rkt")
(provide pull-varo pull-varo-list pull-pairso pull-names-listo humanize dehumanize parseo parseo-h parseo-nh)

(define (parseo exp jexp)
  (conde ((parse-expo exp jexp)) ;; Expressions
         ((fresh (exps exps^) ;; Begin
                 (== exp `(begin . ,exps))
                 (parse-listo exps exps^)
                 (begino exps^ jexp)))
         ((fresh (cond then else cond^ then^ else^) ;; if statements
                 (== exp `(if ,cond ,then ,else))
                 (== jexp (jif cond^ then^ else^))
                 (parse-expo cond cond^)
                 (parse-listo `(,then ,else) `(,then^ ,else^))
                 ))
         ;; different breaks
         ((fresh (val val^)  ;; return
                 (== exp `(return ,val))
                 (== jexp (jthrow `return val^))
                 (parse-expo val val^)
                 ))
         ((fresh (val val^)  ;; throw
                 (== exp `(throw ,val))
                 (== jexp (jthrow `error val^))
                 (parse-expo val val^)
                 ))
         ((fresh (val)  ;; break
                 (== exp  `(break))
                 (== jexp (jthrow `break (jundef)))))
         ((fresh (try-exp catch-exp try-exp^ catch-exp^ label) ;; Try/catch
                 (== exp `(try ,try-exp catch ,label ,catch-exp))
                 (== jexp (jcatch `error try-exp^ label
                                  (jlet label (jall (jvar label)) catch-exp^)))
                 (parse-listo `(,try-exp ,catch-exp) `(,try-exp^ ,catch-exp^))
                 ))
         ((fresh (try-exp try-exp^  finally-exp finally-exp^) ;; Try/finally
                 (== exp `(try ,try-exp finally ,finally-exp))
                 (== jexp (jfin try-exp^ finally-exp^))
                 (parse-listo `(,try-exp ,finally-exp) `(,try-exp^ ,finally-exp^))
                 ))
         ((fresh (try-exp catch-exp try-exp^ catch-exp^ finally-exp finally-exp^ label) ;; Try/catch/finally
                 (== exp `(try ,try-exp catch ,label ,catch-exp finally ,finally-exp))
                 (== jexp (jbeg (jcatch `error try-exp^ label catch-exp^) finally-exp^))
                 (parse-listo `(,try-exp ,catch-exp ,finally-exp) `(,try-exp^ ,catch-exp^ ,finally-exp^))
                 )) ;; QUESTIONABLE!
         ((fresh (cond body cond^ body^ body^^) ;; while statements
                 (== exp `(while ,cond . ,body))
                 (== jexp (jcatch `break (jwhile cond^ body^^) `e (jundef)))
                 (parse-expo cond cond^)
                 (parse-listo body body^)
                 (begino body^ body^^)
                 ))
         ((parse-foro exp jexp))
         ((switcho exp jexp))
         ((varo exp jexp)) ;; Var
         ))

(define (parse-expo exp jexp)
  (conde ;; primitive values
   ((== exp jexp)(fresh (x) (conde ((== exp `(number ,x)))
                                   ((== exp `(string ,x))))))
   ((== exp #t) (== jexp (jbool #t)))
   ((== exp #f) (== jexp (jbool #f)))
   ((symbolo exp) (== jexp (jderef (jvar exp))))
   ((== exp (jundef)) (== jexp (jundef)))
   ((== exp (jnul)) (== jexp (jnul)))
   ((objecto exp jexp))
   ((fresh (op vals vals^) ;; Basic operations
           (== exp `(op ,op . ,vals))
           (== jexp (jdelta op vals^))
           (parse-exp-listo vals vals^)
           ))
   ((functiono exp jexp)) ;; Functions
   ((fresh (func args func^ args^) ;; Function call
           (== exp `(call ,func . ,args))
           (== jexp (jcatch `return (japp (jget (jget func^ (jstr "private")) (jstr "call")) args^) `result (jvar `result)))
           (parse-exp-listo `(,func . ,args) `(,func^ . ,args^))
           ))
   ((fresh (val val^ index index^) ;; Field getter
           (== exp `(@ ,val ,index))
           (== jexp (jderef (jget (jget val^ (jstr "public")) index^)))
           (parse-exp-listo `(,val ,index) `(,val^ ,index^))
           ))
   ((fresh (var val var^ val^) ;; Assignment
           (== exp `(:= ,var ,val))
           (== jexp (jassign var^ val^))
           (lh-parseo var var^)
           (parse-expo val val^)
           ))
   ))

(define (parse-foro exp jexp)
  (fresh (init init^ cond cond^ inc inc^ body body^ body^^)
         (== exp `(for (,init ,cond ,inc) . ,body))
         (== jexp (jbeg init^ (jcatch `break (jwhile cond^ (jbeg body^^ inc^)) `e (jundef))))
         (parse-expo cond cond^)
         (parse-listo `(,inc . (,init . ,body)) `(,inc^ . (,init^ . ,body^)))
         (begino body^ body^^)
         ))

(define (parse-listo lst jlst)
  (conde ((== lst `()) (== jlst `()))
         ((fresh (p p^ rest rest^)
                 (== lst `(,p . ,rest))
                 (== jlst `(,p^ . ,rest^))
                 (parseo p p^)
                 (parse-listo rest rest^)
                 ))))

(define (parse-exp-listo lst jlst)
  (conde ((== lst `()) (== jlst `()))
         ((fresh (p p^ rest rest^)
                 (== lst `(,p . ,rest))
                 (== jlst `(,p^ . ,rest^))
                 (parse-expo p p^)
                 (parse-exp-listo rest rest^)
                 ))))

(define (parseo-h exp jexp)
  (let ([exp (dehumanize exp)])
    (fresh (vars exp^ allocations body)
           (pull-names-listo `(,exp) vars)
           (allocato vars body jexp)
           (parseo exp body))))

(define (parseo-nh exp jexp)
  (fresh (vars exp^ allocations body)
         (pull-names-listo `(,exp) vars)
         (allocato vars body jexp)
         (parseo exp body)))

(define (lh-parseo exp jexp)
  (conde ((symbolo exp) (== jexp (jvar exp)))
         ((fresh (obj obj^ val val^)
                 (== exp `(@ ,obj ,val))
                 (== jexp (jget (jget obj^ (jstr "public")) val^))
                 (parse-exp-listo `(,obj ,val) `(,obj^ ,val^))
                 ))))


(define (objecto exp jexp)
  (fresh (pairs public)
         (== exp `(object . ,pairs))
         (== jexp (jset (jobj (list (cons (jstr "private") (jobj `())))) (jstr "public") public))
         (object-helpero pairs public)
         ))

(define (object-helpero exp-binds jexp)
  (conde ((== exp-binds `()) (== jexp (jobj `())))
         ((fresh (key value rest prev curr)
                 (== exp-binds `((,key ,value) . ,rest))
                 (== jexp (jset prev key (jall value)))
                 (object-helpero rest prev)))))

(define (varo exp jexp)
  (fresh (vars pairs jexp^)
         (== exp `(var . ,vars))
         (pull-pairso vars pairs)
         (conde ((== pairs `()) (== jexp (jundef)))
                ((== jexp (jbeg jexp^ (jundef)))
                 (=/= pairs `())
                 (pair-assigno pairs jexp^)
                 ))))

(define (functiono exp jexp)
  (fresh (params body body^ body^^ body^^^ body^^^^ vars vars^)
         (== exp `(function ,params . ,body))
         (== jexp (jset
                   (jset (jobj `()) (jstr "private")
                         (jset (jobj `()) (jstr "call") (jfun params (jlet `this (jall body^^^^) body^^^^)))) ;Temporary this binding, TODO: remove
                   (jstr "public") (jobj `())))
         (parse-listo body body^)
         (begino body^ body^^)
         (pull-names-listo body vars) ;; TODO: No effect
         (assigno params body^^ body^^^)
         (assigno vars body^^^ body^^^^)
         ))

(define (leto vars cont jexp)
  (fresh (k v rest let-rest)
         (== vars `((,k . ,v) . ,rest))
         (== jexp (jlet k v let-rest))
         (conde ((== rest `())
                 (== let-rest cont))
                ((=/= rest `())
                 (leto rest cont let-rest)))))

(define (begino lst jexp)
  (conde ((== lst `()) (== jexp (jundef)))
         ((== lst `(,jexp)))
         ((fresh (a rest rest-exp)
                 (== lst `(,a . ,rest))
                 (== jexp (jbeg a rest-exp))
                 (=/= rest `())
                 (begino rest rest-exp)
                 ))))

(define (switcho exp jexp)
  (fresh (pairs val val^)
         (== exp `(switch ,val . ,pairs))
         (parse-expo val val^)
         (switch-helpero pairs val^ jexp)))

(define (switch-helpero pairs val jexp)
  (conde ((== pairs `())
          (== jexp (jundef)))
         ((fresh (target body target^ body^ rest rest-exp)
                 (== pairs `((,target ,body) . ,rest))
                 (== jexp (jif (jdelta `=== `(,val ,target^)) body^ rest-exp))
                 (parse-expo target target^)
                 (parseo body body^)
                 (switch-helpero rest val rest-exp)
                 ))))

(define (pull-varo exp vars)
  (conde ((pull-var-expo exp vars))
         ((== exp `(var . ,vars)))
         ((fresh (x) (== exp `(return ,x))) (== vars `()))
         ((fresh (x) (== exp `(throw ,x))) (== vars `()))
         ((fresh (try-exp catch-exp label)
                 (== exp `(try ,try-exp catch ,label ,catch-exp))
                 (pull-varo-list `(,try-exp ,catch-exp) vars)))
         ((fresh (try-exp catch-exp finally-exp label)
                 (== exp `(try ,try-exp catch ,label ,catch-exp finally ,finally-exp))
                 (pull-varo-list `(,try-exp ,catch-exp ,finally-exp) vars)))
         ((fresh (cond then else)
                 (== exp `(if ,cond ,then ,else))
                 (pull-varo-list `(,then ,else) vars)))
         ((fresh (cond body temp temp^)
                 (== exp `(while ,cond . ,body))
                 (pull-var-expo cond temp)
                 (pull-varo-list body temp^)
                 (appendo temp temp^ vars)))
         ((fresh (t x)
                 (== exp `(switch ,t . ,x))
                 (pull-var-switcho x vars)))
         ((fresh (exps)
                 (== exp `(begin . ,exps))
                 (pull-varo-list exps vars)))
         ((fresh (init cond inc body)
                 (== exp `(for (,init ,cond ,inc) . ,body))
                 (pull-varo-list `(,init . ,body) vars)))
         ))

(define (pull-var-switcho switch vars)
  (conde ((== switch `()) (== vars `()))
         ((fresh (case code rest v v2)
                 (== switch `((,case ,code) . ,rest))
                 (pull-varo code v)
                 (appendo v v2 vars)
                 (pull-var-switcho rest v2)))))

(define (pull-var-expo exp vars)
  (conde ((== vars `())
          (conde ((fresh (x) (== exp `(number ,x))))
                 ((fresh (x) (== exp `(string ,x))))
                 ((fresh (x) (== exp `(object . ,x))))
                 ((== exp #t))
                 ((== exp #f))
                 ((== exp (jundef)))
                 ((== exp (jnul)))
                 ((symbolo exp))
                 ((fresh (erest) (== exp `(function . ,erest))))
                 ((== exp `(break)))
                 ((fresh (x) (== exp `(op . ,x))))
                 ((fresh (x) (== exp `(@ . ,x))))
                 ((fresh (x) (== exp `(:= . ,x))))
                 ))
         ((fresh (func args)
                 (== exp `(call ,func . ,args))
                 (pull-var-exp-listo args vars)))
         ))

(define (pull-varo-list exp-list vars)
  (conde ((== `(,exp-list . ,vars) `(() . ())))
         ((fresh (e erest v vrest)
                 (== exp-list `(,e . ,erest))
                 (pull-varo e v)
                 (pull-varo-list erest vrest)
                 (appendo v vrest vars))
          )))

(define (pull-var-exp-listo exp-list vars)
  (conde ((== `(,exp-list . ,vars) `(() . ())))
         ((fresh (e erest v vrest)
                 (== exp-list `(,e . ,erest))
                 (pull-var-expo e v)
                 (pull-var-exp-listo erest vrest)
                 (appendo v vrest vars))
          )))

(define (pull-pairso vars pairs)
  (conde ((== `(,vars . ,pairs) `(() . ())))
         ((fresh (var val name rest v-rest)
                 (== vars `(,var . ,v-rest))
                 (conde ((== var `(,name ,val))
                         (== pairs `((,name ,val) . ,rest)))
                        ((symbolo var)
                         (== pairs rest)))
                 (pull-pairso v-rest rest)))))

(define (pull-nameso vars names)
  (conde ((== `(,vars . ,names) `(() . ())))
         ((fresh (var v-rest name val rest)
                 (== vars `(,var . ,v-rest))
                 (== names `(,name . ,rest))
                 (conde ((== var `(,name ,val)))
                        ((symbolo var)
                         (== name var)))
                 (pull-nameso v-rest rest)))))

(define (pull-names-listo exp names)
  (fresh (vars)
         (pull-varo-list exp vars)
         (pull-nameso vars names)))

(define (pair-assigno pairs jexp)
  (fresh (var val var^ val^ rest jexp^ )
         (== pairs `((,var ,val) . ,rest))
         (lh-parseo var var^)
         (parse-expo val val^)
         (conde ((== rest `())
                 (== jexp (jassign var^ val^)))
                ((== jexp (jbeg (jassign var^ val^) jexp^))
                 (=/= rest `())
                 (pair-assigno rest jexp^)
                 ))))

(define (allocato list cont out)
  (conde ((== `(,list . ,out) `(() . ,cont)))
         ((fresh (a rest rest-padded)
                 (== list `(,a . ,rest))
                 (== out (jlet a (jall (jundef)) rest-padded))
                 (allocato rest cont rest-padded)))))

(define (assigno list cont out)
  (conde ((== `(,list . ,out) `(() . ,cont)))
         ((fresh (a b rest -rest rest-padded)
                 (== list `(,a . ,rest))
                 (== out (jlet a (jall (jvar a)) rest-padded))
                 (assigno rest cont rest-padded)))))

(define/match (mknum->num x)
  [((list)) 0]
  [((cons d rest)) (+ d (* 2 (mknum->num rest)))]
  [(_) (begin x)])

(define/match (dehumanize exp)
  [((? string?)) (jstr exp)]
  [((? integer?)) (jnum exp)]
  [((list)) `()]
  [((? list?)) (cons (dehumanize (car exp)) (dehumanize (cdr exp)))]
  [(_) exp])

(define/match (humanize exp)
  [((list `string x)) (list->string (map (compose integer->char mknum->num) x))]
  [((list `number x)) (mknum->num x)]
  [((list)) `()]
  [((? list?)) (cons (humanize (car exp)) (humanize (cdr exp)))]
  [(_) exp])
