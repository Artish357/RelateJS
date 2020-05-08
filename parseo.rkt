#lang racket
(require "js-structures.rkt" "faster-miniKanren/mk.rkt" "evalo.rkt" "helpers.rkt")
(provide hoist-varo humanize dehumanize parseo/readable parse-topo)

; Parse a JavaScript statement with human-readable literals
(define (parseo/readable stmt jexpr)
  (parse-topo (dehumanize stmt) jexpr))

; Parse a JavaScript statement with relational number and string literals
(define (parse-topo stmt jexpr)
  (fresh (vars exp^ allocations body)
         (hoist-varo stmt vars)
         (allocateo vars body jexpr)
         (parseo stmt body)))

; Parse a JavaScript statement to LambdaJS expression
(define (parseo stmt jexpr)
  (conde
    ; expressions have a helper for their own
    ((parse-expro stmt jexpr))
    ; begin statement (Section 3.2.7)
    ((fresh (stmts jexprs begin-jexpr)
       (== stmt `(begin . ,stmts))
       (== jexpr (jbeg begin-jexpr (jundef)))
       (parse-listo stmts jexprs)
       (begino jexprs begin-jexpr)))
    ; if statement  (Section 3.2.7)
    ((fresh (cond-expr then-stmt else-stmt cond-jexpr then-jexpr else-jexpr)
       (== stmt `(if ,cond-expr ,then-stmt ,else-stmt))
       (== jexpr (jbeg (jif cond-jexpr then-jexpr else-jexpr) (jundef)))
       (parse-expro cond-expr cond-jexpr)
       (parse-listo `(,then-stmt ,else-stmt) `(,then-jexpr ,else-jexpr))))
    ; variable declaration (Section 3.2.1)
    ((fresh (vars bindings assignments-jexpr)
       (== stmt `(var . ,vars))
       (hoist-pairso vars bindings)
       (conde ((== bindings `()) (== jexpr (jundef)))
              ((== jexpr (jbeg assignments-jexpr (jundef)))
               (=/= bindings `())
               (pair-assigno bindings assignments-jexpr)))))
    ; for loops (Section 3.2.7)
    ((fresh (init-stmt init-jexpr
             cond-expr cond-jexpr
             inc-expr inc-jexpr
             body-stmts body-jexprs body-jexpr)
       (== stmt `(for (,init-stmt ,cond-expr ,inc-expr) . ,body-stmts))
       (== jexpr (jbeg init-jexpr
                       (jcatch `break
                               (jwhile cond-jexpr (jbeg body-jexpr inc-jexpr))
                               `e
                               (jundef))))
       (parse-expr-listo `(,cond-expr ,inc-expr) `(,cond-jexpr ,inc-jexpr))
       (parseo init-stmt init-jexpr)
       (parse-listo body-stmts body-jexprs)
       (begino body-jexprs body-jexpr)))
    ; return control effect (Section 3.2.8)
    ((fresh (val-expr val-jexpr)
       (== stmt `(return ,val-expr))
       (== jexpr (jthrow `return val-jexpr))
       (parse-expro val-expr val-jexpr)))
    ; throw control effect (Section 3.2.8)
    ((fresh (val-expr val-jexpr)  ;; throw
       (== stmt `(throw ,val-expr))
       (== jexpr (jthrow `error val-jexpr))
       (parse-expro val-expr val-jexpr)))
    ; break control effect (Section 3.2.8)
    ((== stmt `(break)) (== jexpr (jthrow `break (jundef))))
    ; try/catch control effect (Section 3.2.8)
    ((fresh (try-stmt try-jexpr catch-stmt catch-jexpr catch-var)
       (== stmt `(try ,try-stmt catch ,catch-var ,catch-stmt))
       (== jexpr
           (jbeg (jcatch `error try-jexpr catch-var
                         (jlet `((,catch-var . ,(jall (jvar catch-var))))
                               catch-jexpr))
                 (jundef)))
       (parseo try-stmt try-jexpr)
       (parseo catch-stmt catch-jexpr)))
    ; try/finally control effect (Section 3.2.8)
    ((fresh (try-stmt try-jexpr finally-stmt finally-jexpr)
       (== stmt `(try ,try-stmt finally ,finally-stmt))
       (== jexpr (jfin try-jexpr (jbeg finally-jexpr (jundef))))
       (parseo try-stmt try-jexpr)
       (parseo finally-stmt finally-jexpr)))
    ; try/catch/finally control effect (Section 3.2.8)
    ((fresh (try-stmt try-jexpr
             catch-stmt catch-jexpr catch-var
             finally-stmt finally-jexpr)
       (== stmt `(try ,try-stmt
                  catch ,catch-var ,catch-stmt
                  finally ,finally-stmt))
       (== jexpr (jbeg (jcatch `error try-jexpr catch-var
                               (jlet `((,catch-var . ,(jall (jvar catch-var))))
                                     catch-jexpr))
                       (jbeg finally-jexpr (jundef))))
       (parseo try-stmt try-jexpr)
       (parseo catch-stmt catch-jexpr)
       (parseo finally-stmt finally-jexpr)))
    ; while statements (Section 3.2.7)
    ((fresh (cond-expr cond-jexpr body-stmts body-jexprs body-jexpr)
       (== stmt `(while ,cond-expr . ,body-stmts))
       (== jexpr (jcatch `break (jwhile cond-jexpr body-jexpr) `e (jundef)))
       (parse-expro cond-expr cond-jexpr)
       (parse-listo body-stmts body-jexprs)
       (begino body-jexprs body-jexpr)))))

; Parse a JavaScript expression to LambdaJS expression
(define (parse-expro expr jexpr)
  (conde
   ;; variables (Section 3.2.1)
   ((symbolo expr ) (== jexpr (jderef (jvar expr ))))
   ;; simple literals
   ((conde ((== expr jexpr)
            (fresh (x) (conde ((== expr `(number ,x)))
                              ((== expr `(string ,x))))))
           ((== expr #t) (== jexpr (jbool #t)))
           ((== expr #f) (== jexpr (jbool #f)))
           ((== expr (jnul)) (== jexpr (jnul)))
           ((== expr (jundef)) (== jexpr (jundef)))))
   ;; builtin operations (Section 3.2.5)
   ((fresh (rator rands rand-jexprs)
      (== expr `(op ,rator . ,rands))
      (== jexpr (jdelta rator rand-jexprs))
      (parse-expr-listo rands rand-jexprs)))
   ;; assignments (Section 3.2.6)
   ((fresh (lhs-expr rhs-expr rhs-jexpr)
      (== expr `(:= ,lhs-expr ,rhs-expr))
      (conde ((symbolo lhs-expr)
              (== jexpr (jassign (jvar lhs-expr) rhs-jexpr))
              (parse-expro rhs-expr rhs-jexpr))
             ((fresh (obj-expr obj-jexpr key-expr key-jexpr)
                (== lhs-expr `(@ ,obj-expr ,key-expr))
                (== jexpr
                    (japp (jfun '(obj key rhs)
                                (jbeg (jassign
                                        (jvar 'obj)
                                        (jset (jderef (jvar 'obj))
                                              (jstr "public")
                                              (jset (jget (jderef (jvar 'obj))
                                                          (jstr "public"))
                                                    (jvar 'key) (jvar 'rhs))))
                                      (jvar 'rhs)))
                          (list obj-jexpr key-jexpr rhs-jexpr)))
                (parse-expro obj-expr obj-jexpr)
                (parse-expro key-expr key-jexpr)
                (parse-expro rhs-expr rhs-jexpr))))))
   ;; object field access (Section 3.2.2)
   ((fresh (obj-expr obj-jexpr field-expr field-jexpr)
      (== expr `(@ ,obj-expr ,field-expr))
      (== jexpr (jget (jget (jderef obj-jexpr) (jstr "public")) field-jexpr))
      (parse-expro obj-expr obj-jexpr)
      (parse-expro field-expr field-jexpr)))
   ;; Function call (Section 3.2.4)
   ((fresh (func-expr func-jexpr arg-exprs arg-jexprs)
      (== expr `(call ,func-expr . ,arg-exprs))
      (== jexpr (japp (jget (jget (jderef func-jexpr) (jstr "private"))
                            (jstr "call"))
                      arg-jexprs))
      (parse-expro func-expr func-jexpr)
      (parse-expr-listo arg-exprs arg-jexprs)))

   ;; object creation (Section 3.2.4)
   ((fresh (binding-exprs public-jexpr)
      (== expr `(object . ,binding-exprs))
      (== jexpr (jall (jset (jobj `((,(jstr "private") . ,(jobj '()))))
                            (jstr "public") public-jexpr)))
      (parse-obj-bindingso binding-exprs public-jexpr)))
   ;; function definition (Section 3.2.3)
   ((fresh (params
            body-stmts ; javascript statements
            body-jexprs ; a list of LambdaJS expressions
            body-jexpr ; a single LambdaJS begin expression
            body-jexpr/vars
            body-jexpr/vars+params
            vars
            hoisted-vars
            return-var)
      (== expr `(function ,params . ,body-stmts))
      (== jexpr
          (jall (jset (jobj `((,(jstr "public") . ,(jobj '()))))
                      (jstr "private")
                      (jset (jobj '()) (jstr "call")
                            (jfun params
                                  (jcatch 'return
                                          (jbeg body-jexpr/vars+params (jundef))
                                          'result
                                          (jvar 'result)))))))
      (hoist-var-listo body-stmts vars)
      (differenceo vars params hoisted-vars)
      (parse-listo body-stmts body-jexprs)
      (begino body-jexprs body-jexpr)
      (allocateo hoisted-vars body-jexpr body-jexpr/vars)
      (assigno params body-jexpr/vars body-jexpr/vars+params)))

   ;; Comma expression sequencing (not in paper since we can't decide on a name)
   ((fresh (exps exps^)
           (== expr `(comma . ,exps))
           (parse-expr-listo exps exps^)
           (begino exps^ jexpr)))))

; Parse a list of statements
(define (parse-listo stmts jexprs)
  (conde ((== stmts `()) (== jexprs `()))
         ((fresh (stmt jexpr stmts-rest jexprs-rest)
            (== stmts `(,stmt . ,stmts-rest))
            (== jexprs `(,jexpr . ,jexprs-rest))
            (parseo stmt jexpr)
            (parse-listo stmts-rest jexprs-rest)))))

; Parse a list of expressions
(define (parse-expr-listo exprs jexprs)
  (conde ((== exprs `()) (== jexprs `()))
         ((fresh (expr jexpr exprs-rest jexprs-rest)
            (== exprs `(,expr . ,exprs-rest))
            (== jexprs `(,jexpr . ,jexprs-rest))
            (parse-expro expr jexpr)
            (parse-expr-listo exprs-rest jexprs-rest)))))

; object {...}
(define (parse-obj-bindingso binding-exprs obj-jexpr)
  (conde ((== binding-exprs '()) (== obj-jexpr (jobj '())))
         ((fresh (field-expr field-jexpr val-expr val-jexpr
                  binding-exprs-rest obj-jexpr-rest)
            (== binding-exprs  `((,field-expr ,val-expr) . ,binding-exprs-rest))
            (== obj-jexpr (jset obj-jexpr-rest field-jexpr val-jexpr))
            (parse-expro field-expr field-jexpr)
            (parse-expro val-expr val-jexpr)
            (parse-obj-bindingso binding-exprs-rest obj-jexpr-rest)))))

; build nested LambdaJS begin out of a list of LambdaJS exprs
(define (begino jexprs jexpr)
  (conde ((== jexprs `()) (== jexpr (jundef)))
         ((== jexprs `(,jexpr)))
         ((fresh (first rest rest-jexpr)
            (== jexprs`(,first . ,rest))
            (=/= rest `())
            (== jexpr (jbeg first rest-jexpr))
            (begino rest rest-jexpr)))))

; Hoist variable declarations out of a statement
(define (hoist-varo stmt vars)
  (conde ((fresh (x)
            (== vars `())  ;; These never embed var declarations
            (conde ((== stmt `(return ,x)))
                   ((== stmt `(throw ,x)))
                   ((== stmt `(number ,x)))
                   ((== stmt `(string ,x)))
                   ((== stmt `(object . ,x)))
                   ((== stmt `(comma . ,x)))
                   ((== stmt #t))
                   ((== stmt #f))
                   ((== stmt (jundef)))
                   ((== stmt (jnul)))
                   ((symbolo stmt))
                   ((fresh (func args) (== stmt `(call ,func . ,args))))
                   ((fresh (erest) (== stmt `(function . ,erest))))
                   ((== stmt `(break)))
                   ((== stmt `(op . ,x)))
                   ((== stmt `(@ . ,x)))
                   ((== stmt `(:= . ,x))))))
         ((fresh (x) (== stmt `(var . ,x)) (hoist-nameso x vars)))
         ((fresh (try-stmt catch-stmt catch-var)
                 (== stmt `(try ,try-stmt catch ,catch-var ,catch-stmt))
                 (hoist-var-listo `(,try-stmt ,catch-stmt) vars)))
         ((fresh (try-stmt catch-stmt finally-stmt catch-var)
                 (== stmt `(try ,try-stmt catch ,catch-var ,catch-stmt finally ,finally-stmt))
                 (hoist-var-listo `(,try-stmt ,catch-stmt ,finally-stmt) vars)))
         ((fresh (cond then else)
                 (== stmt `(if ,cond ,then ,else))
                 (hoist-var-listo `(,then ,else) vars)))
         ((fresh (cond body)
                 (== stmt `(while ,cond . ,body))
                 (hoist-var-listo body vars)))
         ((fresh (exps)
                 (== stmt `(begin . ,exps))
                 (hoist-var-listo exps vars)))
         ((fresh (init cond inc body)
                 (== stmt `(for (,init ,cond ,inc) . ,body))
                 (hoist-var-listo `(,init . ,body) vars)))))

(define (hoist-var-listo stmts vars)
  (conde ((== stmts `()) (== vars `()))
         ((fresh (stmt stmts-rest vars-first vars-rest)
            (== stmts `(,stmt . ,stmts-rest))
            (hoist-varo stmt vars-first)
            (hoist-var-listo stmts-rest vars-rest)
            (appendo vars-first vars-rest vars)))))

(define (hoist-pairso vars pairs)
  (conde ((== vars `()) (== pairs `()))
         ((fresh (var val name rest v-rest)
                 (== vars `(,var . ,v-rest))
                 (conde ((== var `(,name ,val))
                         (== pairs `((,name ,val) . ,rest)))
                        ((symbolo var)
                         (== pairs rest)))
                 (hoist-pairso v-rest rest)))))

(define (hoist-nameso vars names)
  (conde ((== vars `()) (== names `()))
         ((fresh (var v-rest name val rest)
                 (== vars `(,var . ,v-rest))
                 (== names `(,name . ,rest))
                 (conde ((== var `(,name ,val)))
                        ((symbolo var)
                         (== name var)))
                 (hoist-nameso v-rest rest)))))

(define (pair-assigno pairs jexpr)
  (fresh (var val val^ rest cont)
         (== pairs `((,var ,val) . ,rest))
         (symbolo var)
         (parse-expro val val^)
         (conde ((== rest `())
                 (== jexpr (jassign (jvar var) val^)))
                ((=/= rest `())
                 (== jexpr (jbeg (jassign (jvar var) val^) cont))
                 (pair-assigno rest cont)))))

(define (allocateo list cont out)
  (conde ((== list `()) (== out cont))
         ((fresh (a rest rest-padded)
                 (== list `(,a . ,rest))
                 (== out (jlet `((,a . ,(jall (jundef)))) rest-padded))
                 (allocateo rest cont rest-padded)))))

(define (assigno list cont out)
  (conde ((== list `()) (== out cont))
         ((fresh (a b rest -rest rest-padded)
                 (== list `(,a . ,rest))
                 (== out (jlet `((,a . ,(jall (jvar a)))) rest-padded))
                 (assigno rest cont rest-padded)))))

; list (set) difference operation
(define (differenceo items toremove remaining)
  (conde ((== items `()) (== remaining items))
         ((fresh (el rest remaining-rest)
            (== items `(,el . ,rest))
            (conde ((== remaining `(,el . ,remaining-rest))
                    (not-in-listo el toremove)
                    (differenceo rest toremove remaining-rest))
                   ((membero el toremove)
                    (differenceo rest toremove remaining)))))))

(define/match (humanize stmt )
  [((list `string x)) (list->string (map (compose integer->char mknum->num) x))]
  [((list `number x)) (mknum->num x)]
  [((list)) `()]
  [((? list?)) (cons (humanize (car stmt )) (humanize (cdr stmt )))]
  [(_) stmt ])

(define/match (dehumanize stmt )
  [((? string?)) (jstr stmt )]
  [((? integer?)) (jnum stmt )]
  [((list)) `()]
  [((? list?)) (cons (dehumanize (car stmt )) (dehumanize (cdr stmt )))]
  [(_) stmt ])

(define/match (mknum->num x)
  [((list)) 0]
  [((cons d rest)) (+ d (* 2 (mknum->num rest)))]
  [(_) (begin x)])


