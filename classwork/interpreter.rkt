#lang plait

(define-type Exp
  [num (n : Number)]
  [plus (left : Exp) (right : Exp)]
  [bool (b : Boolean)]
  [cnd (test_cond : Exp) (true_exp : Exp) (false_exp : Exp)]
  )

(calc : (Exp -> Number))

(define (calc e)
  (type-case Exp e
    [(num n) n]
    [(plus l r) (+ (calc l) (calc r))]
    [(cnd t te fe) (if (calc t) (calc te) (calc fe))]
 ))

(parse : (S-Exp -> Exp))

(define (parse s)
  (cond
    [(s-exp-number? s) (num (s-exp->number s))]
    [[s-exp-list? s]
     (let ([l (s-exp->list s)])
       (if (symbol=? '+ (s-exp->symbol (first l)))
           (plus (parse (second l)) (parse (third l)))
           (error 'parse "failed bc first element of list not added")))]))


;(run : (S-Exp -> Number))

;(define (run s-exp)
;  (parse s-exp))