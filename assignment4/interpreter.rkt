#lang plait

;; Let's define a function for our evaluator with appropriate types
(interp : (Exp Env -> Value))

;; Now let's write its body step by step with each new feature that we add
(define (interp e nv)
  (type-case Exp e
    [(numE n) (numV n)]
    [(boolE b) (boolV b)]
    [(plusE l r) (add (interp l nv) (interp r nv))]
    [(cndE c t e)
     (if (boolean-decision (interp c nv))
         (interp t nv)
         (interp e nv))]
    [(varE s) (lookup s nv)]
    [(let1E var val body)
     (let ([new-env (extend nv var (interp val nv))])
       (interp body new-env))]
    [(lamE v b) (funV v b nv)]            ;; Function definitions return functional values
    ;; Function application
    [(appE f a)
     (let ([fv (interp f nv)]
           [av (interp a nv)])
       (type-case Value fv
         [(funV v b nv-at-definition)
          (interp b (extend nv-at-definition v av))]
         [else (error 'app "didn't get a function")]))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;AST DEFINITION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Let's define a type for our Abstract Syntax Tree
(define-type Exp
  [numE (n : Number)]
  [boolE (b : Boolean)]
  [plusE (left : Exp) (right : Exp)]
  [cndE (test : Exp) (then : Exp) (else : Exp)]
  [varE (name : Symbol)]
  [let1E (var : Symbol) (value : Exp) (body : Exp)]
  [lamE (var : Symbol) (body : Exp)]
  [appE (fun : Exp) (arg : Exp)])

;; Let's define a type for the kinds of **values** our evaluator can produce *given* an AST
(define-type Value
  [numV (a-num : Number)]
  [boolV (a-bool : Boolean)]
  [funV (var : Symbol) (body : Exp) (nv : Env)])

;; Environment cache for keeping track of variable bindings
(define-type-alias Env (Hashof Symbol Value))
(define mt-env (hash empty))

;;;;;;;;;;;;;;;;;;;;;;;;;ENVIRONMENT HELPER FUNCS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; To evaluate the body of a let1 binding expression, we need to *extend*
;; our environment cache with the new binding first, then evaluate the body
(extend : (Env Symbol Value -> Env))
(define (extend old-env new-name value)
  (hash-set old-env new-name value))

;; We need another helper function to look up keys in our environment cache
(define (lookup (s : Symbol) (n : Env))
  (type-case (Optionof Value) (hash-ref n s)
    [(none) (error s "not bound")]
    [(some v) v]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;ADDITION HELPER FUNCS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; To handle addition between numbers or booleans, let's define a helper add
;; that figures out how to do the addition using type matching.
(define (add v1 v2)
  (type-case Value v1
    [(numV n1)
     (type-case Value v2
       [(numV n2) (numV (+ n1 n2))]
       [(boolV b2) (numV (+ n1 (if b2 1 0)))]
       [else (error '+ "RHS of addition must be a number or boolean")])]
    [(boolV b1)
     (type-case Value v2
       [(numV n2) (numV (+ (if b1 1 0) n2))]
       [(boolV b2) (numV (+ (if b1 1 0) (if b2 1 0)))]
       [else (error '+ "RHS of addition must be a number or boolean")])]
    [else (error '+ "LHS of addition must be a number or boolean")]))

;; Modify boolean-decision to handle numeric values for conditional expressions
(define (boolean-decision v)
  (type-case Value v
    [(boolV b) b]
    [(numV n)
     (cond
       [(> n 0) #t]
       [(= n 0) #f]
       [else (error 'if "Condition value cannot be less than 0")])]
    [else (error 'if "expects conditional to evaluate to a boolean or number")]))

