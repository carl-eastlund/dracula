#lang mischief

(provide
  proof->acl2)

(require
  dracula/model/data
  dracula/model/subst
  dracula/proof/term)

(define (proof->acl2 defns0)
  (define defns (simplify-defns defns0))
  (parameterize {[current-symbol-table (fresh-symbol-table)]}
    (bind-defns!/env defns (base-env))
    (define-values {env1 acl2}
      (defns->acl2 defns (base-env)))
    acl2))

(define (simplify-defns defns)
  (for/append {[d (in-list defns)]}
    (simplify-defn d)))

(define (simplify-defn d)
  (match! d
    [(defn _ _ _) (list d)]
    [(in-theory _ _) (list d)]
    [(include-book _ _ _) (list d)]
    [(assert-event _ _) (list d)]
    [(encapsulate src defns)
     (match! (simplify-defns defns)
       ['() '()]
       [defns (list (encapsulate src defns))])]
    [(skip-proofs src defns)
     (match! (simplify-defns defns)
       ['() '()]
       [defns (list (skip-proofs src defns))])]))

(define (bind-defns!/env defns env0)
  (match! defns
    ['() env0]
    [(cons d defns)
     (define env1 (bind-defns!/env defns env0))
     (define env2 (bind-defn!/env d env1))
     env2]))

(define (bind-defn!/env d env0)
  (match! d
    [(defn _ name _) (bind-sym name env0)]
    [(in-theory _ _) env0]
    [(include-book _ _ _) env0]
    [(assert-event _ _) env0]
    [(encapsulate _ defns)
     (combine-env env0
       (bind-defns!/env defns (base-env)))]
    [(skip-proofs _ defns)
     (bind-defns!/env defns env0)]))

(define (defns->acl2 defns env0)
  (for/fold/append-lists {[env env0]} {acl2} {[d (in-list defns)]}
    (defn->acl2 d env)))

(define (defn->acl2 d env0)
  (match! d
    [(defn _ name (stub-term _ arity))
     (define env1 (bind-sym name env0))
     (values
       env1
       (list (make-defstub (var->acl2 name env1) arity)))]
    [(defn _ name (fun-term _ formals body measure goals))
     (define env1 (bind-sym name env0))
     (define env2 (bind-syms formals env1))
     (values
       env1
       (list
         (make-defun (var->acl2 name env2) (vars->acl2 formals env2)
           (and measure (expr->acl2 measure env2))
           (expr->acl2 body env2)
           (and goals (goals->acl2 goals env2)))))]
    [(defn _ name (thm-term _ formals body rules goals))
     (define env1 (bind-sym name env0))
     (define env2 (bind-syms formals env1))
     (values
       env1
       (list
         (make-defthm (var->acl2 name env2)
           (expr->acl2 body env2)
           (and rules (rule-classes->acl2 rules env2))
           (and goals (goals->acl2 goals env2)))))]
    [(in-theory _ theory)
     (values
       env0
       (list (make-in-theory (theory->acl2 theory env0))))]
    [(include-book _ path options)
     (values
       env0
       (list (make-include-book path (book-options->acl2 options env0))))]
    [(assert-event src body)
     (values
       env0
       (list (make-assert (expr->acl2 body env0))))]
    [(encapsulate _ defns)
     (define-values {env1 contents}
       (defns->acl2 defns env0))
     (values
       env0
       (apply make-must-succeed* contents))]
    [(skip-proofs _ defns)
     (define-values {env1 acl2}
       (defns->acl2 defns env0))
     (values
       env1
       (apply make-progn*
         (map make-skip-proofs acl2)))]))

(define (exprs->acl2 es env0)
  (for/list {[e (in-list es)]}
    (expr->acl2 e env0)))

(define (expr->acl2 e env0)
  (match! e
    [(qu-expr _ value) (make-quote value)]
    [(if-expr _ test then else)
     (make-if (expr->acl2 test env0)
       (expr->acl2 then env0)
       (expr->acl2 else env0))]
    [(let-expr _ lhs rhs body)
     (define env1 (bind-syms lhs env0))
     (make-let
       (vars->acl2 lhs env1)
       (exprs->acl2 rhs env0)
       (expr->acl2 body env1))]
    [(app-expr _ fun args)
     (make-app (ref->acl2 fun env0)
       (exprs->acl2 args env0))]
    [(var _ _) (var->acl2 e env0)]))

(define (ref->acl2 f env0) (var->acl2 f env0))

(define (refs->acl2 fs env0)
  (for/list {[f (in-list fs)]}
    (ref->acl2 f env0)))

(define (theory->acl2 theory env0)
  (match! theory
    [(enable _ runes) (make-enable (runes->acl2 runes env0))]
    [(disable _ runes) (make-disable (runes->acl2 runes env0))]))

(define (goals->acl2 goals env0)
  (for/list {[g (in-list goals)]}
    (goal->acl2 g env0)))

(define (goal->acl2 g env0)
  (match! g
    [(goal stx name hints)
     (cons name (hints->acl2 hints env0))]))

(define (hints->acl2 hints env0)
  (for/append {[h (in-list hints)]}
    (hint->acl2 h env0)))

(define (hint->acl2 h env0)
  (match! h
    [(by-hint _ lemma)
     (if lemma
       (list '#:BY (lemma->acl2 lemma env0))
       (list '#:BY (make-nil)))]
    [(use-hint _ lemmas)
     (list '#:USE (lemmas->acl2 lemmas env0))]
    [(in-theory-hint _ theory)
     (list '#:IN-THEORY (theory->acl2 theory env0))]
    [(induct-hint _ scheme)
     (list '#:INDUCT (expr->acl2 scheme env0))]))

(define (lemmas->acl2 lemmas env0)
  (for/list {[i (in-list lemmas)]}
    (lemma->acl2 i env0)))

(define (lemma->acl2 i env0)
  (match! i
    [(lemma _ fun args)
     (list* '#:INSTANCE (ref->acl2 fun env0)
       (named-actuals->acl2 args env0))]))

(define (named-actuals->acl2 named-actuals env0)
  (for/list {[a (in-list named-actuals)]}
    (named-actual->acl2 a env0)))

(define (named-actual->acl2 a env0)
  (match! a
    [(named-actual _ formal actual)
     (list
       (var->acl2 formal env0)
       (expr->acl2 actual env0))]))

(define (rule-classes->acl2 rules env0)
  (for/list {[r (in-list rules)]}
    (rule-class->acl2 r env0)))

(define (rule-class->acl2 r env0)
  (match! r
    [(rewrite _ corollary)
     (make-rule-class '#:REWRITE
       '#:COROLLARY (and corollary (expr->acl2 corollary env0)))]
    [(forward-chaining _ corollary)
     (make-rule-class '#:FORWARD-CHAINING
       '#:COROLLARY (and corollary (expr->acl2 corollary env0)))]
    [(elim _ corollary)
     (make-rule-class '#:ELIM
       '#:COROLLARY (and corollary (expr->acl2 corollary env0)))]
    [(type-prescription _ corollary)
     (make-rule-class '#:TYPE-PRESCRIPTION
       '#:COROLLARY (and corollary (expr->acl2 corollary env0)))]
    [(linear _ corollary)
     (make-rule-class '#:LINEAR
       '#:COROLLARY (and corollary (expr->acl2 corollary env0)))]
    [(definition _ corollary clique)
     (apply make-rule-class '#:DEFINITION
       '#:COROLLARY (and corollary (expr->acl2 corollary env0))
       (clique->acl2 clique env0))]
    [(induction _ corollary pattern condition scheme)
     (make-rule-class '#:INDUCTION
       '#:COROLLARY (and corollary (expr->acl2 corollary env0))
       '#:PATTERN (expr->acl2 pattern env0)
       '#:CONDITION (and condition (expr->acl2 condition env0))
       '#:SCHEME (expr->acl2 scheme env0))]))

(define (clique->acl2 c env0)
  (match! c
    [#false empty]
    [(clique _ funs)
     (list '#:CLIQUE (refs->acl2 funs env0))]
    [(controllers _ funs controlss)
     (list
       '#:CLIQUE
       (refs->acl2 funs env0)
       '#:CONTROLLER-ALIST
       (for/list {[fun (in-list funs)]
                  [controls (in-list controlss)]}
         (cons (ref->acl2 fun env0)
           (for/list {[control (in-list controls)]}
             (if control (make-t) (make-nil))))))]))

(define (make-rule-class key . opts)
  (cons key (rule-class-options opts)))

(define (rule-class-options opts)
  (match! opts
    [(list) '()]
    [(list* _ #false opts) (rule-class-options opts)]
    [(list* key val opts) (list* key val (rule-class-options opts))]))

(define (runes->acl2 runes env0)
  (for/list {[r (in-list runes)]}
    (rune->acl2 r env0)))

(define (rune->acl2 r env0)
  (ref->acl2 r env0))

(define (book-options->acl2 opts env0)
  (for/append {[o (in-list opts)]}
    (book-option->acl2 o env0)))

(define (book-option->acl2 o env0)
  (match! o
    [(book-dir _ dir)
     (list '#:DIR (keyword-upcase dir))]
    [(skip-proofs-ok _ ?)
     (list '#:SKIP-PROOFS-OKP (if ? (make-t) (make-nil)))]))

(define (vars->acl2 vs env0)
  (for/list {[v (in-list vs)]}
    (var->acl2 v env0)))

(define (var->acl2 v env0)
  (match! v
    [(:: package symbol) (:: package symbol)]
    [(var _ _) (lookup-sym v env0)]))

(define (fresh-tag)
  (var (to-syntax (string->uninterned-symbol "EVENT-TAG"))))

(define current-symbol-table
  (make-parameter #false))

(define (fresh-symbol-table) (make-hash))

(define the-base-env
  (for/hasheq {[sym (in-list dracula-package-imports)]}
    (values sym (prefab ':: 'ACL2 sym))))

(define (base-env) the-base-env)
(define (combine-env one two)
  (dict-add one two
    #:combine (lambda {v1 v2} v1)))

(define (lookup-sym v env0)
  (define symbol-table (current-symbol-table))
  (dict-ref symbol-table v
    (lambda {}
      (wrong-syntax (source-of v)
        "cannot find ACL2 name for ~a among ~a"
        (var->string v)
        (vars->string (dict-keys symbol-table))))))

(define (bind-syms vs env0)
  (for/fold {[env env0]} {[v (in-list vs)]}
    (bind-sym v env)))

(define (bind-sym v env0)
  (define symbol-table (current-symbol-table))
  (dict-ref? symbol-table v
    #:success
    (lambda {sym}
      (dict-ref? env0 sym
        #:success
        (lambda {v2}
          (wrong-syntax (source-of v)
            "ACL2 name clash: both ~a and ~a have name ~s"
            (var->string v2)
            (var->string v)
            sym))
        #:failure
        (lambda {}
          (dict-set! symbol-table v sym)
          (dict-set env0 sym v))))
    #:failure
    (lambda {}
      (define sym0 (normalize-symbol (var->symbol v)))
      (define sym (unique-symbol sym0 #:for env0 #:try sym0))
      (dict-set! symbol-table v sym)
      (dict-set env0 sym v))))

(define (unique-symbol sym0 #:for env0
          #:count [n 1]
          #:try [sym (format-symbol "~a.~a" sym0 n)])
  (cond
    [(dict-has-key? env0 sym)
     (unique-symbol sym0 #:for env0 #:count (add1 n))]
    [else sym]))

(define (var->symbol v)
  (define syms (cons (syntax-e (var-id v)) (var-labels v)))
  (define strs (map symbol->string syms))
  (define str (string-join strs "."))
  (define sym (string->symbol str))
  sym)

(define (normalize-symbol sym)
  (string->symbol
    (normalize-string
      (symbol->string sym))))

(define (normalize-string str)
  (build-string (string-length str)
    (lambda {i}
      (normalize-char (string-ref str i)))))

(define (normalize-char c)
  (cond
    [(standard-char? c) (char-upcase c)]
    [else standard-filler-char]))

(define standard-filler-char #\_)
