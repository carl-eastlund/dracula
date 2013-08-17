#lang mischief

(provide

  (struct-out ::)

  (struct-out inst-shape)
  (struct-out rune-shape)
  (struct-out none-shape)

  source-of

  (struct-out ast)

  (struct-out defn)
  (struct-out in-theory)
  (struct-out include-book)
  (struct-out primitive)
  (struct-out encapsulate)
  (struct-out skip-proofs)
  (struct-out assert-event)
  (struct-out validate)

  (struct-out stub-term)
  (struct-out fun-term)
  (struct-out thm-term)
  (struct-out seal-term)
  (struct-out type-term)
  (struct-out gen-term)
  (struct-out app-term)

  (struct-out decl)

  (struct-out value-type)
  (struct-out stub-type)
  (struct-out fun-type)
  (struct-out thm-type)
  (struct-out type-type)
  (struct-out gen-type)
  (struct-out refine-type)
  (struct-out addr-type)

  (struct-out qu-expr)
  (struct-out if-expr)
  (struct-out let-expr)
  (struct-out app-expr)

  (struct-out enable)
  (struct-out disable)

  (struct-out rule-class)
  (struct-out rewrite)
  (struct-out forward-chaining)
  (struct-out elim)
  (struct-out type-prescription)
  (struct-out linear)
  (struct-out definition)
  (struct-out induction)

  (struct-out clique)
  (struct-out controllers)

  (struct-out goal)
  (struct-out by-hint)
  (struct-out use-hint)
  (struct-out in-theory-hint)
  (struct-out induct-hint)

  (struct-out lemma)
  (struct-out named-actual)

  (struct-out book-dir)
  (struct-out skip-proofs-ok)

  (struct-out deref)

  inst-term
  inst-term?
  inst-term-labels
  inst-term-names
  inst-term-program

  program
  program?
  program-defns

  inst-type
  inst-type?
  inst-type-labels
  inst-type-names
  inst-type-environment

  environment
  environment?
  environment-decls

  empty-environment
  environment-ref
  environment-set
  environment-update
  environment-keys
  environment-map
  environment-union
  environment-subtract

  empty-shape-map
  shape-map-ref
  shape-map-set
  shape-map-keys
  shape-map-union
  shape-map-subtract

  var
  var?
  var-id
  var-labels

  var->string
  vars->string
  label->string
  labels->string
  addr->string
  addrs->string)

(require
  (for-syntax
    mischief)
  dracula/proof/term
  unstable/list)

(struct ast [source] #:transparent)

(struct inst-shape ast [labels names shapes] #:transparent)
(struct rune-shape ast [name formals] #:transparent)
(struct none-shape ast [] #:transparent)

(struct defn ast [name term] #:transparent)
(struct in-theory ast [theory] #:transparent)
(struct include-book ast [path options] #:transparent)
(struct primitive ast [name arity package symbol] #:transparent)
(struct encapsulate ast [defns] #:transparent)
(struct skip-proofs ast [defns] #:transparent)
(struct assert-event ast [body] #:transparent)
(struct validate ast [fun inputs outputs] #:transparent)

(struct stub-term ast [arity] #:transparent)
(struct fun-term ast [formals body measure goals] #:transparent)
(struct thm-term ast [formals body rule-classes goals] #:transparent)
(struct seal-term ast [type term] #:transparent)
(struct type-term ast [type] #:transparent)
(struct gen-term ast [formals domains body] #:transparent)
(struct app-term ast [gen args] #:transparent)

(struct decl ast [name type] #:transparent)

(struct value-type ast [] #:transparent)
(struct stub-type ast [arity] #:transparent)
(struct fun-type ast [formals body measure] #:transparent)
(struct thm-type ast [formals body rule-classes] #:transparent)
(struct type-type ast [type] #:transparent)
(struct gen-type ast [formals domains body] #:transparent)
(struct refine-type ast [base labels addr] #:transparent)
(struct addr-type ast [addr] #:transparent)

(struct qu-expr ast [value] #:transparent)
(struct if-expr ast [test then else] #:transparent)
(struct let-expr ast [lhs rhs body] #:transparent)
(struct app-expr ast [fun args] #:transparent)

(struct enable ast [runes] #:transparent)
(struct disable ast [runes] #:transparent)

(struct rule-class ast [corollary] #:transparent)
(struct rewrite rule-class [] #:transparent)
(struct forward-chaining rule-class [] #:transparent)
(struct elim rule-class [] #:transparent)
(struct type-prescription rule-class [] #:transparent)
(struct linear rule-class [] #:transparent)
(struct definition rule-class [clique] #:transparent)
(struct induction rule-class [pattern condition scheme] #:transparent)

(struct clique ast [functions] #:transparent)
(struct controllers ast [functions flags] #:transparent)

(struct goal ast [name hints] #:transparent)

(struct by-hint ast [lemma] #:transparent)
(struct use-hint ast [lemmas] #:transparent)
(struct in-theory-hint ast [theory] #:transparent)
(struct induct-hint ast [scheme] #:transparent)

(struct lemma ast [rune args] #:transparent)
(struct named-actual ast [name actual] #:transparent)

(struct book-dir ast [keyword] #:transparent)
(struct skip-proofs-ok ast [ok] #:transparent)

(struct deref ast [comp label] #:transparent)

(define (source-of x)
  (match! x
    [(ast src) src]
    [(var id _) id]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programs, Environments, Instances

(struct program [defns] #:transparent)
(struct environment [decls] #:transparent)
(struct inst-term ast [labels names program] #:transparent)
(struct inst-type ast [labels names environment] #:transparent)

(define (empty-shape-map) (hash))

(define (empty-environment)
  (environment empty))

(define (shape-map-ref shapes name default)
  (dict-ref shapes name default))

(define (environment-ref env name default)
  (match! env
    [(environment decls)
     (or (for/first {[d (in-list decls)]
                     #:when (equal? (decl-name d) name)}
           (decl-type d))
       (if (procedure? default)
         (default)
         default))]))

(define (shape-map-set shapes name value)
  (dict-set shapes name value))

(define (shape-map-keys shapes)
  (dict-keys shapes))

(define (environment-set env name type)
  (match! env
    [(environment decls)
     (environment
       (append decls
         (list (decl #false name type))))]))

(define (environment-map env f)
  (match! env
    [(environment decls)
     (environment (map f decls))]))

(define (environment-keys env)
  (match! env
    [(environment decls)
     (map decl-name decls)]))

(define (environment-update env name0 f default)

  (define result
    (let/ec return

      (define (update decls)
        (match! decls
          ['() (return default)]
          [(cons (decl src name type) decls)
           (if (equal? name name0)
             (cons (decl src name (f type)) decls)
             (cons (decl src name type) (update decls)))]))

      (match! env
        [(environment decls)
         (environment (update decls))])))

  (if (procedure? result)
    (result)
    result))

(define (environment-union . es)
  (define decls (append-map environment-decls es))
  (cond
    [(check-duplicate (map decl-name decls)) =>
     (lambda {v}
       (error 'environment-union
         "duplicate declaration for ~a"
         (var->string v)))]
    [else (environment decls)]))

(define (environment-subtract e0 . es)
  (environment
    (remove* (append-map environment-decls es)
      (environment-decls e0))))

(define (shape-map-union . sms)
  (apply dict-add (empty-shape-map) sms))

(define (shape-map-subtract sm0 . sms)
  (apply dict-subtract sm0 sms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables

(struct variable [id labels]
  #:transparent
  #:methods gen:equal+hash
  [(define (equal-proc v1 v2 =?)
     (match*! {v1 v2}
       [{(variable id1 labels1)
         (variable id2 labels2)}
        (and (equal? labels1 labels2)
          (free-identifier=? id1 id2))]))
   (define (hash-proc v h) (free-identifier-hash-code (variable-id v) h))
   (define (hash2-proc v h) (h (variable-labels v)))])

(define var? variable?)
(define var-id variable-id)
(define var-labels variable-labels)

(define (make-var id [labels '()])
  (variable id labels))

(define-match-expander var
  (syntax-parser [(_ id-pat labels-pat) #'(variable id-pat labels-pat)])
  (rename-transformer #'make-var))

(define (sym->string label)
  (cond
    [(symbol-interned? label) (~s label)]
    [(symbol-unreadable? label)
     (format "~s:unreadable" label)]
    [else
     (format "~s:uninterned:~a" label (eq-hash-code label))]))

(define (label->string label)
  (sym->string label))

(define (labels->string labels)
  (list->phrase (map label->string labels)))

(define (split-addr addr [labels '()])
  (match! addr
    [(var _ labels0) (values addr (append labels0 labels))]
    [(deref _ comp label) (split-addr comp (cons label labels))]))

(define (addr->string addr)
  (define-values {base labels} (split-addr addr))
  (define sym0 (syntax-e (var-id base)))
  (define syms (cons sym0 labels))
  (define strs (map sym->string syms))
  (define str (string-join strs "."))
  (define src (source-of addr))
  (if (source-location-known? src)
    (format "~a [at ~a]" str (source-location->string src))
    (format "~a" str)))

(define (addrs->string addrs)
  (list->phrase (map addr->string addrs)))

(define (var->string v)
  (addr->string v))

(define (vars->string vs)
  (list->phrase (map var->string vs)))
