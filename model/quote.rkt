#lang mischief

(provide
  quote-proof
  quote-program
  quote-environment
  quote-shape-map)

(require
  dracula/model/data
  dracula/proof/term
  mischief/preserve-expensive-metadata)

(define (quote-proof prog)
  (quote-defns prog))

(define (quote-program prog)
  (match! prog
    [(program defns)
     #`(program
         #,(quote-defns defns))]))

(define (quote-environment env)
  (match! env
    [(environment decls)
     #`(environment
         #,(quote-decls decls))]))

(define (quote-defns defns)
  (quote-list
    (for/list {[d (in-list defns)]}
      (quote-defn d))))

(define (quote-decls decls)
  (quote-list
    (for/list {[d (in-list decls)]}
      (quote-decl d))))

(define (quote-defn d)
  (match! d
    [(defn stx name body)
     #`(defn (quote-source #,stx)
         #,(quote-var name)
         #,(quote-term body))]
    [(in-theory stx theory)
     #`(in-theory (quote-source #,stx)
         #,(quote-theory theory))]
    [(include-book stx path options)
     #`(include-book (quote-source #,stx)
         (quote #,path)
         #,(quote-book-options options))]
    [(primitive stx name arity package symbol)
     #`(primitive (quote-source #,stx)
         #,(quote-var name)
         (quote #,arity)
         (quote #,package)
         (quote #,symbol))]
    [(encapsulate stx defns)
     #`(encapsulate (quote-source #,stx)
         #,(quote-defns defns))]
    [(skip-proofs stx defns)
     #`(skip-proofs (quote-source #,stx)
         #,(quote-defns defns))]
    [(assert-event stx body)
     #`(assert-event (quote-source #,stx)
         #,(quote-expr body))]
    [(validate stx fun inputs outputs)
     #`(validate (quote-source #,stx)
         #,(quote-addr fun)
         (quote #,inputs)
         (quote #,outputs))]
    [_ (quote-expr d)]))

(define (quote-decl d)
  (match! d
    [(decl stx name body)
     #`(decl (quote-source #,stx)
         #,(quote-var name)
         #,(quote-type body))]))

(define (quote-term t)
  (match! t
    [(stub-term stx arity)
     #`(stub-term (quote-source #,stx)
         (quote #,arity))]
    [(fun-term stx formals body measure goals)
     #`(fun-term (quote-source #,stx)
         #,(quote-vars formals)
         #,(quote-expr body)
         #,(quote-optional (and measure (quote-expr measure)))
         #,(quote-optional (and goals (quote-goals goals))))]
    [(thm-term stx formals body rules goals)
     #`(thm-term (quote-source #,stx)
         #,(quote-vars formals)
         #,(quote-expr body)
         #,(quote-optional (and rules (quote-rule-classes rules)))
         #,(quote-optional (and goals (quote-goals goals))))]
    [(inst-term stx labels names prog)
     #`(inst-term (quote-source #,stx)
         #,(quote-datum labels)
         #,(quote-vars names)
         #,(quote-program prog))]
    [(seal-term stx type term)
     #`(seal-term (quote-source #,stx)
         #,(quote-type type)
         #,(quote-term term))]
    [(gen-term stx formals domains body)
     #`(gen-term (quote-source #,stx)
         #,(quote-vars formals)
         #,(quote-types domains)
         #,(quote-term body))]
    [(app-term stx gen args)
     #`(app-term (quote-source #,stx)
         #,(quote-term gen)
         #,(quote-addrs args))]
    [(type-term stx type)
     #`(type-term (quote-source #,stx)
         #,(quote-type type))]
    [(or (deref _ _ _) (var _ _) (:: _ _))
     (quote-addr t)]))

(define (quote-types types)
  (quote-list
    (for/list {[t (in-list types)]}
      (quote-type t))))

(define (quote-type t)
  (match! t
    [(value-type stx)
     #`(value-type (quote-source #,stx))]
    [(stub-type stx arity)
     #`(stub-type (quote-source #,stx)
         (quote #,arity))]
    [(fun-type stx formals body measure)
     #`(fun-type (quote-source #,stx)
         #,(quote-vars formals)
         #,(quote-expr body)
         #,(quote-optional (and measure (quote-expr measure))))]
    [(thm-type stx formals body rules)
     #`(thm-type (quote-source #,stx)
         #,(quote-vars formals)
         #,(quote-expr body)
         #,(quote-optional (and rules (quote-rule-classes rules))))]
    [(inst-type stx labels names env)
     #`(inst-type (quote-source #,stx)
         #,(quote-datum labels)
         #,(quote-vars names)
         #,(quote-environment env))]
    [(type-type stx type)
     #`(type-type (quote-source #,stx)
         #,(quote-type type))]
    [(gen-type stx formals domains body)
     #`(gen-type (quote-source #,stx)
         #,(quote-vars formals)
         #,(quote-types domains)
         #,(quote-type body))]
    [(addr-type stx addr)
     #`(addr-type (quote-source #,stx)
         #,(quote-addr addr))]
    [(or (deref _ _ _) (var _ _) (:: _ _))
     (quote-addr t)]))

(define (quote-dict dict
          #:key quote-key
          #:value quote-value
          #:empty empty-stx)
  (for/fold
      {[stx empty-stx]}
      {[{key value} (in-dict dict)]}
    #`(dict-set #,stx
        #,(quote-key key)
        #,(quote-value value))))

(define (quote-shape-map shapes)
  (quote-dict shapes
    #:key quote-var
    #:value quote-shape
    #:empty #'(empty-shape-map)))

(define (quote-shape-list shape-list)
  (quote-list
    (for/list {[s (in-list shape-list)]}
      (quote-shape s))))

(define (quote-shape s)
  (match! s
    [(inst-shape stx labels names shapes)
     #`(inst-shape (quote-source #,stx)
         (quote #,labels)
         #,(quote-vars names)
         #,(quote-shape-map shapes))]
    [(rune-shape stx name formals)
     #`(rune-shape (quote-source #,stx)
         #,(quote-var name)
         #,(quote-optional (and formals (quote-vars formals))))]
    [(none-shape stx)
     #`(none-shape (quote-source #,stx))]))

(define (quote-exprs es)
  (quote-list
    (for/list {[e (in-list es)]}
      (quote-expr e))))

(define (quote-expr e)
  (match! e
    [(qu-expr stx value)
     #`(qu-expr (quote-source #,stx)
         (quote #,value))]
    [(if-expr stx test then else)
     #`(if-expr (quote-source #,stx)
         #,(quote-expr test)
         #,(quote-expr then)
         #,(quote-expr else))]
    [(let-expr stx lhs rhs body)
     #`(let-expr (quote-source #,stx)
         #,(quote-vars lhs)
         #,(quote-exprs rhs)
         #,(quote-expr body))]
    [(app-expr stx fun args)
     #`(app-expr (quote-source #,stx)
         #,(quote-addr fun)
         #,(quote-exprs args))]
    [(or (var _ _) (:: _ _)) (quote-var e)]))

(define (quote-theory theory)
  (match! theory
    [(enable stx runes)
     #`(enable (quote-source #,stx)
         #,(quote-addrs runes))]
    [(disable stx runes)
     #`(disable (quote-source #,stx)
         #,(quote-addrs runes))]))

(define (quote-goals goals)
  (quote-list
    (for/list {[g (in-list goals)]}
      (quote-goal g))))

(define (quote-goal g)
  (match! g
    [(goal stx name hints)
     #`(goal (quote-source #,stx)
         (quote #,name)
         #,(quote-hints hints))]))

(define (quote-hints hints)
  (quote-list
    (for/list {[h (in-list hints)]}
      (quote-hint h))))

(define (quote-hint h)
  (match! h
    [(by-hint stx lemma)
     #`(by-hint (quote-source #,stx)
         #,(quote-optional (and lemma (quote-lemma lemma))))]
    [(use-hint stx lemmas)
     #`(use-hint (quote-source #,stx)
         #,(quote-lemmas lemmas))]
    [(in-theory-hint stx theory)
     #`(in-theory-hint (quote-source #,stx)
         #,(quote-theory theory))]
    [(induct-hint stx scheme)
     #`(induct-hint (quote-source #,stx)
         #,(quote-expr scheme))]))

(define (quote-lemmas lemmas)
  (quote-list
    (for/list {[i (in-list lemmas)]}
      (quote-lemma i))))

(define (quote-lemma i)
  (match! i
    [(lemma stx fun args)
     #`(lemma (quote-source #,stx)
         #,(quote-addr fun)
         #,(quote-lemma-args args))]))

(define (quote-lemma-args args)
  (quote-list
    (for/list {[arg (in-list args)]}
      (quote-lemma-arg arg))))

(define (quote-lemma-arg arg)
  (match! arg
    [(named-actual stx name actual)
     #`(named-actual (quote-source #,stx)
         #,(quote-var name)
         #,(quote-expr actual))]
    [_ (quote-expr arg)]))

(define (quote-rule-classes rules)
  (quote-list
    (for/list {[r (in-list rules)]}
      (quote-rule-class r))))

(define (quote-rule-class r)
  (match! r
    [(rewrite stx corollary)
     #`(rewrite (quote-source #,stx)
         #,(quote-optional (and corollary (quote-expr corollary))))]
    [(forward-chaining stx corollary)
     #`(forward-chaining (quote-source #,stx)
         #,(quote-optional (and corollary (quote-expr corollary))))]
    [(elim stx corollary)
     #`(elim (quote-source #,stx)
         #,(quote-optional (and corollary (quote-expr corollary))))]
    [(type-prescription stx corollary)
     #`(type-prescription (quote-source #,stx)
         #,(quote-optional (and corollary (quote-expr corollary))))]
    [(linear stx corollary)
     #`(linear (quote-source #,stx)
         #,(quote-optional (and corollary (quote-expr corollary))))]
    [(definition stx corollary clique)
     #`(definition (quote-source #,stx)
         #,(quote-optional (and corollary (quote-expr corollary)))
         #,(quote-optional (and clique (quote-clique clique))))]
    [(induction stx corollary pattern condition scheme)
     #`(induction (quote-source #,stx)
         #,(quote-optional (and corollary (quote-expr corollary)))
         #,(quote-expr pattern)
         #,(quote-optional (and condition (quote-expr condition)))
         #,(quote-expr scheme))]))

(define (quote-clique c)
  (match! c
    [#false #'(quote #false)]
    [(clique stx funs)
     #`(clique (quote-source #,stx)
         #,(quote-vars funs))]
    [(controllers stx funs controlss)
     #`(controllers (quote-source #,stx)
         #,(quote-vars funs)
         (quote #,controlss))]))

(define (quote-book-options opts)
  (quote-list
    (for/list {[o (in-list opts)]}
      (quote-book-option o))))

(define (quote-book-option o)
  (match! o
    [(book-dir stx keyword)
     #`(book-dir (quote-source #,stx)
         (quote #,keyword))]
    [(skip-proofs-ok stx ?)
     #`(skip-proofs-ok (quote-source #,stx)
         (quote #,?))]))

(define (quote-vars vs)
  (quote-list
    (for/list {[v (in-list vs)]}
      (quote-var v))))

(define (quote-addrs addrs)
  (quote-list
    (for/list {[addr (in-list addrs)]}
      (quote-addr addr))))

(define (quote-addr addr)
  (match! addr
    [(deref stx comp label)
     #`(deref (quote-source #,stx)
         #,(quote-addr comp)
         (quote #,label))]
    [(or (var _ _) (:: _ _))
     (quote-var addr)]))

(define (quote-var v)
  (match! v
    [(var id labels)
     #`(var
         (quote-syntax/preserve-expensive-metadata #,id)
         (quote #,labels))]
    [(:: package symbol)
     #`(::
         (quote #,package)
         (quote #,symbol))]))

(define (quote-datum v)
  #`(quote #,v))

(define (quote-list stxs)
  #`(list #,@stxs))

(define (quote-optional stx/false)
  (match! stx/false
    [(? syntax? stx) stx]
    [#false #'#false]))

(define-shorthand (quote-source src)
  (quote-syntax/preserve-expensive-metadata src))
