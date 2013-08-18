#lang racket/base
(require racket/contract
         racket/match
         racket/list
         syntax/boundmap
         racket/dict
         "../acl2/rep.rkt")

;; Compile-time representation of interfaces/modules.

;; A Static Module (SMod) is:
;;  (make-module/static ID ID Syntax (Listof SBody))
;; A Static Body (SBody) is: (Or Syntax SImPort SExPort)
;; A Static [Im/Ex]Port (S[Im/Ex]Port) is:
;;  (make-[im/ex]port/static Syntax SIfc (Listof (cons ID ID)) (Listof SPort))
;; A Static Interface (SIfc) is:
;;  (make-interface/static ID (Listof SDef) (Listof SInc))
;; A Static Include (SInc) is:
;;   (make-include/static SIfc)
;; A Static Definition (SDef) is one of:
;;   (make-fun/static ID (Listof ID) Syntax)
;;   (make-sig/static ID (Listof ID))
;;   (make-con/static ID)
;; A Static Specification (SSpec) is either SInc or SDef.

(define-struct module/static
  (name dynamic source body)
  #:prefab)
(define-struct port/static (source interface ext->int includes) #:prefab)
(define-struct (import/static port/static) () #:prefab)
(define-struct (export/static port/static) () #:prefab)
(define-struct interface/static (name defs incs) #:prefab)
(define-struct spec/static () #:prefab)
(define-struct (include/static spec/static) (ifc) #:prefab)
(define-struct (def/static spec/static) (name) #:prefab)
(define-struct (con/static def/static) (expr options) #:prefab)
(define-struct (fun/static def/static) (args) #:prefab)
(define-struct (sig/static fun/static) (results) #:prefab)
(define-struct (ind/static fun/static) (decls body) #:prefab)

;; Interface Implementation

(define current-loc (make-parameter #f))
(define current-id-mapping (make-parameter #f))

(define (interface/static-abstract i/s)
  (join-terms (map def/static-abstract
                   (interface/static-defs i/s))))

(define (interface/static-concrete i/s)
  (join-terms (filter-map def/static-concrete
                          (interface/static-defs i/s))))

(define (join-terms terms)
  (match terms
    [(list term) term]
    [_ (quasisyntax/loc (current-loc) (progn #,@terms))]))

(define (def/static-abstract s/s)
  (cond
   [(ind/static? s/s) (ind/static-induct s/s)]
   [(sig/static? s/s) (sig/static-stub s/s)]
   [(con/static? s/s) (con/static-axiom s/s)]))

(define (ind/static-induct fun)
  (quasisyntax/loc (current-loc)
    (skip-proofs
     (defun #,(map-ids (def/static-name fun))
       (#,@(fun/static-args fun))
       #,@(ind/static-decls fun)
       #,(map-ids (ind/static-body fun))))))

(define (sig/static-stub sig)
  (quasisyntax/loc (current-loc)
    (defstub #,(map-ids (def/static-name sig))
      (#,@(fun/static-args sig))
      #,(match (sig/static-results sig)
          [(? list? ids) #`(mv #,@ids)]
          [#f #'t]))))

(define (con/static-axiom con)
  (quasisyntax/loc (current-loc)
    (defaxiom #,(map-ids (def/static-name con))
      #,(map-ids (con/static-expr con))
      #,@(map-ids (con/static-options con)))))

(define (def/static-concrete s/s)
  (cond
   [(ind/static? s/s) (ind/static-fun s/s)]
   [(sig/static? s/s) #f]
   [(con/static? s/s) (con/static-theorem s/s)]))

(define (ind/static-fun ind)
  (quasisyntax/loc (current-loc)
    (defun #,(map-ids (def/static-name ind))
      (#,@(fun/static-args ind))
      #,@(ind/static-decls ind)
      #,(map-ids (ind/static-body ind)))))

(define (sig/static-let sig)
  (match (sig/static-results sig)
    [(? list? ids)
     (quasisyntax/loc (current-loc)
       (thm (mv-let (#,@ids)
                    (#,(map-ids (def/static-name sig))
                     #,@(fun/static-args sig))
                    (declare (ignore #,@ids))
                    t)))]
    [#f
     (quasisyntax/loc (current-loc)
       (thm (or t (#,(map-ids (def/static-name sig))
                   #,@(fun/static-args sig)))))]))

(define (con/static-theorem con)
  (quasisyntax/loc (current-loc)
    (defthm #,(map-ids (def/static-name con))
      #,(map-ids (con/static-expr con))
      #,@(map-ids (con/static-options con)))))

(define (map-ids v)
  (cond
   [(identifier? v) ((current-id-mapping) v)]
   [(syntax? v) (datum->syntax v (map-ids (syntax-e v)) v v v)]
   [(pair? v) (cons (map-ids (car v)) (map-ids (cdr v)))]
   [else v]))

(define (build-interface/static name specs)
  (let* ([incs (filter include/static? specs)]
         [defs (filter def/static? specs)])
    (make-interface/static name defs incs)))

(define (def->pair def)
  (cons (syntax-e (def/static-name def)) def))

(define (interface/static-sig-names i/s)
  (map def/static-name
       (filter fun/static? (interface/static-defs i/s))))

(define (interface/static-sig-args i/s)
  (map fun/static-args
       (filter fun/static? (interface/static-defs i/s))))

(define (interface/static-con-names i/s)
  (map def/static-name
       (filter con/static? (interface/static-defs i/s))))

(define (interface/static-includes i/s)
  (map include/static-ifc (interface/static-incs i/s)))

(define (port/static-sig-args port)
  (interface/static-sig-args (port/static-interface port)))

(define (port/static-con-names/external port)
  (interface/static-con-names (port/static-interface port)))

(define (port/static-sig-names/external port)
  (interface/static-sig-names (port/static-interface port)))

(define (assoc->rename assoc)
  (let* ([table (make-free-identifier-mapping)])
    (for ([(ext int) (in-dict assoc)])
      (let* ([old (free-identifier-mapping-get table ext (lambda () int))])
        (unless (free-identifier=? old int)
          (error 'assoc->rename
                 "duplicate renaming: ~s => ~s / ~s"
                 (syntax-e ext) (syntax-e old) (syntax-e int))))
      (free-identifier-mapping-put! table ext int))
    (lambda (id)
      (free-identifier-mapping-get table id (lambda () id)))))

(define (port/static-external->internal port)
  (assoc->rename (port/static-ext->int port)))

(define (port/static-external->internal/includes port)
  (assoc->rename (port/static-ext->int/includes port)))

(define include-table (make-hasheq))

(define (port/static-ext->int/includes port)
  (hash-ref! include-table port
    (lambda ()
      (append* (port/static-ext->int port)
               (map port/static-ext->int/includes
                    (port/static-includes port))))))

(define (port/static-con-names/internal port)
  (map (port/static-external->internal port)
       (port/static-con-names/external port)))

(define (port/static-sig-names/internal port)
  (map (port/static-external->internal port)
       (port/static-sig-names/external port)))

(define (import/static-abstract port)
  (parameterize ([current-loc (port/static-source port)]
                 [current-id-mapping
                  (port/static-external->internal/includes port)])
    (interface/static-abstract
     (port/static-interface port))))

(define (export/static-concrete port)
  (parameterize ([current-loc (port/static-source port)]
                 [current-id-mapping
                  (port/static-external->internal/includes port)])
    (interface/static-concrete
     (port/static-interface port))))

(define (port/static-external=? one two)
  (interface/static-external=? (port/static-interface one)
                               (port/static-interface two)))

(define (interface/static-external=? one two)
  (eq? one two))

(define (module/static-imports mod)
  (filter import/static? (module/static-body mod)))
(define (module/static-exports mod)
  (filter export/static? (module/static-body mod)))
(define (module/static-definitions mod)
  (filter syntax? (module/static-body mod)))

;; Exports

(provide/contract

  [module/static? (-> any/c boolean?)]
  [make-module/static
   (-> identifier?
     identifier?
     syntax?
     (listof (or/c syntax? port/static?))
     module/static?)]
  [module/static-name (-> module/static? identifier?)]
  [module/static-dynamic (-> module/static? identifier?)]
  [module/static-source (-> module/static? syntax?)]
  [module/static-body (-> module/static? (listof (or/c syntax? port/static?)))]
  [module/static-imports (-> module/static? (listof port/static?))]
  [module/static-exports (-> module/static? (listof port/static?))]
  [module/static-definitions (-> module/static? (listof syntax?))]

  [port/static? (-> any/c boolean?)]
  [import/static? (-> any/c boolean?)]
  [export/static? (-> any/c boolean?)]
  [make-import/static
   (->
     syntax?
     interface/static?
     (listof (cons/c identifier? identifier?))
     (listof import/static?)
     import/static?)]
  [make-export/static
   (->
     syntax?
     interface/static?
     (listof (cons/c identifier? identifier?))
     (listof port/static?)
     export/static?)]
  [port/static-source (-> port/static? syntax?)]
  [port/static-interface (-> port/static? interface/static?)]

  [port/static-external=? (-> port/static? port/static? boolean?)]
  [port/static-sig-names/internal (-> port/static? (listof identifier?))]
  [port/static-sig-names/external (-> port/static? (listof identifier?))]
  [port/static-sig-args (-> port/static? (listof (listof identifier?)))]
  [port/static-con-names/internal (-> port/static? (listof identifier?))]
  [port/static-con-names/external (-> port/static? (listof identifier?))]
  [import/static-abstract (-> import/static? syntax?)]
  [export/static-concrete (-> export/static? syntax?)]

  [interface/static? (-> any/c boolean?)]
  [rename build-interface/static make-interface/static
   (-> identifier? (listof spec/static?) interface/static?)]
  [interface/static-name (-> interface/static? identifier?)]
  [interface/static-external=? (-> interface/static? interface/static? boolean?)]
  [interface/static-includes (-> interface/static? (listof interface/static?))]

  [include/static? (-> any/c boolean?)]
  [make-include/static (-> interface/static? include/static?)]

  [ind/static? (-> any/c boolean?)]
  [make-ind/static
   (-> identifier? (listof identifier?) (listof syntax?) syntax? ind/static?)]

  [sig/static? (-> any/c boolean?)]
  [make-sig/static
   (-> identifier? (listof identifier?) (or/c (listof identifier?) #f)
     sig/static?)]

  [con/static? (-> any/c boolean?)]
  [make-con/static (-> identifier? syntax? syntax? con/static?)])
