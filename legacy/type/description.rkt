#lang mischief

(require
  dracula/legacy/type/base)

(struct description-type type
  [fields
   field~>role
   field~>type
   field~>name
   defns]
  #:transparent)

(provide
  (contract-out
    (struct {description-type type}
      ([ref ref?]
       [fields (listof identifier?)]
       [field~>role (dict/c identifier? (or/c 'defn 'decl 'refine))]
       [field~>type (dict/c identifier? type?)]
       [field~>name (dict/c identifier? identifier?)]
       [defns (listof syntax?)]))))
