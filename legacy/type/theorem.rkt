#lang mischief

(require
  dracula/legacy/type/base)

(struct theorem-type type [formals body rule-classes] #:transparent)

(provide
  (contract-out
    (struct {theorem-type type}
      ([ref ref?]
       [formals (listof identifier?)]
       [body syntax?]
       [rule-classes (or/c (listof syntax?) #false)]))))
