#lang mischief

(require
  dracula/legacy/type/base
  dracula/legacy/type/description)

(struct component-type type [description field~>type] #:transparent)

(provide
  (contract-out
    (struct {component-type type}
      ([ref ref?]
       [description description-type?]
       [field~>type (dict/c identifier? type?)]))))
