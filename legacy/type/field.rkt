#lang mischief

(require
  dracula/legacy/type/base
  dracula/legacy/type/description)

(struct field-type type [field] #:transparent)

(provide
  (contract-out
    (struct {field-type type}
      ([ref ref?]
       [field identifier?]))))
