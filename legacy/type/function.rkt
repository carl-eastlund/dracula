#lang mischief

(require
  dracula/legacy/type/base)

(struct function-type type [arity primitive] #:transparent)

(provide
  (contract-out
    (struct {function-type type}
      ([ref ref?]
       [arity nat/c]
       [primitive any/c]))))
