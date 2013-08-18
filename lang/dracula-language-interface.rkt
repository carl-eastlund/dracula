(module dracula-language-interface (lib "a-unit.rkt")
  (require (lib "class.rkt")
           (lib "tool.rkt" "drscheme")
           "dracula-language-interface-sig.rkt")
  (import drscheme:tool^)
  (export dracula-language-interface^)
  
  (define dracula-language<%>
    (interface (drscheme:language:language<%>) dracula-language?))
  
  (define (dracula-language-default-mixin %)
    (if (implementation? % dracula-language<%>)
        %
        (class* % (dracula-language<%>)
          (define/public (dracula-language?) #f)
          (super-new))))
  )
