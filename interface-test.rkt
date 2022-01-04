#lang racket/base

(module+ test
  (require
   cpsc411/test-suite/utils)

  (check-import-list
   "compiler.rkt"
   '(check-exprs-lang
     uniquify
     implement-safe-primops
     specify-representation
     remove-complex-opera*
     sequentialize-let
     impose-calling-conventions
     canonicalize-bind
     select-instructions
     expose-allocation-pointer
     uncover-locals
     undead-analysis
     conflict-analysis
     assign-call-undead-variables
     allocate-frames
     assign-registers
     assign-frame-variables
     replace-locations
     optimize-predicates
     implement-fvars
     expose-basic-blocks
     resolve-predicates
     flatten-program
     patch-instructions
     implement-mops
     generate-x64)))