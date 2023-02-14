#lang racket

(require
 cpsc411/compiler-lib
 cpsc411/2c-run-time)

(provide
 check-values-lang
 uniquify
 sequentialize-let
 normalize-bind
 select-instructions
 uncover-locals
 undead-analysis
 conflict-analysis
 assign-registers
 replace-locations
 assign-homes-opt
 assign-homes
 flatten-begins
 patch-instructions
 implement-fvars
 generate-x64

 compile-m2
 compile-m3)

;; STUBS; delete when you've begun to implement the passes or replaced them with
;; your own stubs.
(define-values (check-values-lang
                ;uniquify
                ;sequentialize-let
                ;normalize-bind
                ;select-instructions
                ;uncover-locals
                undead-analysis
                conflict-analysis
                assign-registers
                ;replace-locations
                assign-homes-opt
                ;assign-homes
                ;flatten-begins
                ;patch-instructions
                ;implement-fvars
                ;generate-x64

                compile-m2
                compile-m3)
  (values ; THIS ONE IS A FUNC CALL, DO NOT COMMENT OUT
    values
    ;values
    ;values
    ;values
    ;values
    ;values
    values
    values
    values
    ;values
    values
    ;values
    ;values
    ;values
    ;values
    ;values
    values
    values))

; ================= Helpers =================

(define (address? a)
  (match a
    [`(rbp ,op ,int)
      #:when (and (member op '(- +))
                  (integer? int))
     #t]
    [_ #f]))

(define (splice-mapped-list list)
  ; splice things when an instruction is replaced by multiple instructions.
  (foldr
    (lambda (elem rest) `(,@elem ,@rest))
            '()
             list))


; =============== New Passes ================

(define (assign-homes-opt p)
  ; Compiles Asm-lang v2 to Nested-asm-lang v2, replacing each abstract location 
  ; with a physical location. This version performs graph-colouring register allocation.

  ; TODO
  p)


(define (undead-analysis p)
  ; Performs undeadness analysis, decorating the program with undead-set tree. 
  ; Only the info field of the program is modified.

  ; TODO
  p)


(define (conflict-analysis p)
  ; Decorates a program with its conflict graph.

  ; TODO
  p)


(define (assign-registers p)
  ; Performs graph-colouring register allocation. The pass attempts to fit each 
  ; of the abstract location declared in the locals set into a register, and if 
  ; one cannot be found, assigns it a frame variable instead.

  ; TODO
  p)


; =============== Old Passes ================

(define (uniquify p)
  ; Compiles Values-lang v3 to Values-unique-lang v3 by resolving 
  ; all lexical identifiers to abstract locations.
  
  (define (uniq-p p)
    (match p
      [`(module ,tail)
          `(module ,@(uniq-t #hash() tail))]))

  (define varcount 0)
  (define (assign-alocs x)
    ; Given a list of names, return a dictionary mapping
    ; each name to a unique aloc.
    (foldr (lambda (n dict) (set! varcount (+ varcount 1))
                            (dict-set dict n (string->symbol (format "~a.~a" n varcount))))
            #hash()
            x))
            
  (define (append-dicts d1 d2)
     ; Merge two dictionaries. Give precedence to keys in d2.
     (foldr (lambda (k dict) (if (not (dict-has-key? dict k)) (dict-set dict k (dict-ref d1 k))
                                                              dict))
            d2
            (dict-keys d1)))

  (define (uniq-t dict t)
    ; Return a list of instructions
    (match t
      [`(let ([,x ,value] ...) ,tail) 
          (let ([newDict (append-dicts dict (assign-alocs x))])
            `((let [,@(map (curry uniq-bind dict newDict) (map list x value))] ; zip the x and value lists
                     ,@(uniq-t newDict tail))))]
      [trivOrBinop
        `(,(uniq-v dict trivOrBinop))]))
  
  (define (uniq-bind parDict dict b)
    ; Return a list of instructions
    ; Values on RHS make use of the parent dict.
    (match b
      [`(,x ,triv) #:when (number? triv)
        `(,(dict-ref dict x) ,triv)]
      [`(,x ,triv) #:when (name? triv)
        `(,(dict-ref dict x) ,(dict-ref parDict triv))]
      [`(,x (,binop ,triv1 ,triv2)) #:when (and (member binop '(+ *)) #t)
        `(,(dict-ref dict x) (,binop ,(if (name? triv1) (dict-ref parDict triv1) triv1)
                                      ,(if (name? triv2) (dict-ref parDict triv2) triv2)))]
      [`(,x1 (let ([,x ,value] ...) ,val))
        (let ([newDict (append-dicts dict (assign-alocs x))])
          `(,(dict-ref dict x1)
            (let [,@(map (curry uniq-bind dict newDict) (map list x value))] ; zip the x and value lists
                 ,(uniq-v newDict val))))]))

  (define (uniq-v dict v)
    (match v
      [value #:when (number? value)
        value]
      [value #:when (name? value)
        (dict-ref dict value)]
      [`(,binop ,triv1 ,triv2) #:when (and (member binop '(+ *)) #t)
        `(,binop ,(if (name? triv1) (dict-ref dict triv1) triv1)
                ,(if (name? triv2) (dict-ref dict triv2) triv2))]
      [`(let ([,x ,value] ...) ,val) 
        (let ([newDict (append-dicts dict (assign-alocs x))])
          `(let [,@(map (curry uniq-bind dict newDict) (map list x value))] ; zip the x and value lists
                ,@(uniq-v newDict val)))]))

  (uniq-p p))


(define (sequentialize-let p)
  ; Compiles Values-unique-lang v3 to Imp-mf-lang v3 by picking a 
  ; particular order to implement let expressions using set!.

  (define (seq-p p)
    (match p
      [`(module ,tail)
          `(module ,@(seq-t tail))]))

  (define (seq-t t)
    ; Return a list of instructions
    (match t
      [`(let ([,aloc ,value] ...) ,tail) 
        `((begin ,@(map seq-bind (map list aloc value)) ; zip the aloc and value lists
                ,@(seq-t tail)))]
      [trivOrBinop
        `(,(seq-v trivOrBinop))]))
  
  (define (seq-bind b)
    ; Return an instruction
    (match b
      [`(,aloc ,triv) #:when (or (number? triv) (aloc? triv))
        `(set! ,aloc ,triv)]
      [`(,aloc (,binop ...)) #:when (and (member (first binop) '(+ *)) #t)
        `(set! ,aloc ,binop)]
      [`(,aloc1 (let ([,aloc ,value] ...) ,val))
        `(set! ,aloc1
               (begin ,@(map seq-bind (map list aloc value)) ; zip the aloc and value lists
                      ,(seq-v val)))]))

  (define (seq-v v)
    (match v
      [value #:when (or (number? value) (aloc? value))
        value]
      [`(,binop ...) #:when (and (member (first binop) '(+ *)) #t)
        binop]
      [`(let ([,aloc ,value] ...) ,val) 
        `(begin ,@(map seq-bind (map list aloc value)) ; zip the aloc and value lists
                      ,@(seq-v val))]))

  (seq-p p))


(define (normalize-bind p)
  ; Compiles Imp-mf-lang v3 to Imp-cmf-lang v3, pushing set! under begin so 
  ; that the right-hand-side of each set! is simple value-producing operation.

  (define (n-bind-p p)
    (match p
      [`(module ,tail)
          `(module ,@(n-bind-t tail))]))
  
  (define (n-bind-t t)
    ; Return a list of instructions
    (match t
      [`(begin ,eff ... ,tail)
       `((begin ,@(map n-bind-e eff) ,@(n-bind-t tail)))]
      [trivOrBinop
        `(,(n-bind-v trivOrBinop))]))
  
  (define (n-bind-v v)
    (match v
      [value #:when (or (number? value) (aloc? value))
        value]
      [`(,binop ...) #:when (and (member (first binop) '(+ *)) #t)
        binop]
      [`(begin ,eff ... ,val) 
        `(begin ,@(map n-bind-e eff) ,@(n-bind-v val))]))
  
  (define (n-bind-e e)
    ; Return a list of instructions
    (match e
      [`(set! ,aloc (begin ,eff ... ,val)) ; normalize so that begin is above set.
        `(begin ,@(map n-bind-e eff) ,(n-bind-e `(set! ,aloc ,(n-bind-v val))))]
      [`(set! ,aloc ,trivOrBinop) 
        `(set! ,aloc ,trivOrBinop)]
      [`(begin ,eff ...)
        `(begin ,@(map n-bind-e eff))]))

  (n-bind-p p))


(define (select-instructions p)
  ; Compiles Imp-cmf-lang v3 to Asm-lang v2, selecting appropriate sequences 
  ; of abstract assembly instructions to implement the operations of the source language.

  (define (sel-ins-p p)
    (match p
      [`(module ,tail)
        (if (or (number? tail) (aloc? tail))
          `(module () ,@(sel-ins-t tail)) ; dispense with 'begin' when top-level tail is a triv
          `(module () (begin ,@(sel-ins-t tail)))
          )]))
  
  (define (sel-ins-t t)
    (define getfresh (if (and (list? t) (and (member (first t) '(+ *)) #t)) (fresh) '_))
    ; Return a list of instructions
    (match t
      [`(begin ,eff ... ,tail)
       `(,@(splice-mapped-list (map sel-ins-e eff))
        ,@(sel-ins-t tail))] ; flattens nested begins
      [value #:when (or (number? value) (aloc? value))
        (sel-ins-v value '_)]
      [`(,binop ...) ; tail binop: need to create a temporary aloc
        `(,@(sel-ins-v binop getfresh) (halt ,getfresh))]))
  
  (define (sel-ins-v v aloc)
    (match v
      [value #:when (or (number? value) (aloc? value)) ; value is a triv
        `((halt ,value))]
      [`(,binop ...)
        (sel-ins-b binop aloc)]))
  
  (define (sel-ins-b b aloc)
    ; Return a sequence of instructions dealing with a binop,
    ; Given an aloc to use.
    ; e.g. sel-ins-b (+ 2 4) tmp.1 ->
    ; ((set! tmp.1 2) (set! tmp.1 (+ tmp.1 4)))
    (match b
      [`(,binop ,triv1 ,triv2)
           `((set! ,aloc ,triv1)
             (set! ,aloc (,binop ,aloc ,triv2)))]))
  
  (define (sel-ins-e e)
    ; Return a list of instructions
    (match e
      [`(set! ,aloc (,binop ...))
        (sel-ins-b binop aloc)]
      [`(begin ,eff ...)  ; collapse nesting
        `(,@(splice-mapped-list (map sel-ins-e eff)))]
      [`(set! ,aloc ,triv) `((set! ,aloc ,triv))])) ; do nothing

  (sel-ins-p p))


(define (assign-homes p)
  ; Compiles Asm-lang v2 to Nested-asm-lang v2, 
  ; replacing each abstract location with a physical location.
  (replace-locations (assign-fvars (uncover-locals p))))


(define (replace-locations p)
  ; Compiles Asm-lang v2/assignments to Nested-asm-lang v2, replacing each 
  ; abstract location with its assigned physical location from the assignment info field.

  (define (replace-loc-p p)
    (match p
      [`(module ((locals ,l) (assignment ,as)) ,tail)
       (replace-loc-t tail as)]))

  (define (get-repl aloc as)
    ; given an abstract location 'aloc' return its replacement as defined
    ; in the list of assignments 'as'.
    (second (first (filter (lambda (elem) (equal? (first elem) aloc))
                    as))))

  (define (replace-loc-t t as)
    ; return a tail with locations replaced.
    (match t
      [`(begin ,effects ... ,tail)
        `(begin ,@(map (curry replace-loc-e as) effects) ,(replace-loc-t tail as))]
      [`(halt ,triv)
        (if (aloc? triv) `(halt ,(get-repl triv as)) `(halt ,triv))]))

  (define (replace-loc-e as e)
    (match e
      [`(set! ,aloc ,triv) #:when (or (number? triv) (aloc? triv))
        `(set! ,(get-repl aloc as) ,(if (aloc? triv) (get-repl triv as) triv))]
      [`(set! ,aloc (,binop ...))
        `(set! ,(get-repl aloc as) ,(replace-loc-b binop as))]
      [`(begin ,effects ...)
        `(begin ,@(map (curry replace-loc-e as) effects))]))

  (define (replace-loc-b b as)
    (match b
      [`(,binop ,aloc1 ,triv)
          `(,binop ,(get-repl aloc1 as) ,(if (aloc? triv) (get-repl triv as) triv))]))

  (replace-loc-p p))


(define (assign-fvars p)
  ; Compiles Asm-lang v2/locals to Asm-lang v2/assignments, by assigning each 
  ; abstract location from the locals info field to a fresh frame variable.

  (define (assignf-p p)
    (match p
      [`(module ((locals ,locals)) ,tail)
       `(module ((locals ,locals) (assignment ,(create-assignments locals))) ,tail)]))
  
  (define (create-assignments locals)
    ; map each element of a list of locals to an fvar.
    (map (lambda (i) `(,(list-ref locals i) ,(fvar-from-i i)))
         (range 0 (length locals))))
  
  (define (fvar-from-i i)
    (string->symbol (format "fv~a" i)))

  (assignf-p p))


(define (uncover-locals p)
  ; Convert asm-lang-v2 into Asm-lang-v2/locals, analysing which 
  ; abstract locations are used in the program and decorating the
  ; program with the set of variables in an info field.

  (define (uloc-p p acc)
    (match p
      [`(module ,info ,tail)
       `(module ((locals ,(uloc-t tail acc))) ,tail)]))

  (define (uloc-t t acc)
    ; return a set of alocs.
    (match t
      [`(begin ,effect ... ,tail)
        (uloc-t 
          tail 
          (foldl (lambda (elem rest) (set-add rest elem)) 
            acc 
            (foldl append '() (map uloc-e effect))))]
      [`(halt ,triv)
        (if (aloc? triv) (set-add acc triv) acc)]))

  (define (uloc-e e)
    ; return: list of all alocs found in effect
    (match e
      [`(set! ,aloc ,triv) #:when (or (number? triv) (aloc? triv))
        (if (aloc? triv) `(,aloc ,triv) `(,aloc))]    
      [`(set! ,aloc (,binop ...))
        (uloc-b binop)]
      [`(begin ,effects ...)
        (foldl append '() (map uloc-e effects))]))

  (define (uloc-b b)
    ; return: list of all alocs found in binop
    (match b
      [`(,binop ,aloc1 ,triv)
          (if (aloc? triv) `(,aloc1 ,triv) `(,aloc1))]))

  (uloc-p p '()))


(define (flatten-begins p)
  ; Convert from nested-asm-lang-v2 to para-asm-lang-v2 by flattening all
  ; 'begins' expressions
  (define (flatten-p p)
    `(begin ,@(flatten-tail p)))
  
  (define (flatten-tail t)
    ; Return a list of flattened instructions.
    (match t
      [`(begin ,e ... ,tail)
          `(,@(splice-mapped-list (map flatten-effect e))
            ,@(flatten-tail tail))]
      [`(halt ,val) 
        `((halt ,val))]))
  
  (define (flatten-effect e)
    (match e
        [`(begin ,e ...)
            `(,@(splice-mapped-list (map flatten-effect e)))]
        [_ `(,e)]))

  (flatten-p p))


(define (patch-instructions p)
  ; Compiles Para-asm-lang v2 to Paren-x64-fvars v2 by patching instructions that have 
  ; no x64 analogue into a sequence of instructions.
  (define (patch-p p)
    (match p
      [`(begin ,e ... (halt ,triv))
       `(begin 
          ,@(foldr ; splice things when an instruction is replaced by multiple instructions.
            (lambda (elem rest) (if (list? (first elem)) `(,@elem ,@rest) `(,elem ,@rest)))
            '()
            (map patch-effect e))
          (set! ,(current-return-value-register) ,triv))]))

  (define (patch-effect e)
    (match e
      [`(set! ,fvar1 ,fvar2) #:when (and (fvar? fvar1) (fvar? fvar2))
          `((set! ,(first (current-patch-instructions-registers)) ,fvar2)
            (set! ,fvar1 ,(first (current-patch-instructions-registers))))]
      [`(set! ,fvar1 ,int64) #:when (and (not (int32? int64)) (int64? int64) )
          `((set! ,(first (current-patch-instructions-registers)) ,int64)
            (set! ,fvar1 ,(first (current-patch-instructions-registers))))]
      [`(set! ,fvar (,binop ...)) #:when (fvar? fvar)
          `((set! ,(first (current-patch-instructions-registers)) ,fvar)
            (set! ,(first (current-patch-instructions-registers)) ,(patch-binop binop))
            (set! ,fvar ,(first (current-patch-instructions-registers))))]
      [_ e]))  ; everything else

  (define (patch-binop b)
    (match b
      [`(,binop ,fvar ,val) #:when (fvar? fvar)
          `(,binop ,(first (current-patch-instructions-registers)) ,val)]
      [_ b]))  ; everything else
  
  (patch-p p))


(define (implement-fvars p)
  ; Compiles the Paren-x64-fvars v2 to Paren-x64 v2 by reifying fvars into displacement mode operands. 
  (define (f-program->p p)
    (match p
      [`(begin ,s ...)
       `(begin ,@(map f-statement->s s))]))
  
  (define (fvar->addr fvar)
    ; Convert an fvar into a statement of the form (currentfbp - offset)
    (list (current-frame-base-pointer-register) `- (* (fvar->index fvar) 8)))

  (define (f-statement->s s)
    ; Given a statement, convert addresses into fvars.
    (match s
      [`(set! ,fvar ,val) #:when (fvar? fvar)   ; (set! fvar <int32|reg>)
          `(set! ,(fvar->addr fvar) ,val)]       
      [`(set! ,reg ,fvar) #:when (fvar? fvar)   ; (set! reg fvar)
          `(set! ,reg ,(fvar->addr fvar))]
      [`(set! ,reg1 (,binop ...))               ; (set! reg1 (binop reg1 <int32|reg|fvar>))
          `(set! ,reg1 ,(f-binop->b binop))]
      [_ s]))  ; everything else

  (define (f-binop->b b)
    (match b
      [`(,binop ,r1 ,fvar) #:when (fvar? fvar)
          `(,binop ,r1 ,(fvar->addr fvar))]
      [_ b]))  ; everything else
       
  (f-program->p p))


(define (generate-x64 p)
  ; Paren-x64-v2 -> x64-instruction-sequence
  ; Given a paren-x64-v2 program, return a string of x64 instructions with each
  ; instruction on a new line.
  (define (program->x64 p)
    (match p
      [`(begin ,s ...)
       (foldr string-append "" (map statement->x64 s))]))

  (define (statement->x64 s)
    ; Given a statement, convert it into an x64 instruction string.
    (match s
      [`(set! (,addr ...) ,triv) (format "mov QWORD [~a], ~a\n" (loc->x64 addr) triv)]   ; (set! addr _ )
      [`(set! ,r1 ,r2) #:when (or (register? r2) (number? r2)) ; 	(set! reg triv)
        (format "mov ~a, ~a\n" r1 r2)]
      [`(set! ,r1 (,addr ...)) #:when (address? addr)
        (format "mov ~a, QWORD [~a]\n" r1 (loc->x64 addr))]
      [`(set! ,r1 ,r2) (binop->ins r2)]))

  (define (binop->ins b)
    (match b
      [`(,binop ,r1 ,r2) #:when (or (register? r2) (number? r2))
        (format "~a ~a, ~a\n" (if (equal? binop '+) "add" "imul") r1 r2)]
      [`(,binop ,r1 (,addr ...))
        (format "~a ~a, QWORD [~a]\n" (if (equal? binop '+) "add" "imul") r1 (loc->x64 addr))]))

  (define (loc->x64 loc)
    ; Convert an address (list) into a string.
    (match loc
      [`(,reg ,op ,off) (format "~a ~a ~a" reg op off)]))

  (program->x64 p))


(module+ test
  (require
   rackunit
   rackunit/text-ui
   cpsc411/langs/v3
   cpsc411/langs/v2-reg-alloc
   cpsc411/langs/v2
   cpsc411/test-suite/public/v3
   cpsc411/test-suite/public/v2-reg-alloc)

  ;; You can modify this pass list, e.g., by adding check-assignment, or other
  ;; debugging and validation passes.
  ;; Doing this may provide additional debugging info when running the rest
  ;; suite.
  ;; If you modify, you must modify the corresponding interpreter in the
  ;; interp-ls, at least by interesting #f as the interpreter for the new pass.
  ;; See the documentation for v3-public-test-suite for details on the structure
  ;; of the interpreter list.
  (current-pass-list (list
                      check-values-lang
                      uniquify
                      sequentialize-let
                      normalize-bind
                      select-instructions
                      assign-homes-opt
                      flatten-begins
                      patch-instructions
                      implement-fvars
                      generate-x64
                      wrap-x64-run-time
                      wrap-x64-boilerplate))

  (define interp-ls (list
                     interp-values-lang-v3
                     interp-values-lang-v3
                     interp-values-unique-lang-v3
                     interp-imp-mf-lang-v3
                     interp-imp-cmf-lang-v3
                     interp-asm-lang-v2
                     interp-nested-asm-lang-v2
                     interp-para-asm-lang-v2
                     interp-paren-x64-fvars-v2
                     interp-paren-x64-v2
                     #f #f))

  (run-tests (v3-public-test-sutie (current-pass-list) interp-ls))
  (run-tests (v2-reg-alloc-public-test-suite undead-analysis conflict-analysis assign-registers)))
