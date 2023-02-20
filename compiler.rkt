#lang racket

(require
 cpsc411/compiler-lib
 cpsc411/2c-run-time
 cpsc411/graph-lib)

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

 undead-effect

 ;allocate-fvars
 ;construct-registers

 compile-m2
 compile-m3)

;; STUBS; delete when you've begun to implement the passes or replaced them with
;; your own stubs.
(define-values (
                check-values-lang
                ;uniquify
                ;sequentialize-let
                ;normalize-bind
                ;select-instructions
                ;uncover-locals
                ;undead-analysis
                ;conflict-analysis
                ;assign-registers
                ;replace-locations
                ;assign-homes-opt
                ;assign-homes
                ;flatten-begins
                ;patch-instructions
                ;implement-fvars
                ;generate-x64

                ;compile-m2
                ;compile-m3
                )
  (values ; THIS ONE IS A FUNC CALL, DO NOT COMMENT OUT
    values
    ;values
    ;values
    ;values
    ;values
    ;values
    ;values
    ;values
    ;values
    ;values
    ;values
    ;values
    ;values
    ;values
    ;values
    ;values
    ;values
    ;values
    ))

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

  (replace-locations (assign-registers (conflict-analysis (undead-analysis (uncover-locals p))))))

; NOTES:
; - a location is UNDEAD (not definitely dead) between a DEFINITION and a REFERENCE
; - a location is DEAD between the final reference of a definition and a new definition

; - work backwards from the end of a program, looking for...
;   > References: location is definitely alive (its value is referenced)
;   > Definitions: "kills" a location, working backwards; the location is dead until the "previous reference"

; we need to reprsent the UNDEAD locations at each instruction, for conflict analysis
;   > Need map instruction to set
;   > A tree of sets works well 

; Input:    asm-lang-v2/locals
; Output:   asm-lang-v2/undead
; Purpose:  Performs undeadness analysis, decorating the program with undead-set tree. 
;           Only the info field of the program is modified.
(define (undead-analysis p)

  ; Decorate the program with the undead-out tree.
  (define (undead-p p) 
    (match p
      [`(module (,locals) ,tail)
       `(module (,locals (undead-out ,(rest (undead-tail tail))))  ; remove the undead-effect evaluation of the first effect
                 ,tail)]))
  
  ; Helper to return the input set of a tail instruction, i.e.
  ; the output set of the instruction prior to the tail.
  (define (get-tail-input h tailtree)
    (match h
      [`(halt ,triv)
        (if (aloc? triv)
            `(,triv)
            '())]
      [`(begin ,effects ... ,tail)
        (first tailtree)]))
  
  ; Helper: Given a list with an unknown level of nesting, 
  ; return the first list of alocs inside. E.g. (((x.1 y.1) (x.1)) (...)) -> (x.1 y.1)
  (define (get-first-entry lst)
   (match lst
     ['()
      '()]
     [`(,entry) #:when (aloc? entry)
       lst]
     [`(,entry ...) #:when (aloc? (first entry))
       lst]
     [_
       (get-first-entry (first lst))]))

  ; Takes a tail and produces the undead-set tree from the effects
  ; Returns a tree with an extra entry at the beginning, i.e. the input set of the first instruction.
  ; This entry is needed as the base case for nested instructions but will otherwise be removed.
  (define (undead-tail t)
    (match t
      [`(halt ,triv)       
        `(())]
      [`(begin ,effects ... ,tail)
        (let* ([tailtree (undead-tail tail)]   ; invariant: first entry in tree is output of current instruction.
               [basecase `(,(get-tail-input tail tailtree))]
               [effectstree (foldr (lambda (e tree)
                                           (let* ([eIn (undead-effect (get-first-entry tree) e)])
                                                  (if (and (> (length eIn) 0) (not (aloc? (first eIn))))
                                                      `(,(first eIn) ,(rest eIn) ,@(rest tree))      ; current effect is recursive
                                                       (cons eIn tree))))
                                   basecase
                                   effects)])
              (append effectstree
                      `(,(rest tailtree))))])) ; get rid of the first entry as it's no longer needed
  
  ; Calculate the undead input for a single effect e,
  ; given the output, undead-out.
  ; This is a list of alocs in the case of a single instruction
  ; and a list of lists in the case of a recursive (begin...), where the first entry is the extra info.
  (define (undead-effect undead-out e)
    (match e
      [`(set! ,aloc (,binop ,aloc ,triv))
        (if (number? triv)
            (set-add undead-out aloc)
            (set-add undead-out triv))]
      [`(set! ,aloc ,triv)
        (if (number? triv)
            (set-remove undead-out aloc)
            (set-remove (set-add undead-out triv) aloc))]
      [`(begin ,effects ...)
        (foldr (lambda (e tree) (cons (undead-effect (get-first-entry tree) e) tree))
                                `(,undead-out)
                                effects)]))

  (undead-p p))


; Decorates a program with its conflict graph.
(define (conflict-analysis p)

  ; Find the entries of lst that are not in excludeLst. The first entry of 
  ; excludeLst is the primary aloc with which to find conflicts.
  ; Update the graph with the conflicts and return it.
  (define (update-conflicts excludeLst lst graph)
    (for/fold ([g graph])
              ([conflict (filter (lambda (e) (not (and (member e excludeLst) #t))) 
                                  lst)])
              (add-edge g (first excludeLst) conflict)))

  (define (c-analysis-p p)
    (match p
      [`(module ((locals ,locals) (undead-out ,undead)) ,tail)
        `(module ((locals ,locals) 
                  (conflicts ,(c-analysis-t undead (new-graph locals) tail))) 
                 ,tail)]))
  
  ; undead : a nested list of lists of abstract locations such as x.1. 
  ; graph   : a graph of the conflicts found so far.
  ; Return a graph.
  (define (c-analysis-t undead graph t)
    (match t
      [`(begin ,effects ... ,tail)
        (c-analysis-t 
          (last undead)
          (for/fold ([g graph])  ; pair effects with entries in the undead list and update graph recursively.
                    ([eff effects] [currUndead undead])
                    (c-analysis-e currUndead g eff))
          tail)]                ; end with the tail.
      [`(halt ,triv)
        graph]))

  ; undead : the entry in the list of undead relating to the current effect
  ; graph   : a graph of the conflicts found so far.
  ; Return a graph.
  (define (c-analysis-e undead graph e)
    (match e
      [`(set! ,aloc ,num) #:when (number? num)
        (update-conflicts `(,aloc) undead graph)]
      [`(set! ,aloc1 ,aloc2) #:when (aloc? aloc2)
        (update-conflicts `(,aloc1 ,aloc2) undead graph)]
      [`(set! ,aloc1 (,binop ,aloc1 ,triv))
        (update-conflicts `(,aloc1) undead graph)]
      [`(begin ,effects ...)
        (for/fold ([g graph])  ; pair effects with entries in the undead list and update graph recursively.
                  ([eff effects] [currUndead undead])
                  (c-analysis-e currUndead g eff))]))

  (c-analysis-p p))

 
; Input:    asm-lang-v2/conflicts
; Output:   asm-lang-v2/assignments
; Purpose:  Performs graph-colouring register allocation. 
;           The pass attempts to fit each of the abstract location declared in the locals 
;           set into a register, and if one cannot be found, assigns it a frame variable instead.
(define (assign-registers p)
  ; a list consisting of r15 r14 r13 r9 r8 rdi rsi rdx rcx rbx rsp
  (define car (current-assignable-registers))

  ; generate a list of fvars from 0 to num
  (define (allocate-fvars num)
    (map make-fvar (range num)))

  ; allocate all registers and frame variables in order of usage
  (define (construct-registers conflicts)
    `(,@(reverse car) ,@(allocate-fvars (length conflicts))))

  ; splice the updated info block into the language
  (define (assign-p p)
    (match p
      [`(module ,info ,tail)
       `(module ,(assign-info info) ,tail)]))

  ; update the info block with new assignments
  (define (assign-info d)
    (let* ([conflicts   (sort-conflicts (first (dict-ref d 'conflicts)))]
           [locals      (dict-keys conflicts)]
           [assignments (reverse (generate-assignments locals conflicts '()))])
          (dict-set d 'assignment (list assignments))))

  ; generates assignments based on the alocs and their conflicts
  (define (generate-assignments locals conflicts assignments)
    (define registers (construct-registers conflicts))
    (if (empty? locals)
        '()
        (let* ([node            (first locals)]
               [node-conflicts  (first (dict-ref conflicts node))]
               [new-locals      (remove node locals)]
               [new-conflicts   (remove node conflicts)]
               [new-assignments (generate-assignments new-locals new-conflicts assignments)])
              (append (list (assign-node node node-conflicts new-assignments registers)) new-assignments))))

  ; assigns a single aloc to a register (or a frame variable if no registers are available)
  (define (assign-node node node-conflicts assignments registers)
    (define register (first registers))
    (if (ormap (lambda (x) (has-conflict node node-conflicts register x)) assignments)
        (assign-node node node-conflicts assignments (rest registers))
        `(,node ,register)))

  ; returns true if an aloc has a conflict with this specific register, otherwise false 
  (define (has-conflict node node-conflicts register assignment)
    (define a-aloc (first assignment))
    (define a-reg  (second assignment))
    (cond [(equal? a-reg register)
          (if (member a-aloc node-conflicts)
                #t
                #f)]
          [else #f]))

  ; sort a list of conflicts by degree
  (define (sort-conflicts c) 
    (sort c (lambda (a b) (< (length (second a)) (length (second b))))))

  (assign-p p))


; Compile while assigning all abstract locations to frame variables.
(define (compile-m2 p) 
  (parameterize ([current-pass-list
                  (list
                      check-values-lang
                      uniquify
                      sequentialize-let
                      normalize-bind
                      select-instructions
                      assign-homes  ; m2
                      flatten-begins
                      patch-instructions
                      implement-fvars
                      generate-x64
                      wrap-x64-run-time
                      wrap-x64-boilerplate)])
  (compile p)))


; Compile while using register allocation.
(define (compile-m3 p)
  (parameterize ([current-pass-list
                  (list
                      check-values-lang
                      uniquify
                      sequentialize-let
                      normalize-bind
                      select-instructions
                      assign-homes-opt ; m3
                      flatten-begins
                      patch-instructions
                      implement-fvars
                      generate-x64
                      wrap-x64-run-time
                      wrap-x64-boilerplate)])
  (compile p)))


; =============== Old Passes ================

; Input:   Values-lang-v3 (assumes a well-formed input)
; Output:  Values-unique-lang-v3
; Purpose: Compiles Values-lang v3 to Values-unique-lang v3 by resolving all lexical 
;          identifiers to abstract locations
(define (uniquify p) 

  ; Input: tail, a list of possibly nested let statements
  ; Output: Values-unique-lang-v3
  ; Purpose: matches on p, which is a list of (nested) let statements
  (define (uniquify-p p dict-acc)
    (match p
      [`(module ,e ...)
       `(module ,@(map (lambda (e) (uniquify-e e dict-acc)) e))]))

  ; Input: value, a let statement or a binary operation
  ; Output: uniquified version of this expression
  ; Purpose: recursively uniquify a single expression
  (define (uniquify-e e binds)
    (match e 
      [`(let ([,xs ,ps] ...) ,body)
        (define new-binds (construct-binds xs binds)) ; build binds for the current layer
        `(let ,(for/list ([x xs][p ps])
                        `[,(dict-ref new-binds x)     ; lookup and place freshly bound aloc'd names (i.e. the let statement at current scope)
                          ,(uniquify-e p binds)])     
              ,(uniquify-e body new-binds))]          ; recurse a level deeper (i.e. these are the nested let statements)

      [`(,binop ,triv1 ,triv2)
        `(,binop ,(uniquify-v triv1 binds) ,(uniquify-v triv2 binds))]
      
      [x (uniquify-v x binds)]))

  ; Input: aloc and binding dictionary
  ; Output: a new binding dictionary
  ; uses fresh to assign a unique aloc? to each name? at the current scope
  (define (construct-binds xs binds)
    (for/fold ([new-binds binds]) ; accumulator
              ([x xs])            ; x is an element, xs is the list
      (dict-set new-binds x (fresh x))))

  ; Input: triv and binding dictionary
  ; Output: int64 or aloc
  ; Purpose: return the triv itself if it is an int64, otherwise look up the bind in the dictionary
  (define (uniquify-v x binds)
    (match x
      [(? integer?) x]
      [(? symbol?) (lookup-bind x binds)]))

  ; Input: aloc
  ; Output: aloc
  ; Purpose: returns the mapped binding at the current scope (if it exists in the dictionary)
  (define (lookup-bind x binds)
    (if (and (name? x) (dict-has-key? binds x)) 
        (dict-ref binds x)                      
        x))                                     

  (uniquify-p p '()))


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
