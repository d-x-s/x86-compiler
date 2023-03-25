#lang racket

(require
 cpsc411/compiler-lib
 cpsc411/2c-run-time
 cpsc411/graph-lib)

(provide
 uniquify
 sequentialize-let
 normalize-bind
 impose-calling-conventions
 select-instructions
 uncover-locals
 undead-analysis
 conflict-analysis
 assign-call-undead-variables
 allocate-frames
 assign-registers
 replace-locations
 assign-frame-variables
 implement-fvars
 optimize-predicates
 expose-basic-blocks
 resolve-predicates
 flatten-program
 patch-instructions

 generate-x64)

;; Stubs; remove or replace with your definitions.
(define-values (;uniquify
                ;sequentialize-let
                ;normalize-bind
                ;impose-calling-conventions
                ;select-instructions
                ;assign-homes-opt
                ;uncover-locals
                ;undead-analysis
                ;conflict-analysis
                ;assign-call-undead-variables
                ;allocate-frames
                ;assign-registers
                ;replace-locations
                ;assign-frame-variables
                ;implement-fvars
                ;optimize-predicates
                ;expose-basic-blocks
                ;resolve-predicates
                ;flatten-program
                ;patch-instructions
                ;generate-x64
                )
  (values   ; do not comment this one out 
   ; values ; uniquify
   ; values ; sequentialize-let
   ; values ; normalize-bind
   ; values ; impose-calling-conventions
   ; values ; select instructions
   ; values ; assign-homes-opt
   ; values ; uncover-locals
   ; values ; undead-analysis
   ; values ; conflict-analysis
   ; values ; assign-call-undead-variables
   ; values ; allocate-frames
   ; values ; assign-registers
   ; values ; replace-locations 
   ; values ; assign-frame-variables
   ; values ; implement-fvars 
   ; values ; optimize-predicates
   ; values ; expose-basic-blocks
   ; values ; resolve-predicates 
   ; values ; flatten-program
   ; values ; patch-instructions
   ; values ; generate-x64
   ))

; ================= Helpers =================

; return true if the value is in the list, false otherwise
(define (is-in-list list value)
  (cond
    [(empty? list) #f]
    [(equal? (first list) value) #t]
    [else (is-in-list (rest list) value)]))

; splice things when an instruction is replaced by multiple instructions.
(define (splice-mapped-list list)
  (foldr
    (lambda (elem rest) `(,@elem ,@rest))
            '()
             list))

; since dict-ref returns a list, extracts the first element of that list given a dict-ref with key
(define (extract-dict i key)
  (first (dict-ref i key)))

; return true if the value is an address (by the m6 definition), otherwise false
(define (address? addr)
  (and (list? addr)
        (frame-base-pointer-register? (first addr))
        (equal? '- (second addr))
        (dispoffset? (third addr))))

; return true if value is relop, false otherwise
(define (relop? relop)
  (or (equal? relop '<=)
      (equal? relop '< )
      (equal? relop '= )
      (equal? relop '>=)
      (equal? relop '> )
      (equal? relop '!=)))

; =============== M6 Passes ================

; Input: asm-pred-lang-v6/conflicts
; Output: asm-pred-lang-v6/pre-framed
; Purpose: Compiles Asm-pred-lang-v6/conflicts to Asm-pred-lang-v6/pre-framed by pre-assigning all 
;          variables in the call-undead sets to frame variables.

; Concept [similar to assign-registers]
; 1) If the input set is empty, return the default assignment
; 2) Choose a variable, "x", from the input set of variables
; 3) Recur with x removed from the input set and the conflict graph
;    > the recursive call should return an assignment for all the remaining variables
; 4) Select a COMPATIBLE frame variable for "x"
;    > compatible? a variable "x" is compatible with a frame location "fvar_i" if it is NOT directly in conflict with "fvar_i"
;                  AND  it is not in conflict with a variable y that has been assigned to "fvar_i"
;    > an easy way to find a compatibel frame variable is to find the set of frame variables to which x cannot be assigned (incompatible set)
;      starting from fv0, assign the first frame variable that is NOT in the incompatible set
;    > finally, add the assignment for the "x" to the result of the recursive call

; What is the purpose?
; > assigns each variable that is live across a call to a frame variable (instead of producign new moves)
; > results in a partial assignment of abstract locations to frame variables (register allocator works form here)

; Notes:
; > the default assignment for this pass is the empty assignment, since nothing has been assigned yet 
; > since many frame variables will be assigned prior to register allocation, we will modify assign-registers
;   down the pipeline to use a similar algorithm for spilling, instead of a naive algorithm that starts
;   spilling at frame variable "fv0" 
; > the interrogator will assign the variable with the least amount of conflicts first
(define (assign-call-undead-variables p)

  (define fvar_cap 50)

  ; generate a list of fvars from 0 to num
  (define (allocate-fvars num)
    (map make-fvar (range num)))

  ; splice the updated info block into the language
  (define (assign-call-undead-p p) 
    (match p
      [`(module ,info ,defs ... ,tail)
       `(module ,(assign-call-undead-info info) ,@(map (lambda (d) (assign-call-undead-block d)) defs) ,tail)]))

  ; generates the assignment for a single block
  (define (assign-call-undead-block d)
    (match d
      [`(define ,label ,info ,tail)
       `(define ,label ,(assign-call-undead-info info) ,tail)]))

  ; update the info block with new assignments
  (define (assign-call-undead-info i)
    (let* ([locals      (first (dict-ref i 'locals))]
           [conflicts   (first (dict-ref i 'conflicts))]
           [call-undead (first (dict-ref i 'call-undead))]
           [assignments (reverse (generate-assignments call-undead conflicts '()))])
          (dict-set (dict-set i 'locals (list (reverse (update-locals assignments locals)))) 'assignment (list assignments))))

  ; generates assignment based on the alocs and their conflicts
  (define (generate-assignments call-undead conflicts assignments)
    (define fvars (allocate-fvars fvar_cap))
    (if (empty? call-undead)
        '()
        (let* ([node            (first call-undead)]
               [node-conflicts  (if (dict-has-key? conflicts node) (first (dict-ref conflicts node)) '())]
               [new-call-undead (remove node call-undead)]
               [new-conflicts   (remove node conflicts)]
               [new-assignments (generate-assignments new-call-undead new-conflicts assignments)])
              (if (aloc? node)
                (append (list (assign-node node node-conflicts new-assignments fvars)) new-assignments)
                new-assignments))))

  ; assigns a single aloc to a frame variable
  (define (assign-node node node-conflicts assignments fvars)
    (define fvar (first fvars))
    (if (ormap (lambda (x) (has-conflict node node-conflicts fvar x)) assignments)
      (assign-node node node-conflicts assignments (rest fvars))
      `(,node ,fvar)))

  ; return true if:
  ; a) the fvar we are trying to assign this node is in the node's conflict list OR...
  ; b) some other assignment already uses this frame variable, and the nodes conflict with each other
  ; otherwise return false
  (define (has-conflict node node-conflicts fvar assignment)
    (define a-aloc (first assignment))
    (define a-fvar (second assignment))
    (cond [(member fvar node-conflicts) #t]
          [(equal? a-fvar fvar) (is-in-list node-conflicts a-aloc)]
          [else #f]))

  ; in the locals list, keep only the locals who have not been assigned yet
  (define (update-locals assignments locals)
    (define assignments-alocs (dict-keys assignments))
    (filter (lambda (x) (not (is-in-list assignments-alocs x))) locals))

  (assign-call-undead-p p))


; Input:   asm-pred-lang-v6/pre-framed
; Output:  asm-pred-lang-v6/framed
; Purpose: Compiles Asm-pred-lang-v6/pre-framed to Asm-pred-lang-v6/framed by allocating frames for 
;          each non-tail call, and assigning all new-frame variables to frame variables in the new frame.
(define (allocate-frames p) 
  
  (define bpr (current-frame-base-pointer-register))
  
  (define framesize 0)

  ; Return the integer x corresponding to the largest frame value fvx in assignment.
  ; if assignment is empty, return -1.
  (define (find-largest-frame assignment)
    (foldr (lambda (p rest) (max (fvar->index (second p)) rest)) 
                            -1
                            assignment))

  ; The size of a frame n (in slots) for a given non-tail call is the maximum of:
  ;   - the number of locations in the call-undead, or
  ;   - one more than the index of the largest frame location in the call-undead set.
  (define (find-framesize! info)
    (define call-undead (info-ref info 'call-undead))
    (define assignment (info-ref info 'assignment))
    (set! framesize (* (current-word-size-bytes) 
                       (max (length call-undead)
                            (+ 1 (find-largest-frame assignment))))))
  
  ; Create a new assignment for every entry fr in new-frames. 
  ; The assignment is (fr fvi) where the 'i's in assignment form an ascending sequence.
  ; Remove the assigned variables from the locals set
  ; Return the updated info.
  (define (update-info info)
    (define locals (info-ref info 'locals))
    (define assignment (info-ref info 'assignment))
    (define new-frames (flatten (info-ref info 'new-frames)))
    (define-values (x new-loc new-as)
      (for/fold ([i (+ 1 (find-largest-frame assignment))]
                 [loc locals]
                 [as assignment])
                ([f new-frames])
                (values (+ i 1) (set-remove loc f) (cons `(,f ,(make-fvar i)) as))))
    (define new-info (info-remove (info-remove (info-remove info 'new-frames) 'call-undead) 'undead-out))
    (info-set (info-set new-info 'locals new-loc) 'assignment new-as))

  (define (allocate-p p)
    (match p
      [`(module ,info ,defines ... ,tail)
        (find-framesize! info)
       `(module ,(update-info info)
                ,@(map allocate-def defines)
                ,(allocate-t tail))]))
  
  (define (allocate-def d)
    (match d
      [`(define ,label ,info ,tail)
        (find-framesize! info)
       `(define ,label ,(update-info info) ,(allocate-t tail))]))

  ; Return an instruction.
  (define (allocate-t t)
    (match t
      [`(begin ,effects ... ,tail)
       `(begin ,@(map allocate-e effects) ,(allocate-t tail))]
      [`(if ,pred ,tail1 ,tail2)
       `(if ,(allocate-pr pred) ,(allocate-t tail1) ,(allocate-t tail2))]
      [`(jump ,trg ,loc ...) 
        t]))

  ; Return an instruction
  (define (allocate-pr pr)
    (match pr
      [`(not ,pred)
       `(not ,(allocate-pr pred))]
      [`(begin ,effects ... ,pred)
       `(begin ,@(map allocate-e effects) ,(allocate-pr pred))]
      [`(if ,pred1 ,pred2 ,pred3)
       `(if ,(allocate-pr pred1) ,(allocate-pr pred2) ,(allocate-pr pred3))]
      [_ pr])) ; bool or relop

  ; Returns an instruction
  (define (allocate-e e)
    (match e
      [`(set! ,loc ,trivOrBinop)
        e]
      [`(begin ,effects ...)
       `(begin ,@(map allocate-e effects))]
      [`(if ,pred ,effect1 ,effect2)
       `(if ,(allocate-pr pred) ,(allocate-e effect1) ,(allocate-e effect2))]
      [`(return-point ,label ,tail)
       `(begin 
            (set! ,bpr (- ,bpr ,framesize))
            (return-point ,label ,(allocate-t tail))
            (set! ,bpr (+ ,bpr ,framesize)))]))

  (allocate-p p)) 


; Input: asm-pred-lang-v6/spilled
; Output: asm-pred-lang-v6/assignments
; Purpose: Compiles Asm-pred-lang-v6/spilled to Asm-pred-lang-v6/assignments 
;          by allocating all abstract locations in the locals set to free frame variables.
(define (assign-frame-variables p)

  (define fvar_cap 50)

  ; generate a list of fvars from 0 to num
  (define (allocate-fvars num)
    (map make-fvar (range num)))

  ; splice the updated info block into the language
  (define (assign-frame-vars-p p) 
    (match p
      [`(module ,info ,defs ... ,tail)
       `(module ,(assign-frame-vars-info info) ,@(map (lambda (d) (assign-frame-vars-block d)) defs) ,tail)]))

  ; generates the assignment for a single block
  (define (assign-frame-vars-block d)
    (match d
      [`(define ,label ,info ,tail)
       `(define ,label ,(assign-frame-vars-info info) ,tail)]))

  ; update the info block with new assignments
  (define (assign-frame-vars-info i)
    (let* ([locals      (first (dict-ref i 'locals))]
           [conflicts   (first (dict-ref i 'conflicts))]
           [assignments (reverse (generate-assignments locals conflicts '()))])
          (dict-set i 'assignment `((,@(extract-dict i 'assignment) ,@assignments)))))

  ; generates assignment based on the alocs and their conflicts
  (define (generate-assignments locals conflicts assignments)
    (define fvars (allocate-fvars fvar_cap))
    (if (empty? locals)
        '()
        (let* ([node            (first locals)]
               [node-conflicts  (if (dict-has-key? conflicts node) (first (dict-ref conflicts node)) '())]
               [new-locals      (remove node locals)]
               [new-conflicts   (remove node conflicts)]
               [new-assignments (generate-assignments new-locals new-conflicts assignments)]
               [assigned-node   (assign-node node node-conflicts new-assignments fvars)])
              (if (false? assigned-node)
                  new-assignments
                 (append (list assigned-node) new-assignments)))))

  ; assigns a single aloc to a frame variable
  (define (assign-node node node-conflicts assignments fvars)
    (define fvar (first fvars))
    (cond [(empty? assignments)
           (define try-assigned (try-assign-fvar node node-conflicts fvars))
           (if (false? try-assigned)
                #f
                try-assigned)]
          [else 
          (if (ormap (lambda (x) (has-conflict node node-conflicts fvar x)) assignments)
              (assign-node node node-conflicts assignments (rest fvars))
             `(,node ,fvar))]))

  ; return false if this node is incompatible with all fvars, otherwise assign it
  (define (try-assign-fvar node node-conflicts fvars)
    (cond [(empty? fvars) #f]
          [else
           (define fvar (first fvars))
           (if (member fvar node-conflicts)
               (try-assign-fvar node node-conflicts (rest fvars))
               `(,node ,fvar))]))

  ; return true if:
  ; a) the fvar we are trying to assign this node is in the node's conflict list OR...
  ; b) some other assignment already uses this frame variable, and the nodes conflict with each other
  ; otherwise return false
  (define (has-conflict node node-conflicts fvar assignment)
    (define a-aloc (first assignment))
    (define a-fvar (second assignment))
    (cond [(member fvar node-conflicts) #t]
          [(equal? a-fvar fvar) (is-in-list node-conflicts a-aloc)]
          [else #f]))

  (assign-frame-vars-p p))

; =============== M5 Passes ================

; Input:   proc-imp-cmf-lang-v6
; Output:  imp-cmf-lang-v6
; Purpose: Compiles Proc-imp-cmf-lang v6 to Imp-cmf-lang v6 by imposing calling conventions on all calls 
;          (both tail and non-tail calls), and entry points. The registers used to passing parameters are 
;          defined by current-parameter-registers, and the registers used for returning are defined by 
;          current-return-address-register and current-return-value-register.
(define (impose-calling-conventions p)

  (define cpr (current-parameter-registers))

  (define cfbp (current-frame-base-pointer-register))

  (define cprLen (length cpr))
  
  (define (impose-p p)
    (match p
      [`(module ,defines ... ,tail)
       `(module ,@(map impose-d defines) ,(impose-t tail))]))

  (define (impose-d d)
    (match d
      [`(define ,label (lambda (,alocs ...) ,tail))
       `(define ,label (begin ,@(map set-opands  (make-para-list (length alocs))  alocs) 
                              ,(impose-t tail)))]))

  (define (impose-t t)
    (match t
      [`(call ,triv ,opands ...)
       `(begin ,@(set-block opands)
               ,(create-jump triv (length opands)))]
      [`(begin ,effects ... ,tail)
        `(begin ,@effects ,(impose-t tail))]
      [`(if ,pred ,tail1 ,tail2)
        `(if ,pred ,(impose-t tail1) ,(impose-t tail2))]
      [value value]))

  (define (make-para-list len)
    (if (> len cprLen)
        (append cpr (build-list (- len cprLen) (lambda (x) (make-fvar x))))
        (take cpr len)))

  (define (set-block opands)
    (map set-opands (reverse opands) (reverse (make-para-list (length opands)))))

  (define (set-opands o r)
    `(set! ,r ,o))

  (define (create-jump t len)
    `(jump ,t ,cfbp ,@(make-para-list len)))  

  (impose-p p))

; =============== M4 Passes ================

; Input:   Nested-asm-lang-fvars v6
; Output:  Nested-asm-lang-fvars v6
; Purpose: Optimize Nested-asm-lang-fvars v6 programs by analyzing and simplifying predicates.
(define (optimize-predicates p)
  
  ; key-and-value-wise intersection of h0 with h1 and h2.
  (define (hash-intersects h0 h1 h2)
    (for/fold ([h #hash()])
              ([k (hash-keys h0)])
              (define currVal (dict-ref h0 k))
              (if (and (equal? (dict-ref h1 k) currVal) (equal? (dict-ref h2 k) currVal))
                  (dict-set h k currVal)
                  h)))

  ; key-and-value-wise intersection of h1 and h2
  (define (hash-intersect h1 h2)
    (for/fold ([h #hash()])
              ([k (hash-keys h1)])
              (define currVal (dict-ref h1 k))
              (if (equal? (dict-ref h2 k) currVal)
                  (dict-set h k currVal)
                  h)))

  (define (optimize-p p)
    (match p
      [`(module ,defines ... ,tail)
        (define-values (tailRes x) (optimize-t tail #hash()))
       `(module ,@(map optimize-def defines) ,tailRes)]))

  (define (optimize-def d)
    (match d
      [`(define ,label ,tail)
        (define-values (tailRes x) (optimize-t tail #hash()))
       `(define ,label ,tailRes)]))
  
  ; Return (values new-eff new-env)
  (define (optimize-t t env)
    (match t
      [`(jump ,trg)
        (values t env)]
      [`(begin ,effects ... ,tail)
        (define-values (effRes new-env)
          (for/fold ([acc '()] ; list of processed effects
                     [currEnv env]) 
                    ([e effects])
                    (define-values (new-eff eff-env) ; process the effect and get updated environment
                                   (optimize-e e currEnv))
                    (values (append acc `(,new-eff)) eff-env)))

        (define-values (tailRes x) (optimize-t tail new-env))
        (values `(begin ,@effRes ,tailRes) new-env)]
      [`(if ,pred ,tail1 ,tail2)
        (define-values (t1 x1) (optimize-t tail1 env))
        (define-values (t2 x2) (optimize-t tail2 env))
        (values (optimize-pred pred t1 t2 env) env)]))
  
  ; Return (values new-eff new-env)
  (define (optimize-e e env)
    (match e
      [`(set! ,loc_1 (,binop ,loc_1 ,triv))
        (define (interp-binop binop)
          (match binop
            ['* *]
            ['+ +]
            ['- -]))
        (define new-env (if (and (dict-has-key? env loc_1) (number? (dict-ref env loc_1)))
                            (if (number? triv)
                                (dict-set env loc_1 ((interp-binop binop) (dict-ref env loc_1) triv)) ; evaluate the binop
                                (if (and (dict-has-key? env triv) (number? (dict-ref env triv)))
                                    (dict-set env loc_1 ((interp-binop binop) (dict-ref env loc_1) (dict-ref env triv)))
                                    env))
                            env))
        
        
        (values e new-env)]
      [`(set! ,loc ,triv)
        (define new-env (if (dict-has-key? env triv) 
                            (dict-set env loc (dict-ref env triv)) 
                            (dict-set env loc triv)))
        (values `(set! ,loc ,triv) new-env)]
      [`(begin ,effects ...)
        (define-values (effRes new-env)
          (for/fold ([acc '()] ; list of processed effects
                     [currEnv env]) 
                    ([e effects])
                    (define-values (new-eff eff-env) ; process the effect and get updated environment
                                   (optimize-e e currEnv))
                    (values (append acc `(,new-eff)) eff-env)))
        (values `(begin ,@effRes) new-env)]
      [`(if ,pred ,effect1 ,effect2)
        (define-values (e1 env1) (optimize-e effect1 env))
        (define-values (e2 env2) (optimize-e effect2 env))
        (values (optimize-pred pred e1 e2 env) (hash-intersects env env1 env2))] ; Remove variables modified in eff1 and eff2 from env.
      [`(return-point ,label ,tail)
        (define-values (tailRes new-env) (optimize-t tail env))
        (values `(return-point ,label ,tailRes) 
                 (hash-intersect env new-env))]))

  ; Process a pred.
  ;   t1 : the already-processed true option
  ;   t2 : the already-processed false option
  ;   env : the environment of the pred
  (define (optimize-pred p t1 t2 env)
    (match p
      [`(begin ,effects ... ,pred)
        (define-values (effRes new-env)
          (for/fold ([acc '()] ; list of processed effects
                     [currEnv env]) 
                    ([e effects])
                    (define-values (new-eff eff-env) ; process the effect and get updated environment
                                   (optimize-e e currEnv))
                    (values (append acc `(,new-eff)) eff-env)))
        `(begin ,@effRes ,(optimize-pred pred t1 t2 new-env))]
      [`(if ,pred1 ,pred2 ,pred3)
        (optimize-pred
          pred1
          (optimize-pred pred2 t1 t2 env)
          (optimize-pred pred3 t1 t2 env)
          env)]
      [`(not ,pred)
        (optimize-pred pred t2 t1 env)]
      [`(,relop ,loc ,opand)
        (optimize-relop p t1 t2 env)]
      [`(false)
        t2]
      [`(true)
        t1]))
  
  ; Given a relop, return the branch that should replace it, otherwise return the original
  ; if-statement.
  ; expr: a relop expression (,relop ,loc ,opand)
  (define (optimize-relop expr t1 t2 env)
      (define (interp-relop relop)
        (match relop
          ['< <]
          ['<= <=]
          ['= =]
          ['!= (compose not =)]
          ['> >]
          ['>= >=]))
      
      (match expr
        [`(,relop ,loc ,opand) #:when (and (dict-has-key? env loc) (int64? (dict-ref env loc)))
          (if (int64? opand)
              (if ((interp-relop relop) (dict-ref env loc) opand)
                    t1
                    t2)
              (if (and (dict-has-key? env opand) (int64? (dict-ref env opand)))
                  (if ((interp-relop relop) (dict-ref env loc) (dict-ref env opand))
                      t1
                      t2)
                  `(if ,expr ,t1 ,t2)))]
        [_ `(if ,expr ,t1 ,t2)]))

  (optimize-p p))


; Input:   nested-asm-lang-v6
; Output:  block-pred-lang-v6
; Purpose: Compile the Nested-asm-lang v6 to Block-pred-lang v6,
;          eliminating all nested expressions by generating fresh basic blocks and jumps.
; M5 > M6 : all the instructions following a return point go into a new block.
(define (expose-basic-blocks p)

  ; a list of basic blocks to return (a mutable variable)
  (define result-acc '())
  
  ; Helper: adds a new basic block to the front of result-acc
  ;   label: the label to add to the block
  ;   body: the list of instructions to add to the block.
  (define (add-new-block! label body)
    (define corrected-body (if (equal? (length body) 1) (first body) `(begin ,@body)))
    (set! result-acc `((define ,label ,corrected-body) ,@result-acc)))

  (define (expose-p p)
    (match p
      [`(module ,defines ... ,tail)
        (map expose-def! (reverse defines))
        (define tailRes (expose-t tail))
        (add-new-block! (fresh-label '__main) tailRes)
       `(module ,@result-acc)]))

  (define (expose-def! d)
    (match d
      [`(define ,label ,tail)
        (define tailRes (expose-t tail))
        (add-new-block! label tailRes)]))

  ; Given a tail, recursively generate blocks. 
  ; Returns the tail block instructions to be used to make a block by the caller
  ; of this function. 
  (define (expose-t t)
    (match t
      [`(jump ,trg) `(,t)]
      [`(begin ,effects ... ,tail)
        (expose-effects effects (expose-t tail))]
      [`(if ,pred ,tail1 ,tail2)
        (define tailBody1 (expose-t tail1))
        (define tailBody2 (expose-t tail2))
        (define true_label (fresh-label '__nested))
        (define false_label (fresh-label '__nested))
        (add-new-block! false_label tailBody2)
        (add-new-block! true_label tailBody1)
        `(,(expose-pred pred true_label false_label))]))

  ; Walk through a list of effects until a predicate or return-point
  ; is encountered, then create a block and continue walking.
  ;    tail: the instruction at the end of the list of effects from whence this effect came
  ; Returns the list of block instructions to be used to make a block by the caller
  ; of this function.
  (define (expose-effects effects tail)
    (match effects
      ['() tail]
      [(cons effect effects)
       (expose-e effect effects tail)]))

  ; Process an effect.
  ; tail: the processed instruction at the end of the list of effects from whence this effect came
  ; effRest: the rest of the parent effect list after the current effect
  ; Returns a list of instructions.
  (define (expose-e e effRest tail)
    (match e
      [`(set! ,loc ,trivOrBinop)
        (cons e (expose-effects effRest tail))]
      [`(begin ,effects ...) ; flatten nested effects
        (expose-effects (append effects effRest) tail)]
      [`(if ,pred ,effect1 ,effect2)
        (define restResult (expose-effects effRest tail))
        
        (define true_label (fresh-label 'tmp))
        (define false_label (fresh-label 'tmp))
        (define join_label (fresh-label 'tmp))  ; join label: the block where effect1 and effect2 will converge
        
        (define eff1Res (expose-e effect1 '() `((jump ,join_label))))
        (define eff2Res (expose-e effect2 '() `((jump ,join_label))))
        
        (add-new-block! join_label restResult)
        (add-new-block! false_label eff2Res)
        (add-new-block! true_label eff1Res)
        
       `(,(expose-pred pred true_label false_label))]
      [`(return-point ,label ,rtail)
        (define restResult (expose-effects effRest tail))
        (define tailBody (expose-t rtail))

        (add-new-block! label restResult) ; give the label to the next block
        tailBody])) ; return the processed tail to be appended to the current block

  ; Given a pred, return an instruction.
  ; Add blocks for recursive preds.
  (define (expose-pred pr k-true k-false)
    (match pr
      [`(not ,pred)
        (expose-pred pred k-false k-true)] ; switch true and false
      [`(if ,pred ,pred1 ,pred2)
        (define tailbody1 `(,(expose-pred pred1 k-true k-false)))
        (define tailbody2 `(,(expose-pred pred2 k-true k-false)))
        (define true_label (fresh-label 'tmp))
        (define false_label (fresh-label 'tmp))
        (add-new-block! false_label tailbody2)
        (add-new-block! true_label tailbody1)
        (expose-pred pred true_label false_label)]
      [`(begin ,effects ... ,pred)
       `(begin ,@(expose-effects effects `(,(expose-pred pred k-true k-false))))]
      [_ ; relop or bool. Note: This is a tail
       `(if ,pr (jump ,k-true) (jump ,k-false))]))

  (expose-p p))
  

; Input:   block-pred-lang-v5
; Output:  block-asm-lang-v4
; Purpose: Compile the Block-pred-lang v4 to Block-asm-lang v4 by manipulating the 
;          branches of if statements to resolve branches.
(define (resolve-predicates p)
  (define (resolve-p p) 
    (match p
      [`(module ,bs ...)
       `(module ,@(resolve-bs bs))]))

  (define (resolve-bs bs)
    (map resolve-b bs))

  (define (resolve-b b)
    (match b 
      [`(define ,label ,tail)
        `(define ,label ,(resolve-t tail))]))

  (define (resolve-t t)
    (match t 
      [`(halt ,opand)
       `(halt ,opand)]

      [`(jump ,trg)
       `(jump ,trg)]

      [`(begin ,effects ... ,tail)
       `(begin ,@effects ,(resolve-t tail))]

      [`(if ,pred (jump ,trg1) (jump ,trg2))
        (match pred
          [`(,relop ,loc ,opand) `(if ,pred (jump ,trg1) (jump ,trg2))]
          [`(true)               `(jump ,trg1)]
          [`(false)              `(jump ,trg2)]
          [`(not ,npred)          (resolve-t `(if ,npred (jump ,trg2) (jump ,trg1)))])]))

  (resolve-p p))


; Input:   block-asm-lang-v4
; Output:  para-asm-lang-v4
; Purpose: Compile Block-asm-lang v4 to Para-asm-lang v4 by flattening basic blocks into labeled instructions.
(define (flatten-program p)

  (define (flatten-p p)
    (match p
     [`(module ,bs ...)
      `(begin ,@(flatten-bs bs))]))

  (define (flatten-bs bs)
    (for/fold ([flt '()])
              ([b   bs])
              (flatten-b b flt)))
  
  (define (flatten-b b acc)
    (match b
      [`(define ,label ,tail)
        (let ([ft (flatten-t tail)])
          (append acc `((with-label ,label ,(first ft))) (rest ft)))]))

  (define (flatten-t t)
     (match t
      [`(halt ,opand)
      `((halt ,opand))]

      [`(jump ,trg)
      `((jump ,trg))]
      
      [`(begin ,effects ...  ,tail)
       `(,@effects ,@(flatten-t tail))]

      [`(if (,relop, loc ,opand) (jump ,trg1) (jump ,trg2))
       `((compare ,loc   ,opand)
         (jump-if ,relop ,trg1)
         (jump    ,trg2))]))

  (flatten-p p))

; =============== M3 Passes ================

; Input:   asm-pred-lang-v6/locals
; Output:  asm-pred-lang-v6/undead
; Purpose: Performs undead analysis, compiling Asm-pred-lang v6/locals to Asm-pred-lang v6/undead 
;          by decorating programs with their undead-set trees.
(define (undead-analysis p)

  ; stores abstract locations and frame variables in the undead-out sets of return-points.
  (define call-acc (mutable-set))

  ; Decorate the program with the undead-out tree.
  (define (undead-p p) 
    (match p
      [`(module ,info ,defines ... ,tail)
        (define-values (ust x) (undead-tail tail))  ; find the undead-set tree of the tail
       `(module ,(info-set (info-set info 'call-undead (set->list call-acc)) `undead-out ust)
                ,@(map undead-def defines)
                ,tail)]))

  (define (undead-def d)
    (match d
      [`(define ,label ,info ,tail)
        (set! call-acc (mutable-set))
        (define-values (ust x) (undead-tail tail))  ; find the undead-set tree of the tail
       `(define ,label 
                ,(info-set (info-set info `undead-out ust) 'call-undead (set->list call-acc))
                ,tail)]))

  ; Takes a tail and produces the undead-set tree from the effects
  ; Return: (values undead-set-tree? undead-set?)
  ; i.e. (<tree for tail> <set for next effect>)
  (define (undead-tail t)
    (match t
      [`(begin ,effects ... ,tail)
        (define-values (tailUst tIn) (undead-tail tail))
 
        (define-values (ust undead-o)
          (for/foldr ([ust `(,tailUst)]   ; ust represents the undead-set-tree for this begin.
                      [nextIn tIn])  ; nextIn: initialized as the input to the first effect we process
                     ([e effects])
                     (define-values (new-ust undead-in)    ; new-ust represents the undead-set-tree for e
                                    (undead-effect nextIn e))
                     (values (cons new-ust ust) undead-in)))

        (values ust undead-o)]
      [`(if ,pred ,tail1 ,tail2)
        (define-values (tail2Ust t2In) (undead-tail tail2)) ; process tail1 and tail2 separately
        (define-values (tail1Ust t1In) (undead-tail tail1))
        (define-values (predUst predIn) (undead-pred (set-union t1In t2In) pred)) ; pass their combined result into pred
        (values `(,predUst ,tail1Ust ,tail2Ust) predIn)]
      [`(jump ,trg ,loc ...)
        (if (label? trg) ; exclude labels from the set
            (values loc loc)
            (values loc (cons trg loc)))]))

  ; Given a pred, return the corresponding undead-out tree.
  ; Use a base undead-out consisting of the union of the undead-outs of the pred branches.
  ; Return: (values undead-set-tree? undead-set?)
  ; i.e. (<tree for pred> <set for next effect>)
  (define (undead-pred undead-out pr)
    (match pr
      [`(,relop ,loc ,opand) #:when (relop? relop)
        (define add-loc (set-add undead-out loc))
        (if (number? opand)
          (values undead-out add-loc)
          (values undead-out (set-add add-loc opand)))]
      [`(,bool) #:when (and (member bool '(true false)) #t)
        (values undead-out undead-out)]
      [`(not ,pred)
        (undead-pred undead-out pred)]
      [`(if ,pred1 ,pred2 ,pred3)
        (define-values (pred3Ust pred3In) (undead-pred undead-out pred3)) ; process the 2nd and 3rd pred separately
        (define-values (pred2Ust pred2In) (undead-pred undead-out pred2)) ; pass their combined results into 1st pred
        (define-values (pred1Ust pred1In) (undead-pred (set-union pred2In pred3In) pred1))
        (values `(,pred1Ust ,pred2Ust ,pred3Ust) pred1In)]
      [`(begin ,effects ... ,pred)
        (define-values (predUst predIn) (undead-pred undead-out pred))
        (define-values (ust undead-o)
          (for/foldr ([ust `(,predUst)]   ; ust represents the undead-set-tree for this begin.
                      [nextIn predIn])  ; nextIn: initialized as the input to the first effect we process
                     ([e effects])
                     (define-values (new-ust undead-in)    ; new-ust represents the undead-set-tree for e
                                    (undead-effect nextIn e))
                     (values (cons new-ust ust) undead-in)))

        (values ust undead-o)]))
  
  ; Calculate the undead input for a single effect e,
  ; given the output, undead-out.
  ; Return: (values undead-set-tree? undead-set?)
  ; i.e. (<tree for current effect> <set for next effect>)
  (define (undead-effect undead-out e)
    (match e
      [`(set! ,loc (,binop ,loc ,opand))
        (let ([newSet (if (number? opand)
                          (set-add undead-out loc)
                          (set-add (set-add undead-out opand) loc))])
          (values undead-out newSet))]
      [`(set! ,loc ,triv)
        (let ([newSet (if (or (number? triv) (label? triv))
                          (set-remove undead-out loc)
                          (set-remove (set-add undead-out triv) loc))])
          (values undead-out newSet))]
      [`(begin ,effects ...)        
        (define-values (ust undead-o)
          (for/foldr ([ust `()]  ; ust represents the undead-set-tree for this begin.
                      [nextIn undead-out])  ; nextIn: initialized as the input to the first effect we process
                     ([e effects])
                     (define-values (new-ust undead-in)    ; new-ust represents the undead-set-tree for e
                                    (undead-effect nextIn e))
                     (values (cons new-ust ust) undead-in)))
        (values ust undead-o)]
      [`(if ,pred ,effect1 ,effect2)
        (define-values (eff2Ust e2In) (undead-effect undead-out effect2)) ; process effects separately
        (define-values (eff1Ust e1In) (undead-effect undead-out effect1)) ; pass their combined results into pred
        (define-values (predUst predIn) (undead-pred (set-union e1In e2In) pred))
        (values `(,predUst ,eff1Ust ,eff2Ust) predIn)]
      [`(return-point ,label ,tail)
        (define-values (ust nextIn) (undead-tail tail))  ; find the undead-set tree of the tail
        (define undead-filtered (filter (lambda (x) (not (register? x))) undead-out))
        (for ([e undead-filtered])
             (set-add! call-acc e))
        ; treat a return-point as defining the current-return-value-register.
        (define new-undead-out (set-remove undead-out (current-return-value-register)))
        (values `(,undead-out ,ust) (set-union nextIn new-undead-out))]))
  
  (undead-p p))


; Input:   asm-pred-lang-v6/undead
; Output:  asm-pred-lang-v6/conflicts
; Purpose: Performs conflict analysis, compiling Asm-pred-lang v6/undead to 
;          Asm-pred-lang v6/conflicts by decorating programs with their conflict graph.
(define (conflict-analysis p)

  ; Find the entries of lst that are not in excludeLst. The first entry of 
  ; excludeLst is the primary loc with which to find conflicts.
  ; Update the graph with the conflicts and return it.
  (define (update-conflicts excludeLst lst graph)
    (for/fold ([g graph])
              ([conflict (filter (lambda (e) (not (and (member e excludeLst) #t))) 
                                  lst)])
              (add-edge g (first excludeLst) conflict)))

  (define (c-analysis-p p)
    (match p
      [`(module ,info ,defines ... ,tail)
        (define locals (info-ref info 'locals))
        (define undead (info-ref info 'undead-out))
       `(module ,(info-set info 'conflicts (c-analysis-t undead (new-graph locals) tail))
                ,@(map c-analysis-def defines)
                ,tail)]))
  
  (define (c-analysis-def d)
    (match d
      [`(define ,label ,info ,tail)
        (define locals (info-ref info 'locals))
        (define undead (info-ref info 'undead-out))
       `(define ,label 
                ,(info-set info 'conflicts (c-analysis-t undead (new-graph locals) tail))
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
      [`(if ,pred ,tail1 ,tail2)
        (define predGraph (c-analysis-pr (first undead) graph pred))
        (define tail1Graph (c-analysis-t (second undead) predGraph tail1))
        (c-analysis-t (third undead) tail1Graph tail2)]
      [`(jump ,trg ,loc ...) 
        graph]))

  ; undead : a nested list of lists of abstract locations such as x.1. 
  ; graph   : a graph of the conflicts found so far.
  ; Return a graph.
  (define (c-analysis-pr undead graph pr)
    (match pr
      [`(not ,pred)
        (c-analysis-pr undead graph pred)]
      [`(begin ,effects ... ,pred)
        (c-analysis-pr 
          (last undead)
          (for/fold ([g graph])  ; pair effects with entries in the undead list and update graph recursively.
                    ([eff effects] [currUndead undead])
                    (c-analysis-e currUndead g eff))
          pred)]
      [`(if ,preds ...)
        (for/fold ([g graph])  ; pair effects with entries in the undead list and update graph recursively.
                  ([p preds] [currUndead undead])
                  (c-analysis-pr currUndead g p))]
      [_ graph])) ; bool or relop

  ; undead : the entry in the list of undead relating to the current effect
  ; graph   : a graph of the conflicts found so far.
  ; Return a graph.
  (define (c-analysis-e undead graph e)
    (match e
      [`(set! ,loc1 ,loc2) #:when (or (aloc? loc2) (register? loc2) (fvar? loc2))
        (update-conflicts `(,loc1 ,loc2) undead graph)]
      [`(set! ,loc ,other) ; other is binop, number, or label
        (update-conflicts `(,loc) undead graph)]
      [`(begin ,effects ...)
        (for/fold ([g graph])  ; pair effects with entries in the undead list and update graph recursively.
                  ([eff effects] [currUndead undead])
                  (c-analysis-e currUndead g eff))]
      [`(if ,pred ,effect1 ,effect2)
        (define predGraph (c-analysis-pr (first undead) graph pred))
        (define e1Graph (c-analysis-e (second undead) predGraph effect1))
        (c-analysis-e (third undead) e1Graph effect2)]
      [`(return-point ,label ,tail)
        (c-analysis-t (second undead) graph tail)]))

  (c-analysis-p p))

 
; Input:    asm-pred-lang-v6/framed
; Output:   asm-pred-lang-v6/spilled
; Purpose:  Performs graph-colouring register allocation, 
;           compiling Asm-pred-lang v6/framed to Asm-pred-lang v6/spilled by decorating programs with their register assignments.
(define (assign-registers p)
  ; a list consisting of '(rsp rbx rcx rdx rsi rdi r8 r9 r13 r14 r15)
  (define car (current-assignable-registers))

  ; splice the updated info block into the language
  (define (assign-p p)
    (match p
      [`(module ,info ,def ... ,tail)
       `(module ,(assign-info info) ,@(map (lambda (d) (assign-block d)) def) ,tail)]))

  ; generates the assignment for a single block
  (define (assign-block d)
    (match d
      [`(define ,label ,info ,tail)
       `(define ,label ,(assign-info info) ,tail)]))

  ; update the info block with new assignments
  ; clean the conflicts so only conflicts declared in locals remain
  ; then sort the conflicts
  (define (assign-info i)
    (let* ([conflicts   (first (dict-ref i 'conflicts))]
           [locals      (first (dict-ref i 'locals))]
           [assignments (generate-assignments locals conflicts '())])
          (dict-set (dict-set i 'locals (list (update-locals assignments locals))) 
                    'assignment 
                   `((,@(extract-dict i 'assignment) ,@assignments)))))

  ; generates assignments based on the alocs and their conflicts
  (define (generate-assignments locals conflicts assignments)
    (define registers (reverse car))
    (if (empty? locals)
        '()
        (let* ([node            (first locals)]
               [node-conflicts  (first (dict-ref conflicts node))]
               [new-locals      (remove node locals)]
               [new-conflicts   (remove node conflicts)]
               [new-assignments (generate-assignments new-locals new-conflicts assignments)]
               [assigned-node   (assign-node node node-conflicts new-assignments registers)])
              (if (false? assigned-node)
                  new-assignments
                 (append (list assigned-node) new-assignments)))))

  ; assigns a single aloc to a register
  (define (assign-node node node-conflicts assignments registers)
    (cond [(empty? registers) #f]
          [(empty? assignments)
            (define try-assigned (try-assign-register node node-conflicts registers))
            (if (false? try-assigned)
                #f
                try-assigned)]
          [else
           (if (ormap (lambda (x) (has-conflict node node-conflicts (first registers) x)) assignments)
              (assign-node node node-conflicts assignments (rest registers))
             `(,node ,(first registers)))]))

  ; return false if this node is incompatible with all registers, otherwise assign it
  (define (try-assign-register node node-conflicts registers)
    (cond [(empty? registers) #f]
          [else
           (define register (first registers))
           (if (member register node-conflicts)
               (try-assign-register node node-conflicts (rest registers))
               `(,node ,register))]))
  
  ; return true if:
  ; a) the fvar we are trying to assign this node is in the node's conflict list OR...
  ; b) some other assignment already uses this register, and the nodes conflict with each other
  ; otherwise return false
  (define (has-conflict node node-conflicts register assignment)
    (define a-aloc (first assignment))
    (define a-reg (second assignment))
    (cond [(member register node-conflicts) #t]
          [(equal? a-reg register) (is-in-list node-conflicts a-aloc)]
          [else #f]))

  ; in the locals list, keep only the locals who have not been assigned yet
  (define (update-locals assignments locals)
    (define assignments-alocs (dict-keys assignments))
    (filter (lambda (x) (not (is-in-list assignments-alocs x))) locals))

  (assign-p p))


; Input:   Values-lang-v6
; Output:  Values-unique-lang v6
; Purpose: Compiles Values-lang v6 to Values-unique-lang v6 by resolving top-level lexical identifiers 
;          into unique labels, and all other lexical identifiers into unique abstract locations.
(define (uniquify p) 

  (define label-binds-box (box '()))

  ; Input: tail, a list of possibly nested let statements
  ; Output: Values-unique-lang-v3
  ; Purpose: matches on p, which is a list of (nested) let statements
  (define (uniquify-p p dict-acc)
    (match p
      [`(module ,def ... ,t)
        (define uniquified-labels (map (lambda (d) (uniquify-define-labels d dict-acc)) def))
       `(module ,@(map (lambda (d) (uniquify-define d dict-acc)) uniquified-labels)
                ,(uniquify-tail t dict-acc))]))

  ; Purpose: generate unique labels for the define statements blocks
  (define (uniquify-define-labels def def-binds)
    (match def
      [`(define ,x (lambda (,xs ...) ,tail))
        (define label-binds (unbox label-binds-box))
        (if (dict-has-key? label-binds x)
            (set-box! label-binds-box label-binds)
            (set-box! label-binds-box (dict-set label-binds x (fresh-label x))))
       `(define ,(dict-ref (unbox label-binds-box) x) (lambda (,@xs) ,tail))]))
  
  ; Purpose: generate unique alocs for the variables in the define statement bodies
  (define (uniquify-define def def-binds)
    (match def
      [`(define ,uniquified-label (lambda (,xs ...) ,t))
        (define new-def-binds (construct-binds xs def-binds))
       `(define ,uniquified-label
                (lambda ,(map (lambda (x) (dict-ref new-def-binds x)) xs) 
                        ,(uniquify-tail t new-def-binds)))]))

  ; Input: value, a let statement or a binary operation
  ; Output: uniquified version of this expression
  ; Purpose: recursively uniquify a single expression
  (define (uniquify-tail t binds)
    (match t 
      [`(let ([,as ,vs] ...) ,tail)
        (define new-binds (construct-binds as binds)) 
        `(let ,(for/list ([a as][v vs])
                        `[,(dict-ref new-binds a) ,(uniquify-value v binds)])     
              ,(uniquify-tail tail new-binds))]          
      
      [`(if ,p ,t1 ,t2)
       `(if ,(uniquify-pred       p binds)
            ,(uniquify-tail      t1 binds) 
            ,(uniquify-tail      t2 binds))]
      
      [`(call ,vs ...)
       `(call ,@(map (lambda (v) (uniquify-value v binds)) vs))]
      
      [value (uniquify-value value binds)]))

  ; Purpose: uniquify a single value
  (define (uniquify-value v binds)
    (match v
       [`(if ,p ,v1 ,v2)
        `(if ,(uniquify-pred       p binds) 
             ,(uniquify-value     v1 binds)
             ,(uniquify-value     v2 binds))]

       [`(,binop ,opand1 ,opand2)
         #:when (or (equal? binop '*) (equal? binop '+) (equal? binop '-))
        `(,binop ,(uniquify-value opand1 binds) ,(uniquify-value opand2 binds))]

      [`(let ([,as ,vs] ...) ,body)
        (define new-binds (construct-binds as binds)) 
        `(let ,(for/list ([a as][v vs])
                        `[,(dict-ref new-binds a) ,(uniquify-value v binds)])     
              ,(uniquify-value body new-binds))]   

      [`(call ,triv ,os ...)
       `(call ,(update-bind triv binds) ,@(map (lambda (o) (uniquify-value o binds)) os))]

       [triv
        (update-bind triv binds)]
      )
  )

  ; Purpose: uniquify a predicate
  (define (uniquify-pred p binds)
    (match p
      [`(,relop ,triv1 ,triv2)
      #:when (relop? relop)
      `(,relop ,(uniquify-value triv1 binds) ,(uniquify-value triv2 binds))]

      [`(true)  `(true)]

      [`(false) `(false)]

      [`(not ,pred)
       `(not ,(uniquify-pred pred binds))]

      [`(let ([,as ,vs] ...) ,pred)
        (define new-binds (construct-binds as binds)) 
        `(let ,(for/list ([a as][v vs])
                        `[,(dict-ref new-binds a) ,(uniquify-value v binds)])     
              ,(uniquify-pred pred new-binds))]         

      [`(if ,p1 ,p2 ,p3)
       `(if ,(uniquify-pred p1 binds)
            ,(uniquify-pred p2 binds)
            ,(uniquify-pred p3 binds))]))

  ; Input: triv and binding dictionary
  ; Output: int64 or aloc, opand, or label
  ; Purpose: return the triv itself if it is an int64, otherwise look up the bind in the dictionary
  (define (update-bind x binds)
    (match x
      [(? integer?) x]
      [(? symbol?) (assign-aloc-or-label x binds)]))

  ; Input: aloc and binding dictionary
  ; Output: a new binding dictionary
  ; uses fresh to assign a unique aloc? to each name? at the current scope
  (define (construct-binds xs binds)
    (for/fold ([new-binds binds]) ; accumulator
              ([x xs])            ; x is an element, xs is the list
      (dict-set new-binds x (fresh x))))    

  ; Purpose: lookup the associated key in a binding dictionary, prioritizng label binds first 
  (define (assign-aloc-or-label x binds)
    (if (dict-has-key? (unbox label-binds-box) x)
        (dict-ref (unbox label-binds-box) x)
        (dict-ref binds x)))                                

  (uniquify-p p '()))


; Input:   values-unique-lang-v6
; Output:  imp-mf-lang-v6
; Purpose: Compiles Values-unique-lang v6 to Imp-mf-lang v6 by picking a 
;          particular order to implement let expressions using set!.
(define (sequentialize-let p)
  
  (define (seq-p p)
    (match p
      [`(module ,defines ... ,tail)
          `(module ,@(map seq-def defines) ,(seq-t tail))]))

  (define (seq-def d)
    (match d
      [`(define ,label (lambda (,aloc ...) ,tail))
       `(define ,label (lambda ,aloc ,(seq-t tail)))]))

  ; Return an instruction
  (define (seq-t t)
    (match t
      [`(let ([,aloc ,value] ...) ,tail) 
       `(begin ,@(map seq-bind (map list aloc value)) ; zip the aloc and value lists
                ,(seq-t tail))]
      [`(if ,pred ,tail1 ,tail2)
       `(if ,(seq-pr pred) ,(seq-t tail1) ,(seq-t tail2))]
      [`(call ,triv ,opand ...)
       t]
      [value
        (seq-v value)]))
    
  (define (seq-pr pr)
    (match pr
      [`(not ,pred)
       `(not ,(seq-pr pred))]
      [`(if ,pred1 ,pred2 ,pred3)
       `(if ,(seq-pr pred1) ,(seq-pr pred2) ,(seq-pr pred3))]
      [`(let ([,aloc ,value] ...) ,pred)
       `(begin ,@(map seq-bind (map list aloc value)) ; zip the aloc and value lists
               ,(seq-pr pred))]
      [_ pr])) ; relop or bool

  ; Return an instruction, given a pair of an aloc and its value.
  (define (seq-bind b)
    (match b
      [`(,aloc1 (let ([,aloc ,value] ...) ,val))
        `(set! ,aloc1
               (begin ,@(map seq-bind (map list aloc value)) ; zip the aloc and value lists
                      ,(seq-v val)))]
      [`(,aloc ,value)
        `(set! ,aloc ,(seq-v value))]))

  (define (seq-v v)
    (match v
      [`(let ([,aloc ,value] ...) ,val) 
       `(begin ,@(map seq-bind (map list aloc value)) ; zip the aloc and value lists
               ,@(seq-v val))]
      [`(if ,pred ,val1 ,val2)
       `(if ,(seq-pr pred) ,(seq-v val1) ,(seq-v val2))]
      [`(call ,triv ,opand ...)
       v]
      [_ v])) ; triv or binop

  (seq-p p))


; Input:   imp-mf-lang-v6
; Output:  proc-imp-cmf-lang-v6
; Purpose: Compiles Imp-mf-lang v6 to Proc-imp-cmf-lang v6, pushing set! under 
;          begin so that the right-hand-side of each set! is simple value-producing operation.
(define (normalize-bind p)

  (define (n-bind-p p)
    (match p
      [`(module ,defines ... ,tail)
          `(module ,@(map n-bind-def defines) ,(n-bind-t tail))]))

  (define (n-bind-def d)
    (match d
      [`(define ,label (lambda (,aloc ...) ,tail))
       `(define ,label (lambda ,aloc ,(n-bind-t tail)))]))
  
  ; Return an instruction
  (define (n-bind-t t)
    (match t
      [`(begin ,eff ... ,tail)
       `(begin ,@(map n-bind-e eff) ,(n-bind-t tail))]
      [`(if ,pred ,tail1 ,tail2)
       `(if ,(n-bind-pr pred) ,(n-bind-t tail1) ,(n-bind-t tail2))]
      [`(call ,triv ,opand ...)
        t]
      [value
        (n-bind-v value)]))
  
  ; Return an instruction
  (define (n-bind-v v)
    (match v
      [`(begin ,eff ... ,val) 
        `(begin ,@(map n-bind-e eff) ,(n-bind-v val))]
      [`(if ,pred ,value1 ,value2)
       `(if ,(n-bind-pr pred) ,(n-bind-v value1) ,(n-bind-v value2))]
      [`(call ,triv ,opand ...)
        v]
      [_ v])) ; triv or binop

  ; Return an instruction
  (define (n-bind-pr pr)
    (match pr
      [`(not ,pred)
        `(not ,(n-bind-pr pred))]
      [`(if ,pred1 ,pred2 ,pred3)
       `(if ,(n-bind-pr pred1) ,(n-bind-pr pred2) ,(n-bind-pr pred3))]
      [`(begin ,eff ... ,pred)
       `(begin ,@(map n-bind-e eff) ,(n-bind-pr pred))]
      [_ pr])) ; relop or bool
  
  ; Return an instruction
  (define (n-bind-e e)
    (match e
      [`(set! ,aloc (begin ,eff ... ,val)) ; normalize so that begin is above set.
        `(begin ,@(map n-bind-e eff) ,(n-bind-e `(set! ,aloc ,(n-bind-v val))))]
      [`(set! ,aloc (if ,pred ,val1 ,val2)) ; normalize so that if is above set.
        `(if ,(n-bind-pr pred) ,(n-bind-e `(set! ,aloc ,(n-bind-v val1))) ,(n-bind-e `(set! ,aloc ,(n-bind-v val2))))]
      [`(set! ,aloc ,trivOrBinop) 
        `(set! ,aloc ,trivOrBinop)]
      [`(if ,pred ,eff1 ,eff2)
       `(if ,(n-bind-pr pred) ,(n-bind-e eff1) ,(n-bind-e eff2))]
      [`(begin ,eff ...)
        `(begin ,@(map n-bind-e eff))]))

  (n-bind-p p))


; Input:   imp-cmf-lang-v6
; Output:  asm-pred-lang-v6
; Purpose: Compiles Imp-cmf-lang v6 to Asm-pred-lang v6, selecting appropriate sequences of
;          abstract assembly instructions to implement the operations of the source language.
; M5 > M6 : remove support for halt, tail is no longer value, add return-points. 
(define (select-instructions p)

  (define (sel-ins-p p)
    (match p
      [`(module ,info ,defines ... ,tail)
        (define tailRes (sel-ins-t tail))
        (if (equal? (length tailRes) 1)
           `(module ,info ,@(map sel-ins-def defines) ,@(sel-ins-t tail))
           `(module ,info ,@(map sel-ins-def defines) (begin ,@(sel-ins-t tail))))]))
  
  (define (sel-ins-def d)
    (match d
      [`(define ,label ,info ,tail)
        (define tailRes (sel-ins-t tail))
        (if (equal? (length tailRes) 1)
           `(define ,label ,info ,@(sel-ins-t tail))
           `(define ,label ,info (begin ,@(sel-ins-t tail))))]))

  ; Return a list of instructions
  (define (sel-ins-t t)
    (match t
      [`(jump ,trg ,loc ...) `(,t)]
      [`(begin ,effects ... ,tail) ; flattens nested begins, including in tail position.
       `(,@(splice-mapped-list (map sel-ins-e effects)) ,@(sel-ins-t tail))]
      [`(if ,pred ,tail1 ,tail2)
        (define tailRes1 (sel-ins-t tail1))
        (define tailRes2 (sel-ins-t tail2))
        (define corrTail1 (if (equal? (length tailRes1) 1)
                              tailRes1
                             `((begin ,@tailRes1)))) ; wrap in begin if multiple instructions
        (define corrTail2 (if (equal? (length tailRes2) 1)
                              tailRes2
                             `((begin ,@tailRes2))))
       `((if ,(sel-ins-pr pred) ,@corrTail1 ,@corrTail2))]))
  
  ; Returns a list of instructions
  (define (sel-ins-v v aloc)
    (match v
      [`(,binop ,opand1 ,opand2)
       `((set! ,aloc ,opand1)
         (set! ,aloc (,binop ,aloc ,opand2)))]
      [triv ; label, int, register, fvar, or aloc
        `((halt ,triv))]))

  ; Returns an instruction
  (define (sel-ins-pr pr)
    (match pr
      [`(not ,pred)
       `(not ,(sel-ins-pr pred))]
      [`(,relop ,opand1 ,opand2) #:when (and (relop? relop) (number? opand1))
        (define getfresh (fresh))
       `(begin (set! ,getfresh ,opand1) 
               (,relop ,getfresh ,opand2))]
      [`(begin ,effects ... ,pred) ; flatten nested begins here (currently different from interrogator)
       `(begin ,@(splice-mapped-list (map sel-ins-e effects)) ,(sel-ins-pr pred))]
      [`(if ,preds ...)
       `(if ,@(map sel-ins-pr preds))]
      [_ pr])) ; bool
  
  ; Process an effect and return a list of instructions
  (define (sel-ins-e e)
    (match e
      [`(set! ,loc (,binop ...))
        (sel-ins-v binop loc)]
      [`(set! ,loc ,triv) `(,e)] ; triv is label, int, register, fvar, or aloc
      [`(begin ,eff ...)  ; flatten nested begins
        `(,@(splice-mapped-list (map sel-ins-e eff)))]
      [`(if ,pred ,effect1 ,effect2)
        ; wrap the effect result in (begin ) if it is multiple instructions.
        ; do NOT flatten begins if the effect was already a (begin ).
        (define effRes1 (sel-ins-e effect1))
        (define effRes2 (sel-ins-e effect2))
        (define corrEffRes1 (if (or (> (length effRes1) 1) (equal? (first effect1) 'begin))
                               `((begin ,@effRes1))
                                effRes1))
        (define corrEffRes2 (if (or (> (length effRes2) 1) (equal? (first effect2) 'begin))
                               `((begin ,@effRes2))
                                effRes2))
       `((if ,(sel-ins-pr pred) ,@corrEffRes1 ,@corrEffRes2))]
      [`(return-point ,label ,tail)
        (define tailRes (sel-ins-t tail))
        (if (equal? (length tailRes) 1)
           `((return-point ,label ,@(sel-ins-t tail)))
           `((return-point ,label (begin ,@(sel-ins-t tail)))))]))

  (sel-ins-p p))


; Input:   asm-pred-lang-v5/assignments
; Output:  nested-asm-lang-v5
; Purpose: Replaces all abstract location with physical locations using the assignment described in the 
;          assignment info field, and dropping any register-allocation-related metadata from the program.
(define (replace-locations p)
  ; Compiles Asm-lang v2/assignments to Nested-asm-lang v2, replacing each 
  ; abstract location with its assigned physical location from the assignment info field.

  (define (replace-loc-p p)
    (match p
      [`(module ,info ,tail)
       `(module ,(replace-loc-t tail info))]
      [`(module ,info ,label ... ,tail)
       `(module ,@(map replace-loc-l label) ,(replace-loc-t tail info))]))

  (define (replace-loc-l l)
    (match l
      [`(define ,label ,info ,tail)
       `(define ,label ,(replace-loc-t tail info))]))

  (define (get-repl aloc as)
    ; given an abstract location 'aloc' return its replacement as defined
    ; in the list of assignments 'as'.
    (info-ref
    (info-ref as 'assignment)
    aloc))

  (define (replace-loc-t t as)
    ; return a tail with locations replaced.
    (match t
      [`(begin ,effects ... ,tail)
        `(begin ,@(map (curry replace-loc-e as) effects) ,(replace-loc-t tail as))]
      [`(if ,pred ,tail1 ,tail2)
        `(if ,(replace-loc-pred as pred) ,(replace-loc-t tail1 as) ,(replace-loc-t tail2 as))]
      [`(jump ,trg ,loc ...)
        `(jump ,(replace-loc trg as))]))

  (define (replace-loc-e as e)
    (match e
      [`(set! ,loc_1 (,binop ,loc_1 ,opand))
        `(set! ,(replace-loc loc_1 as) (,binop ,(replace-loc loc_1 as) ,(replace-loc opand as)))]
      [`(set! ,loc ,triv)
        `(set! ,(replace-loc loc as) ,(replace-loc triv as))]
      [`(begin ,effects ...)
        `(begin ,@(map (curry replace-loc-e as) effects))]
      [`(if ,pred ,effect1 ,effect2)
        `(if ,(replace-loc-pred as pred) ,(replace-loc-e as effect1) ,(replace-loc-e as effect2))]
      [`(return-point ,label ,tail)
        `(return-point ,label ,(replace-loc-t tail as))]))

  (define (replace-loc-pred as p)
    (match p
      [`(if ,pred1 ,pred2 ,pred3)
        `(if ,(replace-loc-pred as pred1) ,(replace-loc-pred as pred2) ,(replace-loc-pred as pred3))]
      [`(not ,pred)
        `(not ,(replace-loc-pred as pred))]
      [`(begin ,effect ... ,pred)
        `(begin ,@(map (curry replace-loc-e as) effect) ,(replace-loc-pred as pred))]
      [`(,relop ,loc ,opand)
        `(,relop ,(replace-loc loc as) ,(replace-loc opand as))]
      [`(true)
        `(true)]
      [`(false)
        `(false)]))

  (define (replace-loc loc as)
    (if (aloc? loc) (get-repl loc as) loc))

  (replace-loc-p p))


; Input:   asm-pred-lang-v6
; Output:  asm-pred-lang-v6/locals
; Purpose: Compiles Asm-pred-lang v6 to Asm-pred-lang v6/locals, analysing which abstract locations 
;          are used in each block, and decorating each block and the module with the set of variables 
;          in an info? field.
(define (uncover-locals p)

  (define (uloc-p p)
    (match p
      [`(module ,info ,defines ... ,tail)
       `(module ,(info-set info 'locals (uloc-t tail '())) ,@(map uloc-def defines) ,tail)]))

  (define (uloc-def def)
    (match def
      [`(define ,label ,info ,tail)
       `(define ,label ,(info-set info 'locals (uloc-t tail '())) ,tail)]))
  
  ; return a set of alocs.
  (define (uloc-t t acc)
    (match t
      [`(begin ,effect ... ,tail)
        (uloc-t 
          tail 
          (foldl (lambda (elem rest) (set-add rest elem)) 
            acc 
            (foldl append '() (map uloc-e effect))))]
      [`(if ,pred ,tail1 ,tail2)
        (set-union 
          (uloc-pred pred)
          (uloc-t tail1 acc)
          (uloc-t tail2 acc))]
      [`(jump ,trg ,loc ...) ; alocs in jump params are not added to locals set.
        (if (aloc? trg) (set-add acc trg) acc)]))
  
  ; return: set of all alocs found in effect
  (define (uloc-e e)
    (match e  
      [`(set! ,loc_1 (,binop ,loc_1 ,opand))
        (aloc-truth loc_1 opand)]
      [`(set! ,loc ,triv)
        (aloc-truth loc triv)]  
      [`(if ,pred ,effect1 ,effect2)
        (set-union
          (uloc-pred pred)
          (uloc-e effect1)
          (uloc-e effect2))]
      [`(begin ,effects ...)
        (foldl append '() (map uloc-e effects))]
      [`(return-point ,label ,tail)
        (uloc-t tail '())]))

  (define (uloc-pred p)
    (match p
      [`(begin ,effect ... ,pred)
        (set-union
          (uloc-pred pred)
          (foldl set-union '() (map uloc-e effect)))]
      [`(if ,pred1 ,pred2 ,pred3)
        (set-union
          (uloc-pred pred1)
          (uloc-pred pred2)
          (uloc-pred pred3))]
      [`(not ,pred)
        (uloc-pred pred)]
      [`(,relop ,loc ,opand)
        (aloc-truth loc opand)]
      [_ `()])) ;bool

  (define (aloc-truth a1 a2)
     (cond
        [(and (aloc? a1) (aloc? a2)) `(,a2 ,a1)]
        [(aloc? a1) `(,a1)]
        [(aloc? a2) `(,a2)]
        [else `()]))

  (uloc-p p))


; Input:   para-asm-lang-v6
; Output:  paren-x64-fvars-v6
; Purpose: Compile the Para-asm-lang v6 to Paren-x64 v6 by patching instructions that have 
;          no x64 analogue into to a sequence of instructions and an auxiliary register from current-patch-instructions-registers.
(define (patch-instructions p)
  (define i-reg1 (first (current-patch-instructions-registers)))
  (define i-reg2 (second (current-patch-instructions-registers)))
  (define ret-reg (current-return-value-register))

  (define (patch-p p)
    (match p
      [`(begin ,e ...)
       `(begin 
          ,@(foldr ; splice things when an instruction is replaced by multiple instructions.
            (lambda (elem rest) (if (list? (first elem)) `(,@elem ,@rest) `(,elem ,@rest)))
            '()
            (map patch-effect e)))]))

  (define (patch-effect e)
    (match e
      [`(set! ,addr1 ,addr2) #:when (and (address? addr1) (address? addr2))
          `((set! ,i-reg1 ,addr2)
            (set! ,addr1 ,i-reg1))]
      [`(set! ,addr1 ,int64) #:when (and (not (int32? int64)) (int64? int64) )
          `((set! ,i-reg1 ,int64)
            (set! ,addr1 ,i-reg1))]
      [`(set! ,addr1 ,label) #:when (and (address? addr1) (label? label))
          `((set! ,i-reg1 ,label)
            (set! ,addr1 ,i-reg1))]
      [`(set! ,addr (,binop ...)) #:when (address? addr)
          `((set! ,i-reg1 ,addr)
            (set! ,i-reg1 ,(patch-binop binop))
            (set! ,addr ,i-reg1))]
      [`(with-label ,label (halt ,opand))
          `((with-label ,label (set! ,ret-reg ,opand))
            (jump done))]
      [`(with-label ,label ,s)
          `(with-label ,label ,(patch-effect s))]
      [`(jump ,addr) #:when (address? addr)
          `((set! ,i-reg1 ,addr)
            (jump ,i-reg1))]
      [`(compare ,addr1 ,addr2) #:when (and (address? addr1) (address? addr2))
          `((set! ,i-reg2 ,addr2)
            (set! ,i-reg1 ,addr1)
            (compare ,i-reg1 ,i-reg2))]
      [`(compare ,reg ,addr1) #:when (address? addr1)
          `((set! ,i-reg1 ,addr1)
            (compare ,reg ,i-reg1))]
      [`(jump-if ,relop ,trg) #:when (not (label? trg))
          `((jump-if ,(patch-relop relop) L.tmp.1)
            (jump ,trg)
            (with-label L.tmp.1 (set! ,i-reg1 ,i-reg1)))]
      [_ e]))  ; everything else

  (define (patch-binop b)
    (match b
      [`(,binop ,addr ,val) #:when (address? addr)
       `(,binop ,i-reg1 ,val)]
      [_ b]))  ; everything else
    
  (define (patch-relop r)
    (cond [(equal? r '<) '>=]
          [(equal? r '<=) '>]
          [(equal? r '=) '!=]
          [(equal? r '>=) '<]
          [(equal? r '>) '<=]
          [(equal? r '!=) '=]))
  
  (patch-p p))


; Input:   paren-x64-fvars-v4
; Output:  paren-x64-v4
; Purpose: Compile the Paren-x64-fvars v4 to Paren-x64 v4 by reifying fvars into displacement mode operands.
(define (implement-fvars p)
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
      [`(with-label ,label ,s)                  ; (with-label label s)
          `(with-label ,label ,(f-statement->s s))]
      [_ s]))  ; everything else

  (define (f-binop->b b)
    (match b
      [`(,binop ,r1 ,fvar) #:when (fvar? fvar)
          `(,binop ,r1 ,(fvar->addr fvar))]
      [_ b]))  ; everything else
       
  (f-program->p p))

; Input:   paren-x64-v6
; Output:  x64-instructions
; Purpose: Compile the Paren-x64 v6 program into a valid sequence of x64 instructions, represented as a string.
(define (generate-x64 p)

  (define (program->x64 p)
    (match p
    [`(begin ,s ...)
      (for/fold ([acc ""])
                ([str s]) 
                (statement->x64 str acc))]))

  (define (statement->x64 s x64)
    (match s
      [`(set! ,addr ,int32)
      #:when (and (address? addr) (int32? int32))
      (string-append x64 "mov " (addr->ins addr) ", " (number->string int32) "\n")]

      [`(set! ,addr ,trg)
      #:when (and (address? addr) (trg? trg))
      (string-append x64 (if (register? trg) "mov " "lea ") (addr->ins addr) ", " (trg->ins trg) "\n")]

      [`(set! ,reg ,loc)
      #:when (and (register? reg) (loc? loc))
      (string-append x64 "mov " (symbol->string reg) ", " (loc->ins loc) "\n")]
      
      [`(set! ,reg ,triv)
      #:when (and (register? reg) (register? triv))
      (string-append x64 "mov " (symbol->string reg) ", "(symbol->string triv) "\n")]

      [`(set! ,reg ,triv)
      #:when (and (register? reg) (label? triv))
      (string-append x64 "lea " (symbol->string reg) ", " (trg->ins triv) "\n")]

      [`(set! ,reg ,triv)
      #:when (and (register? reg) (int64? triv))
      (string-append x64 "mov " (symbol->string reg) ", " (number->string triv) "\n")]
      
      [`(set! ,reg1 (,binop ,reg1 ,int32))
      #:when (and (register? reg1) (binop? binop) (int32? int32))
      (string-append x64 (math->ins binop reg1 int32) "\n")]

      [`(set! ,reg1 (,binop ,reg1 ,addr))
      #:when (and (register? reg1) (binop? binop) (loc? addr))
      (string-append x64 (math->ins binop reg1 addr) "\n")]

      [`(with-label ,label ,s)
      #:when (label? label)
      (string-append x64 (label->ins label) "\n" (statement->x64 s ""))]

      [`(jump ,trg)
      #:when (trg? trg)
      (let ([trgStr (if (register? trg)
                        (symbol->string trg)
                        (sanitize-label trg))])
        (string-append x64 "jmp " trgStr "\n"))]

      [`(compare ,reg ,opand)
      #:when (and (register? reg) (opand? opand))
      (string-append x64 "cmp " (symbol->string reg) ", " (opand->ins opand) "\n")]

      [`(jump-if ,relop ,label)
      #:when (and (relop? relop) (label? label))
      (string-append x64 (jump-if-ins relop) " " (sanitize-label label) "\n")]))

  (define (loc? loc)
    (or (register? loc) (address? loc)))

  (define (trg? trg)
    (or (register? trg) (label? trg)))
  
  (define (triv? triv)
    (or (trg? triv) (int64? triv)))

  (define (binop? b)
    (or (equal? b '*) (equal? b '+) (equal? b '-)))

  (define (opand? opand)
    (or (int64? opand) (register? opand)))

  (define (loc->ins loc)
    (if (register? loc)
        (symbol->string loc)
        (addr->ins loc)))

  (define (trg->ins trg)
    (if (register? trg)
        (symbol->string trg)
        (string-append "[rel " (sanitize-label trg) "]")))

  (define (addr->ins addr)
    (string-append "QWORD [" 
                   (symbol->string(first addr)) 
                   " - " 
                   (number->string(third addr))
                   "]"))

  ; rather verbose, but I believe repeating the cases is more readable than abstracting everything
  ; into tiny helper functions
  (define (math->ins binop reg target)
    (cond [(and (int32? target)   (equal? '* binop)) 
           (string-append "imul " (symbol->string reg) ", " (number->string target))]
          [(and (int32? target)   (equal? '+ binop)) 
           (string-append "add "  (symbol->string reg) ", " (number->string target))]
          [(and (int32? target)   (equal? '- binop)) 
           (string-append "sub "  (symbol->string reg) ", " (number->string target))]

          [(and (register? target)(equal? '* binop)) 
           (string-append "imul " (symbol->string reg) ", " (symbol->string target))]
          [(and (address? target) (equal? '* binop)) 
           (string-append "imul " (symbol->string reg) ", " (addr->ins target))]

          [(and (register? target)(equal? '+ binop)) 
           (string-append "add "  (symbol->string reg) ", " (symbol->string target))]
          [(and (address? target) (equal? '+ binop)) 
           (string-append "add "  (symbol->string reg) ", " (addr->ins target))]

          [(and (register? target)(equal? '- binop)) 
           (string-append "sub "  (symbol->string reg) ", " (symbol->string target))]
          [(and (address? target) (equal? '- binop)) 
           (string-append "sub "  (symbol->string reg) ", " (addr->ins target))]))
  
  (define (label->ins label) 
    (string-append (sanitize-label label) ":"))

  (define (opand->ins opand)
    (if (register? opand)
        (symbol->string opand)
        (number->string opand)))

  (define (jump-if-ins relop)
    (cond [(equal? relop '< ) "jl" ]
          [(equal? relop '<=) "jle"]
          [(equal? relop '= ) "je" ]
          [(equal? relop '>=) "jge"]
          [(equal? relop '> ) "jg" ]
          [(equal? relop '!=) "jne"]))

  (program->x64 p))



(module+ test
  (require
   rackunit
   rackunit/text-ui
   cpsc411/langs/v6
   cpsc411/test-suite/public/v6)

  ;; You can modify this pass list, e.g., by adding other
  ;; optimization, debugging, or validation passes.
  ;; Doing this may provide additional debugging info when running the rest
  ;; suite.
  (define pass-map
    (list
     (cons uniquify interp-values-lang-v6)
     (cons sequentialize-let interp-values-unique-lang-v6)
     (cons normalize-bind interp-imp-mf-lang-v6)
     (cons impose-calling-conventions interp-proc-imp-cmf-lang-v6)
     (cons select-instructions interp-imp-cmf-lang-v6)
     (cons uncover-locals interp-asm-pred-lang-v6)
     (cons undead-analysis interp-asm-pred-lang-v6/locals)
     (cons conflict-analysis interp-asm-pred-lang-v6/undead)
     (cons assign-call-undead-variables interp-asm-pred-lang-v6/conflicts)
     (cons allocate-frames interp-asm-pred-lang-v6/pre-framed)
     (cons assign-registers interp-asm-pred-lang-v6/framed)
     (cons assign-frame-variables interp-asm-pred-lang-v6/spilled)
     (cons replace-locations interp-asm-pred-lang-v6/assignments)
     (cons optimize-predicates interp-nested-asm-lang-fvars-v6)
     (cons implement-fvars interp-nested-asm-lang-fvars-v6)
     (cons expose-basic-blocks interp-nested-asm-lang-v6)
     (cons resolve-predicates interp-block-pred-lang-v6)
     (cons flatten-program interp-block-asm-lang-v6)
     (cons patch-instructions interp-para-asm-lang-v6)
     (cons generate-x64 interp-paren-x64-v6)
     (cons wrap-x64-boilerplate #f)
     (cons wrap-x64-run-time #f)))

  (current-pass-list
   (map car pass-map))

  (run-tests
   (v6-public-test-suite
    (current-pass-list)
    (map cdr pass-map))))
