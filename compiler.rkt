#lang racket

(require
 cpsc411/compiler-lib
 cpsc411/2c-run-time
 cpsc411/graph-lib)

(provide
 link-paren-x64
 interp-paren-x64
 interp-values-lang
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
 optimize-predicates
 expose-basic-blocks
 resolve-predicates
 flatten-program
 patch-instructions
 implement-fvars
 generate-x64)

;; Template support macro; feel free to delete
(define-syntax-rule (.... stx ...)
  (error "Unfinished template"))

;; Stubs; remove or replace with your definitions.
(define-values (check-values-lang
                interp-values-lang
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
                ;optimize-predicates
                ;expose-basic-blocks
                ;resolve-predicates
                ;flatten-program
                ;patch-instructions
                ;implement-fvars
                ;generate-x64
                link-paren-x64
                interp-paren-x64
                )
  (values ;necessary do not comment
   values
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
   ;values
   values
   values
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

(define (relop? relop)
  (or (equal? relop '<=)
      (equal? relop '< )
      (equal? relop '= )
      (equal? relop '>=)
      (equal? relop '> )
      (equal? relop '!=)))

; =============== M4 Passes ================

; Input: nested-asm-lang-v4
; Output: nested-asm-lang-v4
; Purpose: Optimize Nested-asm-lang v4 programs by analyzing and simplifying predicates.
(define (optimize-predicates p)

  (define (optimize-p p)
    (match p
      [`(module ,tail)
       `(module ,(optimize-t tail #hash()))]))

  (define (optimize-t t env)
    (match t
      [`(halt ,triv) #:when (number? triv)
        t]
      [`(halt ,triv) ; triv is reg or fvar
       `(halt ,(if (and (dict-has-key? env triv) (number? (dict-ref env triv))) 
                    (dict-ref env triv)
                    triv))]
      [`(begin ,effects ... ,tail)
        (define-values (effRes new-env)
          (for/fold ([acc '()] ; list of processed effects
                     [currEnv env]) 
                    ([e effects])
                    (define-values (new-eff eff-env) ; process the effect and get updated environment
                                   (optimize-e e currEnv))
                    (values (append acc `(,new-eff)) eff-env)))

       `(begin ,@effRes ,(optimize-t tail new-env))]
      [`(if ,pred ,tail1 ,tail2)
        (optimize-pred pred (optimize-t tail1 env) (optimize-t tail2 env) env)]))

  (define (optimize-e e env)
    (match e
      [`(set! ,loc_1 (,binop ,loc_1 ,triv))
        (define (interp-binop binop)
          (match binop
            ['* *]
            ['+ +]))
        (define new-env (if (dict-has-key? env loc_1)
                             (if (number? triv)
                                 (dict-set env loc_1 ((interp-binop binop) (dict-ref env loc_1) triv))
                                 (if (dict-has-key? env triv)
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
        (values (optimize-pred pred e1 e2 env) env)])) ;??? pass in env of chosen effect?

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
      [`(,relop ,loc ,triv)
        (optimize-relop relop loc triv t1 t2 env)]
      [`(false)
        t2]
      [`(true)
        t1]))

  (define (optimize-relop r l triv t1 t2 env)
      (define (interp-relop relop)
        (match relop
          ['< <]
          ['<= <=]
          ['= =]
          ['!= (compose not =)]
          ['> >]
          ['>= >=]))
      
      (if (and (dict-has-key? env l) (int64? (dict-ref env l))) 
        (if (int64? triv)
          (if ((interp-relop r) (dict-ref env l) triv)
                t1
                t2)
          (if (and (dict-has-key? env triv) (int64? (dict-ref env triv)))
            (if ((interp-relop r) (dict-ref env l) (dict-ref env triv))
                t1
                t2)
            `(if (,r ,l ,triv) ,t1 ,t2))) 
        `(if (,r ,l ,triv) ,t1 ,t2)))

  (optimize-p p))

; Input: nested-asm-lang-v4
; Output: block-pred-lang-v4
; Purpose: Compile the Nested-asm-lang v4 to Block-pred-lang v4,
; eliminating all nested expressions by generating fresh basic blocks and jumps.
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
      [`(module ,tail)
        (define tailRes (expose-t tail))
        (add-new-block! (fresh-label '__main) tailRes)
       `(module ,@result-acc)]))

  ; Given a tail, recursively generate blocks. 
  ; Returns the tail block instructions to be used to make a block by the caller
  ; of this function. 
  (define (expose-t t)
    (match t
      [`(halt ,triv) `(,t)]
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

  ; Walk through a list of effects until a predicate
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
      [`(begin ,effects ...) ; flatten nested effects
        (expose-effects (append effects effRest) tail)]
      [`(if ,pred ,effect1 ,effect2)
        (define restResult (expose-effects effRest tail))
        
        (define true_label (fresh-label 'tmp))
        (define false_label (fresh-label 'tmp))
        (define join-label (fresh-label 'tmp))  ; join label: the block where effect1 and effect2 will converge
        
        (define eff1Res (expose-e effect1 '() `((jump ,join-label))))
        (define eff2Res (expose-e effect2 '() `((jump ,join-label))))
        
        (add-new-block! join-label restResult)
        (add-new-block! false_label eff2Res)
        (add-new-block! true_label eff1Res)
        
       `(,(expose-pred pred true_label false_label))]
      [`(set! ,loc ,trivOrBinop)
        (cons e (expose-effects effRest tail))]))

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
  

; Input: block-pred-lang-v4?
; Output: block-asm-lang-v4?
; Purpose: Compile the Block-pred-lang v4 to Block-asm-lang v4 by manipulating the branches of if statements to resolve branches.
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


; Input: block-asm-lang-v4
; Output: para-asm-lang-v4
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

(define (assign-homes-opt p)
  ; Compiles Asm-lang v2 to Nested-asm-lang v2, replacing each abstract location 
  ; with a physical location. This version performs graph-colouring register allocation.

  (replace-locations (assign-registers (conflict-analysis (undead-analysis (uncover-locals p))))))

; Input:    asm-pred-lang-v4/locals
; Output:   asm-pred-lang-v4/undead
; Purpose:  Performs undeadness analysis, decorating the program with undead-set tree. 
;           Only the info field of the program is modified.
; M3 > M4   Handle the new pred syntax. Undead-analysis tree needs to process branches and integrate.
(define (undead-analysis p)

  ; Decorate the program with the undead-out tree.
  (define (undead-p p) 
    (match p
      [`(module ,locals ,tail)
        (define-values (ust x) (undead-tail tail))  ; find the undead-set tree of the tail
       `(module ,(info-set locals `undead-out ust)
                 ,tail)]))

  ; Takes a tail and produces the undead-set tree from the effects
  ; Return: (values undead-set-tree? undead-set?)
  ; i.e. (<tree for tail> <set for next effect>)
  (define (undead-tail t)
    (match t
      [`(halt ,triv)       
        (if (aloc? triv)
            (values '() `(,triv))
            (values '() '()))]
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
        (values `(,predUst ,tail1Ust ,tail2Ust) predIn)]))

  ; Given a pred, return the corresponding undead-out tree.
  ; Use a base undead-out consisting of the union of the undead-outs of the pred branches.
  ; Return: (values undead-set-tree? undead-set?)
  ; i.e. (<tree for pred> <set for next effect>)
  (define (undead-pred undead-out pr)
    (match pr
      [`(,relop ,aloc ,triv) #:when (relop? relop)
        (define add-aloc (set-add undead-out aloc))
        (if (number? triv)
          (values undead-out add-aloc)
          (values undead-out (set-add add-aloc triv)))]
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
      [`(set! ,aloc (,binop ,aloc ,triv))
        (let ([newSet (if (number? triv)
                          (set-add undead-out aloc)
                          (set-add (set-add undead-out triv) aloc))])
          (values undead-out newSet))]
      [`(set! ,aloc ,triv)
        (let ([newSet (if (number? triv)
                      (set-remove undead-out aloc)
                      (set-remove (set-add undead-out triv) aloc))])
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
        (define-values (predUst predIn) (undead-pred (set-union (set-union e1In e2In) undead-out) pred))
        (values `(,predUst ,eff1Ust ,eff2Ust) predIn)]))
  
  (undead-p p))


; Input: asm-pred-lang-v4/undead
; Output: asm-pred-lang-v4/conflicts
; Decorates a program with its conflict graph.
; M3 > M4   Handle the new pred syntax. No significant changes; just in traversal logic.
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
        graph]
      [`(if ,pred ,tail1 ,tail2)
        (define predGraph (c-analysis-pr (first undead) graph pred))
        (define tail1Graph (c-analysis-t (second undead) predGraph tail1))
        (c-analysis-t (third undead) tail1Graph tail2)]))

  ; undead : a nested list of lists of abstract locations such as x.1. 
  ; graph   : a graph of the conflicts found so far.
  ; Return a graph.
  (define (c-analysis-pr undead graph pr)
    (match pr
      [`(not ,pred)
        (c-analysis-pr undead graph pr)]
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
      [_ graph])) ; everything else

  ; undead : the entry in the list of undead relating to the current effect
  ; graph   : a graph of the conflicts found so far.
  ; Return a graph.
  (define (c-analysis-e undead graph e)
    (match e
      [`(set! ,aloc1 ,aloc2) #:when (aloc? aloc2)
        (update-conflicts `(,aloc1 ,aloc2) undead graph)]
      [`(set! ,aloc ,binopOrAloc)
        (update-conflicts `(,aloc) undead graph)]
      [`(begin ,effects ...)
        (for/fold ([g graph])  ; pair effects with entries in the undead list and update graph recursively.
                  ([eff effects] [currUndead undead])
                  (c-analysis-e currUndead g eff))]
      [`(if ,pred ,effect1 ,effect2)
        (define predGraph (c-analysis-pr (first undead) graph pred))
        (define e1Graph (c-analysis-e (second undead) predGraph effect1))
        (c-analysis-e (third undead) e1Graph effect2)]))

  (c-analysis-p p))

 
; Input:    asm-pred-lang-v4/conflicts
; Output:   asm-pred-lang-v4/assignments
; Purpose:  Performs graph-colouring register allocation. 
;           The pass attempts to fit each of the abstract location declared in the locals 
;           set into a register, and if one cannot be found, assigns it a frame variable instead.

; M3 > M4 
; - The allocator should run the same algorithm as before. 
; - Since the allocator doesn’t traverse programs, it shouldn’t need any changes.
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


; Input:   Values-lang v4
; Output:  Values-unique-lang v4
; Purpose: Compiles Values-lang v4 to Values-unique-lang v4 by resolving all lexical 
;          identifiers to abstract locations

; M3 > M4
; - added support for predicate expressions
; - Values-lang v3/v4 Diff:        https://www.students.cs.ubc.ca/~cs-411/2022w2/lang-differ.cgi?lang1=Values-lang-v3&lang2=Values-lang-v4
; - Values-unique-lang v3/v4 Diff: https://www.students.cs.ubc.ca/~cs-411/2022w2/lang-differ.cgi?lang1=Values-unique-lang-v3&lang2=Values-unique-lang-v4
(define (uniquify p) 

  ; Input: tail, a list of possibly nested let statements
  ; Output: Values-unique-lang-v3
  ; Purpose: matches on p, which is a list of (nested) let statements
  (define (uniquify-p p dict-acc)
    (match p
      [`(module ,t ...)
       `(module ,@(map (lambda (t) (uniquify-tail t dict-acc)) t))]))

  ; Input: value, a let statement or a binary operation
  ; Output: uniquified version of this expression
  ; Purpose: recursively uniquify a single expression
  (define (uniquify-tail t binds)
    (match t 
      [`(let ([,as ,vs] ...) ,body)
        (define new-binds (construct-binds as binds)) 
        `(let ,(for/list ([a as][v vs])
                        `[,(dict-ref new-binds a) ,(uniquify-tail v binds)])     
              ,(uniquify-tail body new-binds))]          
      
      [`(if ,p ,t1 ,t2)
       `(if ,(uniquify-pred p  binds)
            ,(uniquify-tail      t1 binds) 
            ,(uniquify-tail      t2 binds))]
      
      [value (uniquify-value value binds)]))

  (define (uniquify-value v binds)
    (match v
       [`(if ,p ,v1 ,v2)
        `(if ,(uniquify-pred p  binds) 
             ,(uniquify-value     v1 binds)
             ,(uniquify-value     v2 binds))]

       [`(,binop ,triv1 ,triv2)
       `(,binop ,(update-bind triv1 binds) ,(update-bind triv2 binds))]

       [triv
        (update-bind triv binds)]
      )
  )

  (define (uniquify-pred p binds)
    (match p
      [`(,relop ,triv1 ,triv2)
      #:when (relop? relop)
      `(,relop ,(update-bind triv1 binds) ,(update-bind triv2 binds))]

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
  ; Output: int64 or aloc
  ; Purpose: return the triv itself if it is an int64, otherwise look up the bind in the dictionary
  (define (update-bind x binds)
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

  ; Input: aloc and binding dictionary
  ; Output: a new binding dictionary
  ; uses fresh to assign a unique aloc? to each name? at the current scope
  (define (construct-binds xs binds)
    (for/fold ([new-binds binds]) ; accumulator
              ([x xs])            ; x is an element, xs is the list
      (dict-set new-binds x (fresh x))))                                    

  (uniquify-p p '()))


; Input:   values-unique-lang-v4
; Output:  imp-mf-lang-v4
; Compiles Values-unique-lang v4 to Imp-mf-lang v4 by picking a
; particular order to implement let expressions using set!.
; M2 > M4  Handle the new pred syntax. No significant changes; just in traversal logic.
(define (sequentialize-let p)
  
  (define (seq-p p)
    (match p
      [`(module ,tail)
          `(module ,(seq-t tail))]))

  ; Return an instruction
  (define (seq-t t)
    (match t
      [`(let ([,aloc ,value] ...) ,tail) 
       `(begin ,@(map seq-bind (map list aloc value)) ; zip the aloc and value lists
                ,(seq-t tail))]
      [`(if ,pred ,tail1 ,tail2)
       `(if ,(seq-pr pred) ,(seq-t tail1) ,(seq-t tail2))]
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
      [`(let ([,aloc ,value] ...) ,val) 
       `(begin ,@(map seq-bind (map list aloc value)) ; zip the aloc and value lists
               ,@(seq-v val))]
      [`(if ,pred ,val1 ,val2)
       `(if ,(seq-pr pred) ,(seq-v val1) ,(seq-v val2))]
      [_ v])) ; triv or binop

  (seq-p p))


; Input:   imp-mf-lang-v4
; Output:  imp-cmf-lang-v4
; Compiles Imp-mf-lang v4 to Imp-cmf-lang v4, pushing set! under begin and if so that
; the right-hand-side of each set! is a simple value-producing operation.
; M3 > M4 : handle predicates. Push if above set.
(define (normalize-bind p)

  (define (n-bind-p p)
    (match p
      [`(module ,tail)
          `(module ,(n-bind-t tail))]))
  
  ; Return an instruction
  (define (n-bind-t t)
    (match t
      [`(begin ,eff ... ,tail)
       `(begin ,@(map n-bind-e eff) ,(n-bind-t tail))]
      [`(if ,pred ,tail1 ,tail2)
       `(if ,(n-bind-pr pred) ,(n-bind-t tail1) ,(n-bind-t tail2))]
      [value
        (n-bind-v value)]))
  
  ; Return an instruction
  (define (n-bind-v v)
    (match v
      [`(begin ,eff ... ,val) 
        `(begin ,@(map n-bind-e eff) ,(n-bind-v val))]
      [`(if ,pred ,value1 ,value2)
       `(if ,(n-bind-pr pred) ,(n-bind-v value1) ,(n-bind-v value2))]
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

; Input:   imp-cmf-lang-v4
; Output:  asm-pred-lang-v4
; Purpose: Compiles Imp-cmf-lang v4 to Asm-pred-lang v4, selecting appropriate sequences of 
;          abstract assembly instructions to implement the operations of the source language.
(define (select-instructions p)

  (define (sel-ins-p p)
    (match p
      [`(module ,tail)
        (if (or (number? tail) (aloc? tail) (equal? (first tail) 'if))
          `(module () ,@(sel-ins-t tail)) ; dispense with 'begin' when top-level tail is a triv
          `(module () (begin ,@(sel-ins-t tail))))]))
  
  ; Return a list of instructions
  (define (sel-ins-t t)
    (match t
      [`(begin ,effects ... ,tail) ; flattens nested begins
       `(,@(splice-mapped-list (map sel-ins-e effects)) ,@(sel-ins-t tail))]
      [value #:when (or (number? value) (aloc? value))
        (sel-ins-v value '_)]
      [`(,binop ...) #:when (and (member (first binop) '(+ *)) #t)
        ; tail binop: need to create a temporary aloc
        (define getfresh (fresh))
        `(,@(sel-ins-v binop getfresh) (halt ,getfresh))]
      [`(if ,pred ,tail1 ,tail2)
        (define tailRes1 (wrap (sel-ins-t tail1)))
        (define tailRes2 (wrap (sel-ins-t tail2)))
       `((if ,(sel-ins-pr pred) ,tailRes1 ,tailRes2))]))
  
  (define (sel-ins-v v aloc)
    (match v
      [value #:when (or (number? value) (aloc? value)) ; value is a triv
        `((halt ,value))]
      [`(,binop ...)
        (sel-ins-b binop aloc)]))

  (define (sel-ins-pr pr)
    (match pr
      [`(,bool) #:when (and (member bool '(true false)) #t)
        pr]
      [`(not ,pred)
       `(not ,(sel-ins-pr pred))]
      [`(,relop ,triv1 ,triv2) #:when (relop? relop)
        (define getfresh (fresh))
       `(begin (set! ,getfresh ,triv1) 
               (,relop ,getfresh ,triv2))]
      [`(begin ,effects ... ,pred)
       `(begin ,@(map wrap (map sel-ins-e effects)) ,(sel-ins-pr pred))]
      [`(if ,preds ...)
       `(if ,@(map sel-ins-pr preds))]))
  
  ; Return a sequence of instructions dealing with a binop,
  ; Given an aloc to use.
  ; e.g. sel-ins-b (+ 2 4) tmp.1 ->
  ; ((set! tmp.1 2) (set! tmp.1 (+ tmp.1 4)))
  (define (sel-ins-b b aloc)
    (match b
      [`(,binop ,triv1 ,triv2)
           `((set! ,aloc ,triv1)
             (set! ,aloc (,binop ,aloc ,triv2)))]))
  
  ; Process an effect and return a list of instructions
  (define (sel-ins-e e)
    (match e
      [`(set! ,aloc (,binop ...))
        (sel-ins-b binop aloc)]
      [`(begin ,eff ...)  ; flatten nested begins
        `(,@(splice-mapped-list (map sel-ins-e eff)))]
      [`(set! ,aloc ,triv) `(,e)]
      [`(if ,pred ,effect1 ,effect2)
        ; wrap the effect result in (begin ) if it is multiple instructions.
        ; do NOT flatten begins if the effect was already a (begin ).
        (define effRes1 (if (equal? (first effect1) 'begin) 
                           `(begin ,@(sel-ins-e effect1)) 
                            (wrap (sel-ins-e effect1))))
        (define effRes2 (if (equal? (first effect2) 'begin) 
                           `(begin ,@(sel-ins-e effect2)) 
                            (wrap (sel-ins-e effect2))))
       `((if ,(sel-ins-pr pred) ,effRes1 ,effRes2))]))

  ; Helper for sel-ins-e and sel-ins-t. Wrap a list of instructions in begin.
  ; If it's a single instruction return that instruction.
  (define (wrap lst)
    (match lst
      [`(,e ...) #:when (and (> (length e) 1) (list? (first e))) ; e.g. ((set 1 2) (set 1 2))
       `(begin ,@e)]
      [`(,x) ; e.g. ((halt 1))
        `(,@x)]
      [_ lst])) ; e.g. (halt 1)

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
      [`(module ,info ,tail)
       `(module ,(replace-loc-t tail info))]))

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
      [`(halt ,triv)
        (if (aloc? triv) `(halt ,(get-repl triv as)) `(halt ,triv))]
      [`(if ,pred ,tail1 ,tail2)
        `(if ,(replace-loc-pred as pred) ,(replace-loc-t tail1 as) ,(replace-loc-t tail2 as))]))

  (define (replace-loc-e as e)
    (match e
      [`(set! ,aloc ,triv) #:when (or (number? triv) (aloc? triv))
        `(set! ,(get-repl aloc as) ,(if (aloc? triv) (get-repl triv as) triv))]
      [`(set! ,aloc (,binop ...))
        `(set! ,(get-repl aloc as) ,(replace-loc-b binop as))]
      [`(begin ,effects ...)
        `(begin ,@(map (curry replace-loc-e as) effects))]
      [`(if ,pred ,effect1 ,effect2)
        `(if ,(replace-loc-pred as pred) ,(replace-loc-e as effect1) ,(replace-loc-e as effect2))]))

  (define (replace-loc-pred as p)
    (match p
      [`(if ,pred1 ,pred2 ,pred3)
        `(if ,(replace-loc-pred as pred1) ,(replace-loc-pred as pred2) ,(replace-loc-pred as pred3))]
      [`(not ,pred)
        `(not ,(replace-loc-pred as pred))]
      [`(begin ,effect ... ,pred)
        `(begin ,@(map (curry replace-loc-e as) effect) ,(replace-loc-pred as pred))]
      [`(,relop ,aloc ,triv)
        `(,relop ,(get-repl aloc as) ,(if (aloc? triv) (get-repl triv as) triv))]
      [`(true)
        `(true)]
      [`(false)
        `(false)]))

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


; Input: asm-pred-lang-v4
; Output: asm-pred-lang-v4/locals
; Purpose: Compiles Asm-pred-lang v4 to Asm-pred-lang v4/locals, analysing which abstract locations are used in the module and decorating the module with the set of variables in an info field.

; M2 > M4
; - extend language to deal with control flow

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
        (if (aloc? triv) (set-add acc triv) acc)]
      [`(if ,pred ,tail1 ,tail2)
        (set-union 
        (uloc-pred pred)
        (uloc-t tail1 acc)
        (uloc-t tail2 acc)
        )]))

  (define (uloc-e e)
    ; return: list of all alocs found in effect
    (match e
      [`(set! ,aloc ,triv) #:when (or (number? triv) (aloc? triv))
        (if (aloc? triv) `(,aloc ,triv) `(,aloc))]    
      [`(set! ,aloc (,binop ...))
        (uloc-b binop)]
      [`(if ,pred ,effect1 ,effect2)
       (set-union
        (uloc-pred pred)
        (uloc-e effect1)
        (uloc-e effect2))]
      [`(begin ,effects ...)
        (foldl append '() (map uloc-e effects))]
        ))

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
      [`(,relop ,aloc ,triv)
        (if (aloc? triv) `(,aloc ,triv) `(,aloc))]
      [`(true)
        `()]
      [`(false)
        `()]))

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


; Input: para-asm-lang-v4
; Output: paren-x64-fvars-v4
; Purpose: Compiles Para-asm-lang v4 to Paren-x64-fvars v4 by patching each instruction that has no x64 analogue into a sequence of instructions using auxiliary register from current-patch-instructions-registers.

; M2 > M4
; - extend language to deal with control flow
(define (patch-instructions p)
  ; Compiles Para-asm-lang v2 to Paren-x64-fvars v2 by patching instructions that have 
  ; no x64 analogue into a sequence of instructions.

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
      [`(set! ,fvar1 ,fvar2) #:when (and (fvar? fvar1) (fvar? fvar2))
          `((set! ,i-reg1 ,fvar2)
            (set! ,fvar1 ,i-reg1))]
      [`(set! ,fvar1 ,int64) #:when (and (not (int32? int64)) (int64? int64) )
          `((set! ,i-reg1 ,int64)
            (set! ,fvar1 ,i-reg1))]
      [`(set! ,fvar (,binop ...)) #:when (fvar? fvar)
          `((set! ,i-reg1 ,fvar)
            (set! ,i-reg1 ,(patch-binop binop))
            (set! ,fvar ,i-reg1))]
      [`(halt ,opand)
          `((set! ,ret-reg ,opand)
            (jump done))]
      [`(with-label ,label (halt ,opand))
          `((with-label ,label (set! ,ret-reg ,opand))
            (jump done))]
      [`(with-label ,label ,s)
          `(with-label ,label ,(patch-effect s))]
      [`(jump ,trg) #:when (fvar? trg)
          `((set! ,i-reg1 ,trg)
            (jump ,i-reg1))]
      [`(compare ,fvar1 ,fvar2) #:when (and (fvar? fvar1) (fvar? fvar2))
          `((set! ,i-reg2 ,fvar2)
            (set! ,i-reg1 ,fvar1)
            (compare ,i-reg1 ,i-reg2))]
      [`(compare ,reg ,fvar1) #:when (fvar? fvar1)
          `((set! ,i-reg1 ,fvar1)
            (compare ,reg ,i-reg1))]
      [`(jump-if ,relop ,trg) #:when (not (label? trg))
          `((jump-if ,(patch-relop relop) L.tmp.1)
            (jump ,trg)
            (with-label L.tmp.1 (set! ,i-reg1 ,i-reg1)))]
      [_ e]))  ; everything else

  (define (patch-binop b)
    (match b
      [`(,binop ,fvar ,val) #:when (fvar? fvar)
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


; Input: paren-x64-fvars-v4
; Output: paren-x64-v4
; Purpose: Compile the Paren-x64-fvars v4 to Paren-x64 v4 by reifying fvars into displacement mode operands.

; M2 > M4
; - extend language to deal with control flow
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

; Input: paren-x64-v4
; Output: x64-instructions
; Purpose: Compile the Paren-x64 v4 program into a valid sequence of x64 instructions, represented as a string.

; M3 > M4
; - extend language to deal with jumps, conditional jumps, and labels
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
      #:when (and (address? addr) (register? trg))
      (string-append x64 "mov " (addr->ins addr) ", " (trg->ins trg) "\n")]

      [`(set! ,addr ,trg)
      #:when (and (address? addr) (label? trg))
      (string-append x64 "lea " (addr->ins addr) ", " (trg->ins trg) "\n")]

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

  (define (address? addr)
    (and (list? addr)
         (frame-base-pointer-register? (first addr))
         (equal? '- (second addr))
         (dispoffset? (third addr))))
  
  (define (triv? triv)
    (or (trg? triv) (int64? triv)))

  (define (binop? b)
    (or (equal? b '*) (equal? b '+)))

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

  (define (math->ins binop reg target)
    (cond [(and (int32? target)   (equal? '* binop)) 
           (string-append "imul " (symbol->string reg) ", " (number->string target))]
          [(and (int32? target)   (equal? '+ binop)) 
           (string-append "add "  (symbol->string reg) ", " (number->string target))]
          [(and (register? target)(equal? '* binop)) 
           (string-append "imul " (symbol->string reg) ", " (symbol->string target))]
          [(and (address? target) (equal? '* binop)) 
           (string-append "imul " (symbol->string reg) ", " (addr->ins target))]
          [(and (register? target)(equal? '+ binop)) 
           (string-append "add "  (symbol->string reg) ", " (symbol->string target))]
          [(and (address? target) (equal? '+ binop)) 
           (string-append "add "  (symbol->string reg) ", " (addr->ins target))]))
  
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
   cpsc411/langs/v3
   cpsc411/langs/v2-reg-alloc
   cpsc411/langs/v2
   cpsc411/test-suite/public/v3
   cpsc411/test-suite/public/v2-reg-alloc
   cpsc411/langs/v4
   cpsc411/test-suite/public/v4
   racket/engine
   )

  ;; Milliseconds (any/c -> any_1) (() -> any_2) -> any_1 or any_2
  ;; Runs proc in an engine, returning its result, or calling the failure
  ;; continuation of proc fails to finish before timeout-ms milliseconds.
  (define (run-with-timeout timeout-ms proc
                            [fail-k (lambda () (error "Timed out"))])
    (let* ([e (engine proc)]
           [res (engine-run timeout-ms e)])
      (unless res
        (fail-k))
      (engine-result e)))

  ;; (any/c -> any/c) Milliseconds -> void
  ;; Checks that th *does* timeout after ms milliseconds
  ;; Silently passes or fails with (fail-check) if the test fails
  (define-check (check-timeout? th ms)
    (when (run-with-timeout ms th (lambda () #t))
      (fail-check)))

  (check-timeout?
   (lambda (_)
    (interp-paren-x64
     '(begin
        (with-label L.f.10 (jump L.f.10)))))
   2000)

  ;; You can modify this pass list, e.g., by adding check-assignment, or other
  ;; debugging and validation passes.
  ;; Doing this may provide additional debugging info when running the rest
  ;; suite.
  (define pass-map
    (list
     (cons check-values-lang interp-values-lang-v4)
     (cons uniquify interp-values-lang-v4)
     (cons sequentialize-let interp-values-unique-lang-v4)
     (cons normalize-bind interp-imp-mf-lang-v4)
     (cons select-instructions interp-imp-cmf-lang-v4)

     (cons uncover-locals interp-asm-pred-lang-v4)
     (cons undead-analysis interp-asm-pred-lang-v4/locals)
     (cons conflict-analysis interp-asm-pred-lang-v4/undead)
     (cons assign-registers interp-asm-pred-lang-v4/conflicts)
     (cons replace-locations interp-asm-pred-lang-v4/assignments)

     (cons optimize-predicates interp-nested-asm-lang-v4)
     (cons expose-basic-blocks interp-nested-asm-lang-v4)
     (cons resolve-predicates interp-block-pred-lang-v4)
     (cons flatten-program interp-block-asm-lang-v4)
     (cons patch-instructions interp-para-asm-lang-v4)
     (cons implement-fvars interp-paren-x64-fvars-v4)
     (cons generate-x64 interp-paren-x64-v4)
     (cons wrap-x64-run-time #f)
     (cons wrap-x64-boilerplate #f)))

  (current-pass-list
   (map car pass-map))

  (run-tests
   (v4-public-test-suite
    (current-pass-list)
    (map cdr pass-map)

    link-paren-x64
    interp-paren-x64
    interp-values-lang
    check-values-lang)))
