#lang racket

(require
 cpsc411/compiler-lib
 cpsc411/ptr-run-time
 cpsc411/graph-lib
 racket/syntax)

(provide
 uniquify
 implement-safe-primops
 specify-representation
 remove-complex-opera*
 sequentialize-let
 normalize-bind
 impose-calling-conventions
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
 implement-fvars
 optimize-predicates
 expose-basic-blocks
 resolve-predicates
 flatten-program
 patch-instructions
 implement-mops
 generate-x64)

;; Stubs; remove or replace with your definitions.
; (define-values (uniquify
;                 implement-safe-primops
;                 specify-representation
;                 remove-complex-opera*
;                 sequentialize-let
;                 normalize-bind
;                 impose-calling-conventions
;                 select-instructions
;                 expose-allocation-pointer
;                 uncover-locals
;                 undead-analysis
;                 conflict-analysis
;                 assign-call-undead-variables
;                 allocate-frames
;                 assign-registers
;                 assign-frame-variables
;                 replace-locations
;                 implement-fvars
;                 optimize-predicates
;                 expose-basic-blocks
;                 resolve-predicates
;                 flatten-program
;                 patch-instructions
;                 implement-mops
;                 generate-x64)
;   (values
;    values
;    values
;    values
;    values
;    values
;    values
;    values
;    values
;    values
;    values
;    values
;    values
;    values
;    values
;    values
;    values
;    values
;    values
;    values
;    values
;    values
;    values
;    values
;    values
;    values))

; ================= Global Variables =================

(define fbp (current-frame-base-pointer-register))

(define hbp (current-heap-base-pointer-register))

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

; return true if the value is an address, otherwise false
(define (address? addr)
  (and (list? addr)
       (or (and (frame-base-pointer-register? (first addr))
                (equal? '- (second addr))
                (dispoffset? (third addr)))
           (and (register? (first addr))
                (equal? '+ (second addr))
                (int32? (third addr)))
           (and (register? (first addr))
                (equal? '+ (second addr))
                (register? (third addr))))))

; return true if value is relop, false otherwise
(define (relop? relop)
  (or (equal? relop '<=)
      (equal? relop '< )
      (equal? relop '= )
      (equal? relop '>=)
      (equal? relop '> )
      (equal? relop '!=)))


; =============== M8 Passes ================

; Input:   paren-x64-mops-v8
; Output:  paren-x64-mops-v8
; Purpose: Compiles mops to instructios on pointers with index and displacement-mode operands.
(define (implement-mops p)

  (define (mop-p p) 
    (match p
      [`(begin ,ss ...)
       `(begin ,@(map mop-s ss))]))

  (define (mop-s s)
    (match s
      [`(with-label ,label ,s)
       `(with-label ,label ,(mop-s s))]
      [`(set! ,reg1 (mref ,reg2 ,index))
       `(set! ,reg1 (,reg2 + ,index))]
      [`(mset! ,reg1 ,index ,int32) #:when (int32? int32)
       `(set! (,reg1 + ,index) ,int32)]
      [`(mset! ,reg1 ,index ,trg) #:when (or (register? trg) (label? trg))
       `(set! (,reg1 + ,index) ,trg)]
      [_ s]))

  (mop-p p))


; Input:   asm-alloc-lang-v8
; Output:  asm-pred-lang-v8
; Purpose: Implements the allocation primitive in terms of pointer arithmetic on the current-heap-base-pointer-register.
; Transformation: 
;   `set!, loc (alloc, index)) 
; 
;   `(begin
;       (set! ,loc ,hbp)
;       (set! ,hbp (+ ,hbp ,index)))
(define (expose-allocation-pointer p)

  (define (expose-alloc-p p)
    (match p
      [`(module ,info ,ds ... ,tail)
       `(module ,info ,@(map expose-alloc-define ds) ,(expose-alloc-tail tail))]))

  (define (expose-alloc-define d)
    (match d
      [`(define ,label ,info ,tail)
       `(define ,label ,info ,(expose-alloc-tail tail))]))

  (define (expose-alloc-tail t)
    (match t
      [`(begin ,fx ... ,tail)
       `(begin ,@(map expose-alloc-effect fx) ,(expose-alloc-tail tail))]
      [`(if ,pred ,tail1 ,tail2)
       `(if ,(expose-alloc-pred pred) ,(expose-alloc-tail tail1) ,(expose-alloc-tail tail2))]
      [_ t]))

  (define (expose-alloc-effect e)
    (match e
      [`(begin ,fx ...)
       `(begin ,@(map expose-alloc-effect fx))]
      [`(if ,pred ,effect1 ,effect2)
       `(if ,(expose-alloc-pred pred)
            ,(expose-alloc-effect effect1)
            ,(expose-alloc-effect effect2))]
      [`(return-point ,label ,tail)
       `(return-point ,label ,(expose-alloc-tail tail))]
      [`(set! ,loc (,alloc ,index))
       `(begin (set! ,loc ,hbp) (set! ,hbp (+ ,hbp , index)))]
      [_ e]))

  (define (expose-alloc-pred p)
    (match p
      [`(not ,pred)
       `(not ,(expose-alloc-pred pred))]
      [`(begin ,fx ... ,pred)
       `(begin ,@(map expose-alloc-effect fx) ,(expose-alloc-pred pred))]
      [`(if ,pred1 ,pred2 ,pred3)
       `(if (expose-alloc-pred pred1)
            (expose-alloc-pred pred2)
            (expose-alloc-pred pred3))]
      [_ p]))

  (expose-alloc-p p))

; =============== M7 Passes ================

; Input:   exprs-unique-lang-v8
; Output:  exprs-unsafe-data-lang-v8
; Purpose: Implement safe primitive operations by inserting procedure definitions for each primitive 
;          operation which perform dynamic tag checking, to ensure type safety.
(define (implement-safe-primops p) 
  
  (define fn-acc '()) ; list of all the functions created
  (define fn-count 5) ; number of functions added (not including hardcoded ones). Starts at 5.
  (define label-dict (make-hash)) ; mapping between primops and labels

  ; map binops and unops to numbers.
  ; for binop, doubles as error code.
  (define binops #hash((* . 1) (+ . 2) (- . 3) (< . 4) 
                       (<= . 5) (> . 6) (>= . 7) (eq? . 18)
                       (cons . 17)))
  (define unops #hash((fixnum? . 37) (boolean? . 38) (empty? . 39) (void? . 40)
                       (ascii-char? . 41) (error? . 42) (not . 45)
                       (pair? . 43) (vector? . 44) (car . 35) (cdr . 36) 
                       (vector-length . 29)))
  
  ; binop function generator. Create the function and add it to label-dict and fn-acc.
  ; Returns a label.
  (define (generate-binop binop)
    ; each (binop . x) has errorcode = x and function (lambda (tmp.a tmp.b) ...) where
    ;  a = 12 + x*2
    ;  b = a+1
    (define id (dict-ref binops binop))
    (define tmp-a (format-symbol "tmp.~a" (+ 12 (* id 2))))
    (define tmp-b (format-symbol "tmp.~a" (+ 13 (* id 2))))
    (define new-fn (case binop
                      [(eq? cons) `(lambda (,tmp-a ,tmp-b) (,binop ,tmp-a ,tmp-b))]
                      [else `(lambda (,tmp-a ,tmp-b)
                                     (if (fixnum? ,tmp-b)
                                         (if (fixnum? ,tmp-a)
                                             (,(format-symbol "unsafe-fx~a" binop) ,tmp-a ,tmp-b)
                                             (error ,id))
                                         (error ,id)))]))
    (define new-label (format-symbol "L.~a.~a" binop fn-count))
    (set! fn-count (+ fn-count 1))
    (set! fn-acc (cons `(define ,new-label ,new-fn) fn-acc))
    (dict-set! label-dict binop new-label)
    new-label)
  
  ; unop function generator. Create the function and add it to label-dict and fn-acc.
  ; Returns a label.
  (define (generate-unop unop)
    (define id (dict-ref unops unop))
    (define tmp (format-symbol "tmp.~a" id))
    (define new-label (format-symbol "L.~a.~a" unop fn-count))
    (set! fn-count (+ fn-count 1))
    (define new-fn (case unop
                      [(car cdr) 
                      `(lambda (,tmp) (if (pair? ,tmp) (,(format-symbol "unsafe-~a" unop) ,tmp) (error ,(+ 12 (- id 35)))))]
                      [(vector-length)
                       '(lambda (tmp.29) (if (vector? tmp.29) (unsafe-vector-length tmp.29) (error 9)))]
                      [else `(lambda (,tmp) (,unop ,tmp))]))
    (set! fn-acc (cons `(define ,new-label ,new-fn) fn-acc))
    (dict-set! label-dict unop new-label)
    new-label)

  ; Generate make-vector function and add the two functions needed for 
  ; make-vector, make-init-vector and vector-init-loop.
  ; Returns a label.
  (define (generate-make-vec)
    (define new-label (format-symbol "L.make-vector.~a" fn-count))
    (set! fn-count (+ fn-count 1))
    (define make-vec `(define ,new-label 
                          (lambda (tmp.28) (if (fixnum? tmp.28) (call L.make-init-vector.1 tmp.28) (error 8)))))
    (define make-init '(define L.make-init-vector.1
                          (lambda (tmp.1)
                          (if (unsafe-fx>= tmp.1 0)
                              (let ((tmp.2 (unsafe-make-vector tmp.1)))
                              (call L.vector-init-loop.4 tmp.1 0 tmp.2))
                              (error 12)))))
    (define init-loop '(define L.vector-init-loop.4
                          (lambda (len.3 i.5 vec.4)
                          (if (eq? len.3 i.5)
                              vec.4
                              (begin
                              (unsafe-vector-set! vec.4 i.5 0)
                              (call L.vector-init-loop.4 len.3 (unsafe-fx+ i.5 1) vec.4))))))
    (set! fn-acc (cons init-loop (cons make-init (cons make-vec fn-acc))))
    (dict-set! label-dict 'make-vector new-label)
    new-label)

  ; Generate vector-set! function and add the unsafe-vector-set! function needed.
  ; Returns a label.
  (define (generate-vec-set)
    (define new-label (format-symbol "L.vector-set!.~a" fn-count))
    (set! fn-count (+ fn-count 1))
    (define vec-set `(define ,new-label 
                      (lambda (tmp.30 tmp.31 tmp.32)
                        (if (fixnum? tmp.31)
                            (if (vector? tmp.30)
                            (call L.unsafe-vector-set!.2 tmp.30 tmp.31 tmp.32)
                            (error 10))
                            (error 10)))))
    (define unsafe-set '(define L.unsafe-vector-set!.2
                        (lambda (tmp.6 tmp.7 tmp.8)
                        (if (unsafe-fx< tmp.7 (unsafe-vector-length tmp.6))
                            (if (unsafe-fx>= tmp.7 0)
                            (begin (unsafe-vector-set! tmp.6 tmp.7 tmp.8) (void))
                            (error 10))
                            (error 10)))))
    (set! fn-acc (cons unsafe-set (cons vec-set fn-acc)))
    (dict-set! label-dict 'vector-set! new-label)
    new-label)

  ; Generate vector-ref function and add the unsafe-vector-ref! function needed.
  ; Returns a label.
  (define (generate-vec-ref)
    (define new-label (format-symbol "L.vector-ref.~a" fn-count))
    (set! fn-count (+ fn-count 1))
    (define vec-ref `(define ,new-label 
                      (lambda (tmp.33 tmp.34)
                      (if (fixnum? tmp.34)
                          (if (vector? tmp.33)
                          (call L.unsafe-vector-ref.3 tmp.33 tmp.34)
                          (error 11))
                          (error 11)))))
    (define unsafe-ref '(define L.unsafe-vector-ref.3
                          (lambda (tmp.11 tmp.12)
                          (if (unsafe-fx< tmp.12 (unsafe-vector-length tmp.11))
                              (if (unsafe-fx>= tmp.12 0)
                              (unsafe-vector-ref tmp.11 tmp.12)
                              (error 11))
                              (error 11)))))
    (set! fn-acc (cons unsafe-ref (cons vec-ref fn-acc)))
    (dict-set! label-dict 'vector-ref new-label)
    new-label)

  (define (primop-p p)
    (match p
      [`(module ,defines ... ,value)
        (define definesRes (map primop-def defines))
        (define valRes (primop-v value))
       `(module ,@fn-acc ,@definesRes ,valRes)]))

  (define (primop-def d)
    (match d
      [`(define ,label (lambda (,aloc ...) ,value))
       `(define ,label (lambda ,aloc ,(primop-v value)))]))

  (define (primop-v v)
    (match v
      [`(call ,val ,values ...)
        (define valsRes (map primop-v values))
        (define valRes (primop-v val))
       `(call ,valRes ,@valsRes)]
      [`(let ([,aloc ,value] ...) ,val)
        (define valsRes (map list aloc (map primop-v value))) ; zip alocs with processed vals
        (define valRes (primop-v val))
       `(let ,valsRes ,valRes)]
      [`(if ,val1 ,val2 ,val3)
       `(if ,(primop-v val1) ,(primop-v val2) ,(primop-v val3))]
      [triv 
        (primop-triv triv)]))
  
  ; Generate a function and add to the accumulator if it is not there already.
  ; Return the appropriate label, or t if t is not a primop.
  (define (primop-triv t)
    (match t
      [binop #:when (dict-has-key? binops binop)
        (if (dict-has-key? label-dict binop)
            (dict-ref label-dict binop)
            (generate-binop binop))]
      ['make-vector
        (if (dict-has-key? label-dict 'make-vector)
            (dict-ref label-dict 'make-vector)
            (generate-make-vec))]
      ['vector-set!
        (if (dict-has-key? label-dict 'vector-set!)
            (dict-ref label-dict 'vector-set!)
            (generate-vec-set))]
      ['vector-ref
        (if (dict-has-key? label-dict 'vector-ref)
            (dict-ref label-dict 'vector-ref)
            (generate-vec-ref))]
      [unop #:when (dict-has-key? unops unop)
        (if (dict-has-key? label-dict unop)
            (dict-ref label-dict unop)
            (generate-unop unop))]
      [_ t])) ; everything else
      
  (primop-p p))


; Input:   exprs-unsafe-data-lang-v8
; Output:  exprs-bits-lang-v8
; Purpose: Compiles immediate data and primitive operations into their implementations as 
;          ptrs and primitive bitwise operations on ptrs.
(define (specify-representation p)
  
  (define binops #hash((unsafe-fx* . *) (unsafe-fx+ . +) (unsafe-fx- . -) (eq? . =) 
                       (unsafe-fx< . <) (unsafe-fx<= . <=) (unsafe-fx> . >) (unsafe-fx>= . >=)))

  (define (specify-p p)
    (match p
      [`(module ,defines ... ,value)
       `(module ,@(map specify-d defines) ,(specify-v value))]))

  (define (specify-d d)
    (match d
      [`(define ,label (lambda (,aloc ...) ,value))
       `(define ,label (lambda ,aloc ,(specify-v value)))]))

  (define (specify-v v)
    (match v
      [`(call ,values ...)
       `(call ,@(map specify-v values))]
      [`(let (,assigns ...) ,value)
       `(let ,(map specify-assign assigns) ,(specify-v value))]
      [`(if ,value1 ,value2 ,value3)
       `(if ,(specify-pred value1) ,(specify-v value2) ,(specify-v value3))]
      [`(begin ,effects ... ,value)
       `(begin ,@(map specify-e effects) ,(specify-v value))]
      [`(,primop ,values ...) #:when (not (or (equal? primop 'error) (equal? primop 'void)))
        (specify-primop primop values)]
      [triv
        (specify-triv triv)]))

  (define (specify-e e)
    (match e
      [`(begin ,effects ...)
       `(begin ,@(map specify-e effects))]
      [`(,primop ,values ...)
        (specify-primop primop values)]))

  ; Given an aloc and a value, return the same pair but with the processed value.
  (define (specify-assign a)
    (match a
      [`(,aloc ,value)
       `(,aloc ,(specify-v value))]))

  ; Handles a value in pred position
  (define (specify-pred p)
    (match p
      [`(not ,pred)
       `(not ,(specify-pred pred))]
      [`(let (,assigns ...) ,pred) 
       `(let ,(map specify-assign assigns) ,(specify-pred pred))]
      [`(if ,pred ,pred1 ,pred2)
       `(if ,(specify-pred pred) ,(specify-pred pred1) ,(specify-pred pred2))]
      [_ `(!= ,(specify-v p) ,(current-false-ptr))]))

  (define (specify-triv t)
    (match t
      ['#t (current-true-ptr)]
      ['#f (current-false-ptr)]
      ['empty (current-empty-ptr)]
      [`(void) (current-void-ptr)]
      [`(error ,uint8)
        (bitwise-ior (arithmetic-shift uint8 (current-error-shift)) (current-error-tag))]
      [ascii-char-literal #:when (ascii-char-literal? ascii-char-literal)
        (bitwise-ior (arithmetic-shift (char->integer ascii-char-literal) (current-ascii-char-shift)) (current-ascii-char-tag))]
      [fixnum #:when (int61? fixnum)
        (bitwise-ior (arithmetic-shift fixnum (current-fixnum-shift)) (current-fixnum-tag))]
      [_ t])) ; label or aloc

  (define (specify-primop p v)
    (match p
      ['fixnum?     `(if (= (bitwise-and ,@(map specify-v v) ,(current-fixnum-mask)) ,(current-fixnum-tag)) ,(current-true-ptr) ,(current-false-ptr))]
      ['boolean?    `(if (= (bitwise-and ,@(map specify-v v) ,(current-boolean-mask)) ,(current-boolean-tag)) ,(current-true-ptr) ,(current-false-ptr))]
      ['empty?      `(if (= (bitwise-and ,@(map specify-v v) ,(current-empty-mask)) ,(current-empty-tag)) ,(current-true-ptr) ,(current-false-ptr))]
      ['void?       `(if (= (bitwise-and ,@(map specify-v v) ,(current-void-mask)) ,(current-void-tag)) ,(current-true-ptr) ,(current-false-ptr))]
      ['ascii-char? `(if (= (bitwise-and ,@(map specify-v v) ,(current-ascii-char-mask)) ,(current-ascii-char-tag)) ,(current-true-ptr) ,(current-false-ptr))]
      ['error?      `(if (= (bitwise-and ,@(map specify-v v) ,(current-error-mask)) ,(current-error-tag)) ,(current-true-ptr) ,(current-false-ptr))]
      ['not         `(if (!= ,@(map specify-v v) ,(current-false-ptr)) ,(current-false-ptr) ,(current-true-ptr))]
      ['pair?       `(if (= (bitwise-and ,@(map specify-v v) ,(current-pair-mask)) ,(current-pair-tag)) ,(current-true-ptr) ,(current-false-ptr))]
      ['vector?     `(if (= (bitwise-and ,@(map specify-v v) ,(current-vector-mask)) ,(current-vector-tag)) ,(current-true-ptr) ,(current-false-ptr))]
      ['cons
        (define tmp (fresh))
        (define val1 (specify-v (first v)))
        (define val2 (specify-v (second v)))
       `(let ((,tmp (+ (alloc ,(current-pair-size)) ,(current-pair-tag)))) (begin (mset! ,tmp -1 ,val1) (mset! ,tmp ,(- (current-word-size-bytes) 1) ,val2) ,tmp))]
      ['unsafe-car   `(mref ,@(map specify-v v) ,(car-offset))]
      ['unsafe-cdr   `(mref ,@(map specify-v v) ,(cdr-offset))]
      ['unsafe-make-vector
        (define tmp (fresh))
        (define valRes (specify-v (first v)))
        (define allocParam (if (number? valRes) 
                               (if (int61? (first v)) (+ valRes 8) (+ valRes 2))
                              `(* (+ 1 (arithmetic-shift-right ,valRes ,(current-vector-shift))) ,(current-word-size-bytes))))
       `(let ([,tmp (+ (alloc ,allocParam) ,(current-vector-tag))])
            (begin (mset! ,tmp -3 ,valRes) ,tmp))]
      ['unsafe-vector-length `(mref ,(specify-v (first v)) ,(+ -3 (current-vector-length-displacement)))]
      ['unsafe-vector-set!
        (define val1Res (specify-v (first v)))
        (define val2Res (specify-v (second v)))
        (define val3Res (specify-v (third v)))
       `(mset! ,val1Res
               ,(if (number? val2Res)
                    (if (int61? (second v)) (+ val2Res 5) (- val2Res 1))
                   `(+ (* (arithmetic-shift-right ,val2Res ,(current-vector-shift)) ,(current-word-size-bytes)) 5))
               ,val3Res)]
      ['unsafe-vector-ref
        (define val1Res (specify-v (first v)))
        (define val2Res (specify-v (second v)))
       `(mref ,val1Res
              ,(if (number? val2Res)
                    (+ val2Res 5)
                  `(+ (* (arithmetic-shift-right ,val2Res ,(current-vector-shift)) ,(current-word-size-bytes)) 5)))]
      ['unsafe-fx*
        (define vals (map specify-v v))
        (if (int61? (first vals))
            `(* ,(arithmetic-shift (first vals) (- (current-fixnum-shift))) ,(second vals))
            (if (int61? (second vals))
              `(* ,(first vals) ,(arithmetic-shift (second vals) (- (current-fixnum-shift))))
              `(* ,(first vals) (arithmetic-shift-right ,(second vals) ,(current-fixnum-shift)))))]
      [binop
        (if (or (equal? binop 'unsafe-fx+) (equal? binop 'unsafe-fx-))
           `(,(dict-ref binops binop) ,@(map specify-v v))
           `(if (,(dict-ref binops binop) ,@(map specify-v v)) ,(current-true-ptr) ,(current-false-ptr)))]))

  (specify-p p))


; Input:   exprs-bits-lang-v8
; Output:  values-bits-lang-v8
; Purpose: Performs the monadic form transformation, unnesting all non-trivial operators and operands 
;          to binops, and calls making data flow explicit and simple to implement imperatively.
;          All operands are evaluated from left to right.
(define (remove-complex-opera* p)
  
  (define (remop-p p)
    (match p
      [`(module ,defines ... ,value)
       `(module ,@(map remop-def defines) ,(remop-v value))]))

  (define (remop-def d)
    (match d
      [`(define ,label (lambda (,aloc ...) ,value))
       `(define ,label (lambda ,aloc ,(remop-v value)))]))
    
  (define (remop-pr pr)
    (match pr
      [`(not ,pred)
       `(not ,(remop-pr pred))]
      [`(if ,pred1 ,pred2 ,pred3)
       `(if ,(remop-pr pred1) ,(remop-pr pred2) ,(remop-pr pred3))]
      [`(let ([,aloc ,value] ...) ,pred)
       `(let (,@(map remop-bind (map list aloc value))) ; zip the aloc and value lists
             ,(remop-pr pred))]
      [`(begin ,effects ... ,pred)
       `(begin ,@(map remop-eff effects) ,(remop-pr pred))]
      [`(,relop ,val1 ,val2)
       (handle-vals `(,relop) `(,val1 ,val2) '())]
      [_ pr])) ; bool

  (define (remop-bind b)
    (match b
      [`(,aloc ,value)
       `(,aloc ,(remop-v value))]))
  
  ; Process a list of values, doing recursive handling for complex values and constructing
  ; a let-expression for each. Return the new instruction.
  ; This function is used for both binops and call because they only differ in the base case.
  ; base: the container for the base case. e.g. `(call ,triv) or `(+)
  ; processedLst: accumulator for the base case. Initialize as empty.
  (define (handle-vals base lst processedLst)
    (match lst
      ['()
       (append base (reverse processedLst))]
      [(cons p pRest) #:when (not (list? p)) ; head is atomic
       (handle-vals base pRest (cons p processedLst))]
      [`(,p) #:when (equal? base '(mset!))
        ; special case: third value of mset! is not atomic
        (define res (remop-v p))
        (append base (reverse (cons res processedLst)))]
      [(cons p pRest)
       (define tmp (fresh))
       (define headRes (remop-v p))
       (define tailRes (handle-vals base pRest (cons tmp processedLst)))
      `(let ([,tmp ,headRes]) ,tailRes)]))

  ; Return an instruction.
  (define (remop-v v)
    (match v
      [`(mref ,val1 ,val2)
        (handle-vals '(mref) `(,val1 ,val2) '())]
      [`(alloc ,val)
        (handle-vals '(alloc) `(,val) '())]
      [`(call ,value ,values ...)
        (handle-vals '(call) (cons value values) '())]
      [`(let ([,aloc ,value] ...) ,val) 
       `(let (,@(map remop-bind (map list aloc value))) ; zip the aloc and value lists
             ,(remop-v val))]
      [`(if ,pred ,val1 ,val2)
       `(if ,(remop-pr pred) ,(remop-v val1) ,(remop-v val2))]
      [`(begin ,effects ... ,value)
       `(begin ,@(map remop-eff effects) ,(remop-v value))]
      [`(,binop ,val1 ,val2)
        (handle-vals `(,binop) `(,val1 ,val2) '())]
      [_ v])) ; int, aloc, or label

  (define (remop-eff e)
    (match e
      [`(mset! ,val1 ,val2 ,val3)
        (handle-vals '(mset!) `(,val1 ,val2 ,val3) '())]
      [`(begin ,effects ...)
       `(begin ,@(map remop-eff effects))]))
  
  (remop-p p))


; =============== M6 Passes ================

; Input: asm-pred-lang-v7/conflicts
; Output: asm-pred-lang-v7/pre-framed
; Purpose: Compiles Asm-pred-lang-v7/conflicts to Asm-pred-lang-v7/pre-framed by pre-assigning all 
;          variables in the call-undead sets to frame variables.
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


; Input:   asm-pred-lang-v8/pre-framed
; Output:  asm-pred-lang-v8/framed
; Purpose: Compiles Asm-pred-lang-v8/pre-framed to Asm-pred-lang-v8/framed by allocating frames for 
;          each non-tail call, and assigning all new-frame variables to frame variables in the new frame.
(define (allocate-frames p) 
  
  (define bpr (current-frame-base-pointer-register))
  
  (define framesize 0)

  ; Return the integer x corresponding to the largest frame value fvx in call-undead,
  ; and if a call-undead is assigned, check its associated fvar.
  ; if call-undead is empty, return -1.
  (define (find-largest-frame call-undead assignment)
    ; dereference entries in call-undead
    (define deref-undead (map (lambda (x) (if (dict-has-key? assignment x)
                                              (first (dict-ref assignment x))
                                              x))
                              call-undead))
    (foldr (lambda (p rest) (max p rest)) 
                            -1
                            (map fvar->index (filter fvar? deref-undead))))

  ; The size of a frame n (in slots) for a given non-tail call is the maximum of:
  ;   - the number of locations in the call-undead, or
  ;   - one more than the index of the largest frame location in the call-undead set.
  (define (find-framesize! info)
    (define call-undead (info-ref info 'call-undead))
    (define assignment (info-ref info 'assignment))
    (set! framesize (* (current-word-size-bytes) 
                       (max (length call-undead)
                            (+ 1 (find-largest-frame call-undead assignment))))))
  
  ; Create a new assignment for every entry fr in new-frames. 
  ; The assignment is (fr fvi) where the 'i's in assignment form an ascending sequence.
  ; Remove the assigned variables from the locals set
  ; Return the updated info.
  (define (update-info info)
    (define locals (info-ref info 'locals))
    (define assignment (map (lambda (x) `(,(first x) . ,(second x))) (info-ref info 'assignment)))
    (define new-frames (info-ref info 'new-frames))
    ; double loop over entries of new-frames. i resets with every (frame ...). 
    (define-values (new-loc new-as)
      (for/fold ([loc locals]
                 [as assignment])
                ([frame new-frames])

                (define-values (x new-a)
                  (for/fold ([i (/ framesize (current-word-size-bytes))]
                             [currA as])
                            ([f frame])
                            (values (+ i 1) (dict-set currA f (make-fvar i)))))

                (values (foldr (lambda (f rest) (set-remove rest f)) loc frame) new-a)))

    (define new-info (info-remove (info-remove (info-remove info 'new-frames) 'call-undead) 'undead-out))
    (info-set (info-set new-info 'locals (reverse new-loc)) 'assignment (map list (dict-keys new-as) (dict-values new-as))))

  (define (allocate-p p)
    (match p
      [`(module ,info ,defines ... ,tail)
        (define defines-res (map allocate-def defines))
        (find-framesize! info)
       `(module ,(update-info info)
                ,@defines-res
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
      [`(begin ,effects ...)
       `(begin ,@(map allocate-e effects))]
      [`(if ,pred ,effect1 ,effect2)
       `(if ,(allocate-pr pred) ,(allocate-e effect1) ,(allocate-e effect2))]
      [`(return-point ,label ,tail)
       `(begin 
            (set! ,bpr (- ,bpr ,framesize))
            (return-point ,label ,(allocate-t tail))
            (set! ,bpr (+ ,bpr ,framesize)))]
      [_ e]))

  (allocate-p p))


; Input:   asm-pred-lang-v7/spilled
; Output:  asm-pred-lang-v7/assignments
; Purpose: Compiles Asm-pred-lang-v7/spilled to Asm-pred-lang-v7/assignments 
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

; Input:   proc-imp-cmf-lang-v8
; Output:  imp-cmf-lang-v8
; Purpose: Compiles Proc-imp-cmf-lang v8 to Imp-cmf-lang v8 by imposing calling conventions on all calls 
;          (both tail and non-tail calls), and entry points. The registers used to passing parameters are 
;          defined by current-parameter-registers, and the registers used for returning are defined by 
;          current-return-address-register and current-return-value-register.
(define (impose-calling-conventions p)

  (define cpr (current-parameter-registers))
  (define cfbp (current-frame-base-pointer-register))
  (define cra (current-return-address-register))
  (define crv (current-return-value-register))
  (define cprLen (length cpr))
  (define new-frames `())
  (define new-framesL `())
  
  (define (impose-p p)
    (match p
      [`(module ,defines ... ,tail)
        (define tmp (fresh 'tmp-ra))
        (let* ([i-tail `(begin (set! ,tmp ,cra) ,(impose-t tail tmp))]
               [frames new-frames])
               `(module ((new-frames ,frames)) ,@(map impose-d defines) ,i-tail))]))

  (define (impose-d d)
    (match d
      [`(define ,label (lambda (,alocs ...) ,tail))
        (define tmpL (fresh 'tmp-ra))
        (let* ([i-tail `(begin (set! ,tmpL ,cra) (begin ,@(map set-opands  (make-para-list (length alocs))  alocs) 
                              ,(impose-t tail tmpL)))]
               [framesL new-framesL])
       `(define ,label ((new-frames ,framesL)) ,i-tail))]))

  (define (impose-t t tmp)
    (match t
      [`(call ,triv ,opands ...)
       `(begin ,@(set-block opands)
               (set! ,cra ,tmp)
               ,(create-jump triv (length opands)))]
      [`(begin ,effects ... ,tail)
        `(begin ,@(map (curry impose-e tmp) effects) ,(impose-t tail tmp))]
      [`(if ,pred ,tail1 ,tail2)
        `(if ,(impose-pred pred tmp) ,(impose-t tail1 tmp) ,(impose-t tail2 tmp))]
      [value 
        `(begin
            (set! ,crv ,t)
            (jump ,tmp ,cfbp ,crv))]))

  (define (impose-pred pred tmp)
    (match pred
      [`(not ,pred)
        `(not ,(impose-pred pred tmp))]
      [`(begin ,effects ... ,pred)
        `(begin ,@(map (curry impose-e tmp) effects) ,(impose-pred pred tmp))]
      [`(if ,pred1 ,pred2 ,pred3)
        `(if ,(impose-pred pred1 tmp) ,(impose-pred pred2 tmp) ,(impose-pred pred3 tmp))]
      [_ pred]))

  (define (impose-e tmp e)
    (match e
      [`(set! ,aloc (call ,triv ,opands ...))
        (set! new-frames (append new-frames '(())))
        (define rp-label (fresh-label 'rp))
        `(begin
         (return-point ,rp-label
            (begin
              ,@(set-block opands)
              (set! ,cra ,rp-label)
              ,(create-jump triv (length opands))))
          (set! ,aloc ,crv))]
      [`(set! ,aloc ,value)
        `(set! ,aloc ,value)]
      [`(mset! ,aloc ,opand ,triv)
        e]
      [`(begin ,effects ... )
        `(begin ,@(map (curry impose-e tmp) effects))]
      [`(if ,pred ,effect1 ,effect2)
        `(if ,(impose-pred pred tmp),(impose-e tmp effect1) ,(impose-e tmp effect2))]))

  (define (make-para-list len)
    (if (> len cprLen)
        (append cpr (build-list (- len cprLen) (lambda (x) (make-fvar x))))
        (take cpr len)))

  (define (set-block opands)
    (map set-opands (reverse opands) (reverse (make-para-list (length opands)))))

  (define (set-opands o r)
    `(set! ,r ,o))

  (define (create-jump t len)
    `(jump ,t ,cfbp ,cra ,@(make-para-list len)))  

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


; Input:   nested-asm-lang-v7
; Output:  block-pred-lang-v7
; Purpose: Compile the Nested-asm-lang v7 to Block-pred-lang v7,
;          eliminating all nested expressions by generating fresh basic blocks and jumps.
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
  

; Input:   block-pred-lang-v7
; Output:  block-asm-lang-v7
; Purpose: Compile the Block-pred-lang v7 to Block-asm-lang v7 by manipulating the 
;          branches of if statements to resolve branches.
(define (resolve-predicates p)
  (define (resolve-p p) 
    (match p
      [`(module ,bs ...)
       `(module ,@(map resolve-b bs))]))

  (define (resolve-b b)
    (match b 
      [`(define ,label ,tail)
        `(define ,label ,(resolve-t tail))]))

  (define (resolve-t t)
    (match t
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


; Input:   block-asm-lang-v7
; Output:  para-asm-lang-v7
; Purpose: Compile Block-asm-lang v7 to Para-asm-lang v7 by flattening basic blocks into labeled instructions.
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
      [`(jump ,trg)
      `((jump ,trg))]
      [`(begin ,effects ...  ,tail)
       `(,@effects ,@(flatten-t tail))]
      [`(if (,relop, loc ,opand) (jump ,trg1) (jump ,trg2))
       `((compare ,loc ,opand)
         (jump-if ,relop ,trg1)
         (jump ,trg2))]))

  (flatten-p p))


; =============== M3 Passes ================

; Input:   asm-pred-lang-v8/locals
; Output:  asm-pred-lang-v8/undead
; Purpose: Performs undead analysis, compiling Asm-pred-lang v8/locals to Asm-pred-lang v8/undead 
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

        (values ust undead-o)]
      [`(,relop ,loc ,opand)
        (define add-loc (set-add undead-out loc))
        (if (number? opand)
            (values undead-out add-loc)
            (values undead-out (set-add add-loc opand)))] ; reg, fvar, aloc
      [`(,bool)
        (values undead-out undead-out)]))
  
  ; Calculate the undead input for a single effect e,
  ; given the output, undead-out.
  ; Return: (values undead-set-tree? undead-set?)
  ; i.e. (<tree for current effect> <set for next effect>)
  (define (undead-effect undead-out e)
    (match e
      [`(set! ,loc1 (mref ,loc2 ,opand))
        (let ([newSet (if (number? opand)
                          (set-add (set-remove undead-out loc1) loc2)
                          (set-add (set-remove (set-add undead-out opand) loc1) loc2))])
          (values undead-out newSet))]
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
      [`(mset! ,loc ,index ,triv)
        (define set1 (set-add undead-out loc))
        (define set2 (if (number? index)
                         set1
                         (set-add set1 index)))
        (define newSet (if (or (number? triv) (label? triv))
                         set2
                         (set-add set2 triv)))
        (values undead-out newSet)]
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


; Input:   asm-pred-lang-v8/undead
; Output:  asm-pred-lang-v8/conflicts
; Purpose: Performs conflict analysis, compiling Asm-pred-lang v8/undead to 
;          Asm-pred-lang v8/conflicts by decorating programs with their conflict graph.
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
  ; graph  : a graph of the conflicts found so far.
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
  ; graph  : a graph of the conflicts found so far.
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
  ; graph  : a graph of the conflicts found so far.
  ; Return a graph.
  (define (c-analysis-e undead graph e)
    (match e
      [`(set! ,loc1 (mref ,loc2 ,index))
        (update-conflicts `(,loc1) undead graph)]
      [`(set! ,loc1 ,loc2) #:when (or (aloc? loc2) (register? loc2) (fvar? loc2))
        (update-conflicts `(,loc1 ,loc2) undead graph)]
      [`(set! ,loc ,other) ; other is binop, number, or label
        (update-conflicts `(,loc) undead graph)]
      [`(mset! ,loc ,index ,triv)
        graph]
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

 
; Input:    asm-pred-lang-v7/framed
; Output:   asm-pred-lang-v7/spilled
; Purpose:  Performs graph-colouring register allocation, 
;           compiling Asm-pred-lang v7/framed to Asm-pred-lang v7/spilled by decorating programs with their register assignments.
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


; Input:   exprs-lang-v7
; Output:  exprs-unique-lang-v7
; Purpose: Compiles Values-lang v6 to Values-unique-lang v6 by resolving top-level lexical identifiers 
;          into unique labels, and all other lexical identifiers into unique abstract locations.
(define (uniquify p) 

  (define label-binds-box (box '()))

  ; destructures the module statement and calls the appropriate uniquify recursive functions
  (define (uniquify-p p dict-acc)
    (match p
      [`(module ,def ... ,v)
        (define uniquified-labels (map (lambda (d) (uniquify-define-labels d dict-acc)) def))
       `(module ,@(map (lambda (d) (uniquify-define d dict-acc)) uniquified-labels)
                ,(uniquify-value v dict-acc))]))

  ; generate unique labels for the define statements blocks
  (define (uniquify-define-labels def def-binds)
    (match def
      [`(define ,x (lambda (,xs ...) ,value))
        (define label-binds (unbox label-binds-box))
        (if (dict-has-key? label-binds x)
            (set-box! label-binds-box label-binds)
            (set-box! label-binds-box (dict-set label-binds x (fresh-label x))))
       `(define ,(dict-ref (unbox label-binds-box) x) (lambda (,@xs) ,value))]))
  
  ; generate unique alocs for the variables in the define statement bodies
  (define (uniquify-define def def-binds)
    (match def
      [`(define ,uniquified-label (lambda (,xs ...) ,t))
        (define new-def-binds (construct-binds xs def-binds))
       `(define ,uniquified-label
                (lambda ,(map (lambda (x) (try-lookup x new-def-binds)) xs) 
                        ,(uniquify-value t new-def-binds)))]))

  ; recursively uniquify a single value
  (define (uniquify-value t binds)
    (match t 
      [`(let ([,xs ,vs] ...) ,value)
        (define new-binds (construct-binds xs binds)) 
        `(let ,(for/list ([x xs][v vs])
                        `[,(try-lookup x new-binds) ,(uniquify-value v binds)])     
              ,(uniquify-value value new-binds))]          
      
      [`(if ,v0 ,v1 ,v2)
       `(if ,(uniquify-value v0 binds)
            ,(uniquify-value v1 binds) 
            ,(uniquify-value v2 binds))]
      
      [`(call ,vs ...)
       `(call ,@(map (lambda (v) (uniquify-value v binds)) vs))]
      
      [triv (update-bind triv binds)]))

  ; return the triv itself if it is an int64, otherwise look up the bind in the dictionary
  (define (update-bind x binds)
    (match x
      [(? prim-f?) x]
      [(? name?) (try-lookup x binds)]
      [_ x]))

  ; uses fresh to give a unique assignment to each name at the current scope
  (define (construct-binds xs binds)
    (for/fold ([new-binds binds])
              ([x xs])           
      (dict-set new-binds x (fresh x))))    

  ; tries to lookup the associated key in a binding dictionary, prioritizng label binds first 
  (define (try-lookup x binds)
    (if (dict-has-key? (unbox label-binds-box) x)
        (dict-ref (unbox label-binds-box) x)
        (if (dict-has-key? binds x)
            (dict-ref binds x)
             x)))

  ;  return true if prim-f according to M8, false otherwise                            
  (define (prim-f? b)
    (or (equal? b '*)
        (equal? b '+)
        (equal? b '-)
        (equal? b '<)
        (equal? b '<=)
        (equal? b '>)
        (equal? b '>=)
        (equal? b 'eq?)
        (equal? b 'fixnum?)
        (equal? b 'boolean?)
        (equal? b 'empty?)
        (equal? b 'void?)
        (equal? b 'ascii-char?)
        (equal? b 'error?)
        (equal? b 'not)
        (equal? b 'pair?)
        (equal? b 'vector?)
        (equal? b 'cons)
        (equal? b 'car)
        (equal? b 'cdr)
        (equal? b 'make-vector)
        (equal? b 'vector-length)
        (equal? b 'vector-set!)
        (equal? b 'vector-ref))) 

  (uniquify-p p '()))


; Input:   values-bits-lang-v8
; Output:  imp-mf-lang-v8
; Purpose: Picks a particular order to implement let expressions using set!.
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
      [`(begin ,effects ... ,tail)
       `(begin ,@(map seq-eff effects) ,(seq-t tail))]
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
      [`(begin ,effects ... ,pred)
       `(begin ,@(map seq-eff effects) ,(seq-pr pred))]
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
               ,(seq-v val))]
      [`(if ,pred ,val1 ,val2)
       `(if ,(seq-pr pred) ,(seq-v val1) ,(seq-v val2))]
      [`(call ,triv ,opand ...)
       v]
      [`(begin ,effects ... ,value)
       `(begin ,@(map seq-eff effects) ,(seq-v value))]
      [_ v])) ; triv, binop, mref, or alloc

  (define (seq-eff e)
    (match e
      [`(mset! ,aloc ,opand ,value) 
       `(mset! ,aloc ,opand ,(seq-v value))]
      [`(let ([,aloc ,value] ...) ,effect) 
       `(begin ,@(map seq-bind (map list aloc value)) ; zip the aloc and value lists
               ,(seq-eff effect))]
      [`(begin ,effects ...)
       `(begin ,@(map seq-eff effects))]))

  (seq-p p))


; Input:   imp-mf-lang-v8
; Output:  proc-imp-cmf-lang-v8
; Purpose: Pushes set! and mset! under begin and if so that the right-hand-side 
;          of each is simple value-producing operand.
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
      [_ v])) ; triv, binop, mref, or alloc

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
      [`(set! ,aloc ,value) 
        e]

      [`(mset! ,aloc ,opand (begin ,eff ... ,val)) ; normalize so that begin is above set.
        `(begin ,@(map n-bind-e eff) ,(n-bind-e `(mset! ,aloc ,opand ,(n-bind-v val))))]
      [`(mset! ,aloc ,opand (if ,pred ,val1 ,val2)) ; normalize so that if is above set.
        `(if ,(n-bind-pr pred) ,(n-bind-e `(mset! ,aloc ,opand ,(n-bind-v val1))) ,(n-bind-e `(mset! ,aloc ,opand ,(n-bind-v val2))))]
      [`(mset! ,aloc ,opand ,value) #:when (list? value)
        (define tmp (fresh))
       `(begin (set! ,tmp ,(n-bind-v value)) (mset! ,aloc ,opand ,tmp))]
      [`(mset! ,aloc ,opand ,value)
        e]

      [`(if ,pred ,eff1 ,eff2)
       `(if ,(n-bind-pr pred) ,(n-bind-e eff1) ,(n-bind-e eff2))]
      [`(begin ,eff ...)
        `(begin ,@(map n-bind-e eff))]))

  (n-bind-p p))


; Input:   imp-cmf-lang-v8
; Output:  asm-alloc-lang-v8
; Purpose: Selects appropriate sequences of abstract assembly instructions to 
;          implement the operations of the source language.
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
      [`(mref ,loc ,opand)
       `((set! ,aloc (mref ,loc ,opand)))]
      [`(alloc ,opand)
       `((set! ,aloc (alloc ,opand)))]
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
           `((return-point ,label (begin ,@(sel-ins-t tail)))))]
      [`(mset! ,loc ,opand ,triv)
       `((mset! ,loc ,opand ,triv))]))

  (sel-ins-p p))


; Input:   asm-pred-lang-v8/assignments
; Output:  nested-asm-lang-fvars-v8
; Purpose: Replaces all abstract location with physical locations using the assignment described in the 
;          assignment info field, and dropping any register-allocation-related metadata from the program.
(define (replace-locations p)

  (define (replace-loc-p p)
    (match p
      [`(module ,info ,defines ... ,tail)
       `(module ,@(map replace-loc-def defines) ,(replace-loc-t tail (info-ref info 'assignment)))]))

  (define (replace-loc-def d)
    (match d
      [`(define ,label ,info ,tail)
       `(define ,label ,(replace-loc-t tail (info-ref info 'assignment)))]))
  
  ; given an abstract location 'aloc' return its replacement as defined
  ; in the list of assignments 'as'.
  (define (get-repl aloc as)
    (info-ref as aloc))
  
  ; return a tail with locations replaced.
  (define (replace-loc-t t as)
    (match t
      [`(begin ,effects ... ,tail)
        `(begin ,@(map (curry replace-loc-e as) effects) ,(replace-loc-t tail as))]
      [`(if ,pred ,tail1 ,tail2)
        `(if ,(replace-loc-pred as pred) ,(replace-loc-t tail1 as) ,(replace-loc-t tail2 as))]
      [`(jump ,trg ,loc ...)
        `(jump ,(replace-loc trg as))]))

  (define (replace-loc-e as e)
    (match e
      [`(set! ,loc_1 (mref ,loc_2 ,index))
       `(set! ,(replace-loc loc_1 as) (mref ,(replace-loc loc_2 as) ,(replace-loc index as)))]
      [`(set! ,loc_1 (,binop ,loc_1 ,opand))
        `(set! ,(replace-loc loc_1 as) (,binop ,(replace-loc loc_1 as) ,(replace-loc opand as)))]
      [`(set! ,loc ,triv)
        `(set! ,(replace-loc loc as) ,(replace-loc triv as))]
      [`(mset! ,loc ,index ,triv)
       `(mset! ,(replace-loc loc as) ,(replace-loc index as) ,(replace-loc triv as))]
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
      [_ p])) ; boolean

  (define (replace-loc loc as)
    (if (aloc? loc) (get-repl loc as) loc))

  (replace-loc-p p))


; Input:   asm-pred-lang-v8
; Output:  asm-pred-lang-v8/locals
; Purpose: Compiles Asm-pred-lang v8 to Asm-pred-lang v8/locals, analysing which abstract locations 
;          are used in each block, and decorating each block and the module with the set of variables 
;          in an info? field.
(define (uncover-locals p)

  (define (uloc-p p)
    (match p
      [`(module ,info ,defines ... ,tail)
        (define tailRes (set->list (list->set (uloc-t tail))))
       `(module ,(info-set info 'locals tailRes) ,@(map uloc-def defines) ,tail)]))

  (define (uloc-def def)
    (match def
      [`(define ,label ,info ,tail)
        (define tailRes (set->list (list->set (uloc-t tail))))
       `(define ,label ,(info-set info 'locals tailRes) ,tail)]))
  
  ; return a list (not set) of alocs.
  (define (uloc-t t)
    (match t
      [`(begin ,effects ... ,tail)
        (define effRes (foldl append '() (map uloc-e effects)))
        (append effRes (uloc-t tail))]
      [`(if ,pred ,tail1 ,tail2)
        (append 
          (uloc-pred pred)
          (uloc-t tail1)
          (uloc-t tail2))]
      [`(jump ,trg ,loc ...) ; alocs in jump params are not added to locals set.
        (if (aloc? trg) 
           `(,trg) 
           '())]))
  
  ; return: list of all alocs found in effect
  (define (uloc-e e)
    (match e
      [`(set! ,loc_1 (mref ,loc_2 ,index))
        (find-alocs `(,loc_1 ,loc_2 ,index))]
      [`(set! ,loc_1 (,binop ,loc_1 ,opand))
        (find-alocs `(,loc_1 ,opand))]
      [`(set! ,loc ,triv)
        (find-alocs `(,loc ,triv))]
      [`(mset! ,loc ,index ,triv)
        (find-alocs `(,loc ,index ,triv))]
      [`(if ,pred ,effect1 ,effect2)
        (append
          (uloc-pred pred)
          (uloc-e effect1)
          (uloc-e effect2))]
      [`(begin ,effects ...)
        (foldl append '() (map uloc-e effects))]
      [`(return-point ,label ,tail)
        (uloc-t tail)]))

  (define (uloc-pred p)
    (match p
      [`(begin ,effects ... ,pred)
        (append
          (foldl append '() (map uloc-e effects))
          (uloc-pred pred))]
      [`(if ,pred1 ,pred2 ,pred3)
        (append
          (uloc-pred pred1)
          (uloc-pred pred2)
          (uloc-pred pred3))]
      [`(not ,pred)
        (uloc-pred pred)]
      [`(,relop ,loc ,opand)
        (find-alocs `(,loc ,opand))]
      [_ '()])) ;bool

  ; Given a list, return all the alocs in that list.
  (define (find-alocs lst)
     (filter aloc? lst))

  (uloc-p p))


; Input:   para-asm-lang-v8
; Output:  paren-x64-mops-v8
; Purpose: Compile the Para-asm-lang v7 to Paren-x64 v7 by patching instructions that have 
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
      [`(set! ,loc_1 (mref ,loc_2 ,index))
        (cond [(and (address? loc_1) (address? loc_2) (or (and (not (int32? index)) (int64? index)) (address? index)))
                `((set! ,i-reg1 ,loc_2)
                  (set! ,i-reg2 ,index)
                  (set! ,i-reg1 (mref ,i-reg1 ,i-reg2))
                  (set! ,loc_1 ,i-reg1))]
              [(and (address? loc_1) (address? loc_2))
                `((set! ,i-reg1 ,loc_2)
                  (set! ,i-reg1 (mref ,i-reg1 ,index))
                  (set! ,loc_1 ,i-reg1))]
              [(and (address? loc_1) (or (and (not (int32? index)) (int64? index)) (address? index)))
                `((set! ,i-reg1 ,index)
                  (set! ,i-reg1 (mref ,loc_2 ,i-reg1))
                  (set! ,loc_1 ,i-reg1))]
              [(address? loc_1)
                `((set! ,i-reg1 (mref ,loc_2 ,index))
                  (set! ,loc_1 ,i-reg1))]
              [(and (address? loc_2) (or (and (not (int32? index)) (int64? index)) (address? index)))
                `((set! ,i-reg1 ,loc_2)
                  (set! ,i-reg2 ,index)
                  (set! ,loc_1 (mref ,i-reg1 ,i-reg2)))]
              [(address? loc_2)
                `((set! ,i-reg1 ,loc_2)
                  (set! ,loc_1 (mref ,i-reg1 ,index)))]
              [(or (and (not (int32? index)) (int64? index)) (address? index))
                `((set! ,i-reg1 ,index)
                  (set! ,loc_1 (mref ,loc_2 ,i-reg1)))]
              [else e])]
      [`(mset! ,loc_1 ,index ,triv)
        (cond [(and (address? loc_1) (or (and (not (int32? index)) (int64? index)) (address? index)) (or (and (not (int32? triv)) (int64? triv)) (address? triv) (label? triv)))
                `((set! ,i-reg1 ,loc_1)
                  (set! ,i-reg2 ,index)
                  (set! ,i-reg1 (+ ,i-reg1 ,i-reg2))
                  (set! ,i-reg2 ,triv)
                  (mset! ,i-reg1 0 ,i-reg2))]
              [(and (address? loc_1) (or (and (not (int32? index)) (int64? index)) (address? index)))
                `((set! ,i-reg1 ,loc_1)
                  (set! ,i-reg2 ,index)
                  (mset! ,i-reg1 ,i-reg2 ,triv))]
              [(and (address? loc_1) (or (and (not (int32? triv)) (int64? triv)) (address? triv) (label? triv)))
                `((set! ,i-reg1 ,triv)
                  (set! ,i-reg2 ,loc_1)
                  (mset! ,i-reg2 ,index ,i-reg1))]
              [(and (or (and (not (int32? index)) (int64? index)) (address? index)) (or (and (not (int32? triv)) (int64? triv)) (address? triv) (label? triv)))
                `((set! ,i-reg1 ,triv)
                  (set! ,i-reg2 ,index)
                  (mset! ,loc_1 ,i-reg2 ,i-reg1))]
              [(or (and (not (int32? index)) (int64? index)) (address? index))
                `((set! ,i-reg1 ,index)
                  (mset! ,loc_1 ,i-reg1 ,triv))]
              [(or (and (not (int32? triv)) (int64? triv)) (address? triv) (label? triv))
                `((set! ,i-reg1 ,triv)
                  (mset! ,loc_1 ,index ,i-reg1))]
              [(address? loc_1)
                `((set! ,i-reg1 ,loc_1)
                  (mset! ,i-reg1 ,index ,triv))]
              [else e])]
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
      [`(compare ,addr1 ,reg) #:when (address? addr1)
          `((set! ,i-reg1 ,addr1)
            (compare ,i-reg1 ,reg ))]
      [`(compare ,addr1 ,int64) #:when (and (address? addr1) (int64? int64))
          `((set! ,i-reg1 ,addr1)
            (compare ,i-reg1 ,int64))]
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


; Input:   nested-asm-lang-fvars-v8
; Output:  nested-asm-lang-v8
; Purpose: Compile nested-asm-lang-fvars-v8 to nested-asm-lang-v8 by reifying fvars into displacement mode operands.
(define (implement-fvars p)
  
  (define (fvars-p p)
    (match p
      [`(module ,defines ... ,tail)
       `(module ,@(map fvars-defines defines) ,(fvars-t tail 0))]))
  
  (define (fvars-defines d)
    (match d
      [`(define ,label ,tail)
       `(define ,label ,(fvars-t tail 0))]))

  ; Convert an fvar into a statement of the form (currentfbp - offset)
  ; Does nothing if input is not an fvar.
  ; offset: the current offset to fbp
  (define (fvar->addr fvar offset)
    (if (fvar? fvar)
        `(,fbp 
            - 
          ,(+ (* (fvar->index fvar) (current-word-size-bytes)) offset))
        fvar))

  ; Return an instruction
  (define (fvars-t t offset)
    (match t
      [`(jump ,trg)
        `(jump ,(fvar->addr trg offset))]
      [`(begin ,effects ... ,tail)
        (define-values (effRes e-offset)
          (for/fold ([effRes '()] ; list of processed effects
                     [currOffset offset])
                    ([e effects])
                    (define-values (e-ins e-off)
                                   (fvars-e e currOffset))
                    (values (append effRes `(,e-ins)) e-off)))

        (define tailRes (fvars-t tail e-offset))

       `(begin ,@effRes ,tailRes)]
      [`(if ,pred ,tail1 ,tail2)
        (define tailRes1 (fvars-t tail1 offset))
        (define tailRes2 (fvars-t tail2 offset))
       `(if ,(fvars-pred pred offset) ,tailRes1 ,tailRes2)]))

  ; Return an instruction
  (define (fvars-pred pred offset)
    (match pred
      [`(not ,pred)
       `(not ,(fvars-pred pred offset))]
      [`(begin ,effects ... ,pred)
        (define-values (effRes e-offset)
          (for/fold ([effRes '()] ; list of processed effects
                     [currOffset offset])
                    ([e effects])
                    (define-values (e-ins e-off)
                                   (fvars-e e currOffset))
                    (values (append effRes `(,e-ins)) e-off)))

        (define predRes (fvars-pred pred e-offset))

       `(begin ,@effRes ,predRes)]
      [`(if ,pred1 ,pred2 ,pred3)
       `(if ,(fvars-pred pred1 offset) ,(fvars-pred pred2 offset) ,(fvars-pred pred3 offset))]
      [`(,relop ,loc ,opand)
       `(,relop ,(fvar->addr loc offset) ,(fvar->addr opand offset))]
      [_ pred])) ; bool

  ; Return (values new-ins new-fbp-offset)
  (define (fvars-e e offset)
    (match e
      [`(set! ,loc_1 (mref ,loc_2 ,ind))
        (values
          `(set! ,(fvar->addr loc_1 offset) (mref ,(fvar->addr loc_2 offset) ,(fvar->addr ind offset)))
          offset)]
      [`(set! ,loc_1 (,binop ,loc_1 ,opand)) #:when (and (equal? loc_1 fbp) (number? opand))
        (values e ((binop->op binop) offset opand))] ; update offset
      [`(set! ,loc_1 (,binop ,loc_1 ,opand))
        (values
          `(set! ,(fvar->addr loc_1 offset) (,binop ,(fvar->addr loc_1 offset) ,(fvar->addr opand offset)))
          offset)]
      [`(set! ,loc ,triv)  ; Don't change offset even if loc is fbp
        (values `(set! ,(fvar->addr loc offset) ,(fvar->addr triv offset)) offset)]
      [`(mset! ,loc ,index ,triv)
        (values 
          `(mset! ,(fvar->addr loc offset) ,(fvar->addr index offset) ,(fvar->addr triv offset))
          offset)]
      [`(begin ,effects ... )
        (define-values (effRes e-offset)
          (for/fold ([effRes '()] ; list of processed effects
                     [currOffset offset])
                    ([e effects])
                    (define-values (e-ins e-off)
                                   (fvars-e e currOffset))
                    (values (append effRes `(,e-ins)) e-off)))

        (values `(begin ,@effRes) e-offset)]
      [`(if ,pred ,effect1 ,effect2)
        (define-values (e1Res e1-off) (fvars-e effect1 offset))
        (define-values (e2Res e2-off) (fvars-e effect2 offset)) ; the resulting e1-off and e2-off should be the equal
        (values `(if ,(fvars-pred pred offset) ,e1Res ,e2Res) e1-off)]
      [`(return-point ,label ,tail)
        (values `(return-point ,label ,(fvars-t tail offset)) offset)]))
  
  (define (binop->op binop)
    (match binop
      ['* *]
      ['+ +]
      ['- -]))
       
  (fvars-p p))


; Input:   paren-x64-v7
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

  (define (opand? opand)
    (or (int64? opand) (register? opand)))

  (define (binop? b)
    (or (equal? b '*)
        (equal? b '+)
        (equal? b '-)
        (equal? b 'bitwise-and)
        (equal? b 'bitwise-ior)
        (equal? b 'bitwise-xor)
        (equal? b 'arithmetic-shift-right)))

  (define (loc->ins loc)
    (if (register? loc)
        (symbol->string loc)
        (addr->ins loc)))

  (define (trg->ins trg)
    (if (register? trg)
        (symbol->string trg)
        (string-append "[rel " (sanitize-label trg) "]")))

  (define (addr->ins addr)
    (cond [(and (frame-base-pointer-register? (first addr))
                (equal? '- (second addr))
                (dispoffset? (third addr)))
           (string-append "QWORD [" 
              (symbol->string(first addr)) 
              " - " 
              (number->string(third addr))
              "]")]

          [(and (register? (first addr))
                (equal? '+ (second addr))
                (int32? (third addr)))
           (string-append "QWORD [" 
              (symbol->string(first addr)) 
              " + " 
              (number->string(third addr))
              "]")]

          [(and (register? (first addr))
                (equal? '+ (second addr))
                (register? (third addr)))
           (string-append "QWORD [" 
              (symbol->string(first addr)) 
              " + " 
              (symbol->string(third addr))
              "]")]))

  (define (math->ins binop reg target)
      (string-append (binop->ins binop) " " (symbol->string reg) ", " (target->ins target)))

  (define (binop->ins binop)
      (cond [(equal? '* binop)                      "imul"]
            [(equal? '+ binop)                      "add"]
            [(equal? '- binop)                      "sub"]
            [(equal? 'bitwise-and binop)            "and"]
            [(equal? 'bitwise-ior binop)            "or"]
            [(equal? 'bitwise-xor binop)            "xor"]
            [(equal? 'arithmetic-shift-right binop) "sar"]))

  (define (target->ins target)
    (cond [(int32? target)
           (number->string target)]
          [(register? target)
           (symbol->string target)]
          [(address? target)
           (addr->ins target)]))
  
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
   cpsc411/langs/v8
   cpsc411/test-suite/public/v8)

  ;; You can modify this pass list, e.g., by adding other
  ;; optimization, debugging, or validation passes.
  ;; Doing this may provide additional debugging info when running the rest
  ;; suite.
  (define pass-map
    (list
     #;(cons check-exprs-lang #f)
     (cons uniquify interp-exprs-lang-v8)
     (cons implement-safe-primops interp-exprs-unique-lang-v8)
     (cons specify-representation interp-exprs-unsafe-data-lang-v8)
     (cons remove-complex-opera* interp-exprs-bits-lang-v8)
     (cons sequentialize-let interp-values-bits-lang-v8)
     (cons normalize-bind interp-imp-mf-lang-v8)
     (cons impose-calling-conventions interp-proc-imp-cmf-lang-v8)
     (cons select-instructions interp-imp-cmf-lang-v8)
     (cons expose-allocation-pointer interp-asm-alloc-lang-v8)
     (cons uncover-locals interp-asm-pred-lang-v8)
     (cons undead-analysis interp-asm-pred-lang-v8/locals)
     (cons conflict-analysis interp-asm-pred-lang-v8/undead)
     (cons assign-call-undead-variables interp-asm-pred-lang-v8/conflicts)
     (cons allocate-frames interp-asm-pred-lang-v8/pre-framed)
     (cons assign-registers interp-asm-pred-lang-v8/framed)
     (cons assign-frame-variables interp-asm-pred-lang-v8/spilled)
     (cons replace-locations interp-asm-pred-lang-v8/assignments)
     (cons optimize-predicates interp-nested-asm-lang-fvars-v8)
     (cons implement-fvars interp-nested-asm-lang-fvars-v8)
     (cons expose-basic-blocks interp-nested-asm-lang-v8)
     (cons resolve-predicates interp-block-pred-lang-v8)
     (cons flatten-program interp-block-asm-lang-v8)
     (cons patch-instructions interp-para-asm-lang-v8)
     (cons implement-mops interp-paren-x64-mops-v8)
     (cons generate-x64 interp-paren-x64-v8)
     (cons wrap-x64-boilerplate #f)
     (cons wrap-x64-run-time #f)))

  (current-pass-list
   (map car pass-map))

  (run-tests
   (v8-public-test-suite
    (current-pass-list)
    (map cdr pass-map))))
