#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt")

(test-case "assign-frame-variables 1 - empty locals set" 
    (check-equal? 
        (assign-frame-variables '(module
            ((locals ())
            (conflicts
                ((tmp-ra.14 (r15 fv0 fv1 rbp tmp-ra.15))
                (tmp-ra.15 (rax tmp-ra.14))
                (tmp-ra.16 (fv0 r15 rax rcx))
                (tmp-ra.17 (fv0 fv1 r15 rax rcx))))
            (assignment ((tmp-ra.14 rcx) (tmp-ra.15 r15))))
            (begin
                (set! tmp-ra.14 r15)
                (set! fv1 2)
                (set! fv0 1)
                (set! r15 tmp-ra.14)
                (jump L.swap.1 rbp r15 fv0 fv1))))

            '(module
                ((locals ())
                (conflicts
                    ((tmp-ra.14 (r15 fv0 fv1 rbp tmp-ra.15))
                    (tmp-ra.15 (rax tmp-ra.14))
                    (tmp-ra.16 (fv0 r15 rax rcx))
                    (tmp-ra.17 (fv0 fv1 r15 rax rcx))))
                (assignment ((tmp-ra.14 rcx) (tmp-ra.15 r15))))
                (begin
                    (set! tmp-ra.14 r15)
                    (set! fv1 2)
                    (set! fv0 1)
                    (set! r15 tmp-ra.14)
                    (jump L.swap.1 rbp r15 fv0 fv1)))))

(test-case "assign-frame-variables 2 - two leftover local assignments" 
    (check-equal? 
        (assign-frame-variables '(module
            ((locals (tmp-ra.16 tmp-ra.17))
            (conflicts
                ((tmp-ra.14 (r15 fv0 fv1 rbp tmp-ra.15))
                (tmp-ra.15 (rax tmp-ra.14))
                (tmp-ra.16 (fv0 r15 rax rcx))
                (tmp-ra.17 (fv0 fv1 r15 rax rcx))))
            (assignment ((tmp-ra.14 rcx) (tmp-ra.15 r15))))
            (begin
                (set! tmp-ra.14 r15)
                (set! fv1 2)
                (set! fv0 1)
                (set! r15 tmp-ra.14)
                (jump L.swap.1 rbp r15 fv0 fv1))))

            '(module
                ((locals (tmp-ra.16 tmp-ra.17)) ; <-- remaining locals
                (conflicts
                    ((tmp-ra.14 (r15 fv0 fv1 rbp tmp-ra.15))
                    (tmp-ra.15 (rax tmp-ra.14))
                    (tmp-ra.16 (fv0 r15 rax rcx))
                    (tmp-ra.17 (fv0 fv1 r15 rax rcx))))
                (assignment ; <-- remaining locals get assigned non-conflicting fvars
                    ((tmp-ra.14 rcx) (tmp-ra.15 r15) (tmp-ra.17 fv2) (tmp-ra.16 fv1))))
                (begin
                    (set! tmp-ra.14 r15)
                    (set! fv1 2)
                    (set! fv0 1)
                    (set! r15 tmp-ra.14)
                    (jump L.swap.1 rbp r15 fv0 fv1)))))

(test-case "assign-frame-variables 3 - two leftover local assignments, direct conflict " 
    (check-equal? 
        (assign-frame-variables '(module
                    ((locals (tmp-ra.16 tmp-ra.17))
                    (conflicts
                        ((tmp-ra.14 (r15 fv0 fv1 rbp tmp-ra.15))
                        (tmp-ra.15 (rax tmp-ra.14))
                        (tmp-ra.16 (fv0 tmp-ra.17 r15 rax rcx))
                        (tmp-ra.17 (fv0 tmp-ra.16 r15 rax rcx))))
                    (assignment ((tmp-ra.14 rcx) (tmp-ra.15 r15))))
                    (begin
                        (set! tmp-ra.14 r15)
                        (set! fv1 2)
                        (set! fv0 1)
                        (set! r15 tmp-ra.14)
                        (jump L.swap.1 rbp r15 fv0 fv1))))

            '(module
                ((locals (tmp-ra.16 tmp-ra.17))
                (conflicts
                    ((tmp-ra.14 (r15 fv0 fv1 rbp tmp-ra.15))
                    (tmp-ra.15 (rax tmp-ra.14))
                    (tmp-ra.16 (fv0 tmp-ra.17 r15 rax rcx))
                    (tmp-ra.17 (fv0 tmp-ra.16 r15 rax rcx))))
                (assignment
                    ((tmp-ra.14 rcx) (tmp-ra.15 r15) (tmp-ra.17 fv1) (tmp-ra.16 fv2))))
                (begin
                    (set! tmp-ra.14 r15)
                    (set! fv1 2)
                    (set! fv0 1)
                    (set! r15 tmp-ra.14)
                    (jump L.swap.1 rbp r15 fv0 fv1)))))

; M7 Tests

(test-case "assign-frame-variables 4 - extend binops" 
    (check-equal? 
        (assign-frame-variables 
            '(module
                ((locals (tmp-ra.16 tmp-ra.17))
                (conflicts
                    ((tmp-ra.14 (r15 fv0 fv1 rbp tmp-ra.15))
                    (tmp-ra.15 (rax tmp-ra.14))
                    (tmp-ra.16 (fv0 r15 rax rcx))
                    (tmp-ra.17 (fv0 fv1 r15 rax rcx))))
                (assignment ((tmp-ra.14 rcx) (tmp-ra.15 r15))))
                (begin
                    (set! tmp-ra.14 r15)
                    (set! fv1 (arithmetic-shift-right fv1 1))
                    (set! fv0 1)
                    (set! r15 tmp-ra.14)
                    (jump L.swap.1 rbp r15 fv0 fv1))))

            '(module
                ((locals (tmp-ra.16 tmp-ra.17))
                (conflicts
                    ((tmp-ra.14 (r15 fv0 fv1 rbp tmp-ra.15))
                    (tmp-ra.15 (rax tmp-ra.14))
                    (tmp-ra.16 (fv0 r15 rax rcx))
                    (tmp-ra.17 (fv0 fv1 r15 rax rcx))))
                (assignment
                    ((tmp-ra.14 rcx) (tmp-ra.15 r15) (tmp-ra.17 fv2) (tmp-ra.16 fv1))))
                (begin
                    (set! tmp-ra.14 r15)
                    (set! fv1 (arithmetic-shift-right fv1 1))
                    (set! fv0 1)
                    (set! r15 tmp-ra.14)
                    (jump L.swap.1 rbp r15 fv0 fv1)))))