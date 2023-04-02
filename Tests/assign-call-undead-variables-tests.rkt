#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt"
)

(test-case "assign-call-undead-variables 1 - empty call-undead set, trivial case" 
    (check-equal? 
        (assign-call-undead-variables
    '(module
        ((new-frames ())
        (locals (tmp-ra.2))
        (call-undead ())
        (undead-out
            ((tmp-ra.2 rbp)
            (tmp-ra.2 rsi rbp)
            (tmp-ra.2 rsi rdi rbp)
            (rsi rdi r15 rbp)
            (rsi rdi r15 rbp)))
        (conflicts
            ((tmp-ra.2 (rdi rsi rbp))
            (rbp (r15 rdi rsi tmp-ra.2))
            (rsi (r15 rdi rbp tmp-ra.2))
            (rdi (r15 rbp rsi tmp-ra.2))
            (r15 (rbp rdi rsi)))))
        (begin
            (set! tmp-ra.2 r15)
            (set! rsi 2)
            (set! rdi 1)
            (set! r15 tmp-ra.2)
            (jump L.swap.1 rbp r15 rdi rsi))))

    '(module
        ((new-frames ())
        (locals (tmp-ra.2))
        (call-undead ())
        (undead-out
            ((tmp-ra.2 rbp)
            (tmp-ra.2 rsi rbp)
            (tmp-ra.2 rsi rdi rbp)
            (rsi rdi r15 rbp)
            (rsi rdi r15 rbp)))
        (conflicts
            ((tmp-ra.2 (rdi rsi rbp))
            (rbp (r15 rdi rsi tmp-ra.2))
            (rsi (r15 rdi rbp tmp-ra.2))
            (rdi (r15 rbp rsi tmp-ra.2))
            (r15 (rbp rdi rsi))))
        (assignment ())) ; <-- simply adds an empty assignment to the info block
        (begin
            (set! tmp-ra.2 r15)
            (set! rsi 2)
            (set! rdi 1)
            (set! r15 tmp-ra.2)
            (jump L.swap.1 rbp r15 rdi rsi)))))

(test-case "assign-call-undead-variables 2 - assign single variable" 
    (check-equal? 
        (assign-call-undead-variables 
        '(module
            ((new-frames ())
            (locals (tmp-ra.2))
            (call-undead (x.1)) 
            (undead-out
                ((tmp-ra.2 rbp)
                (tmp-ra.2 rsi rbp)
                (tmp-ra.2 rsi rdi rbp)
                (rsi rdi r15 rbp)
                (rsi rdi r15 rbp)))
            (conflicts
                ((tmp-ra.2 (rdi rsi rbp))
                (rbp (r15 rdi rsi tmp-ra.2))
                (rsi (r15 rdi rbp tmp-ra.2))
                (rdi (r15 rbp rsi tmp-ra.2))
                (r15 (rbp rdi rsi)))))
            (begin
                (set! tmp-ra.2 r15)
                (set! rsi 2)
                (set! rdi 1)
                (set! r15 tmp-ra.2)
                (jump L.swap.1 rbp r15 rdi rsi))))

            '(module
                ((new-frames ())
                (locals (tmp-ra.2))
                (call-undead (x.1)) ; <-- one variable in call-undead-set
                (undead-out
                    ((tmp-ra.2 rbp)
                    (tmp-ra.2 rsi rbp)
                    (tmp-ra.2 rsi rdi rbp)
                    (rsi rdi r15 rbp)
                    (rsi rdi r15 rbp)))
                (conflicts                       ; <-- even if this variable does not have any conflicts data, assign it anyway
                    ((tmp-ra.2 (rdi rsi rbp))
                    (rbp (r15 rdi rsi tmp-ra.2))
                    (rsi (r15 rdi rbp tmp-ra.2))
                    (rdi (r15 rbp rsi tmp-ra.2))
                    (r15 (rbp rdi rsi))))
                (assignment ((x.1 fv0)))) ; <-- we have a single assignment
                (begin
                    (set! tmp-ra.2 r15)
                    (set! rsi 2)
                    (set! rdi 1)
                    (set! r15 tmp-ra.2)
                    (jump L.swap.1 rbp r15 rdi rsi)))))

(test-case "assign-call-undead-variables 3 - fvars in call-undead set " 
    (check-equal? 
    (assign-call-undead-variables 
        '(module
            ((new-frames ())
            (locals (tmp-ra.2))
            (call-undead (fv2 fv1 fv0))
            (undead-out
                ((tmp-ra.2 rbp)
                (tmp-ra.2 rsi rbp)
                (tmp-ra.2 rsi rdi rbp)
                (rsi rdi r15 rbp)
                (rsi rdi r15 rbp)))
            (conflicts
                ((fv0 (fv2 fv1 fv3))
                (fv1 (fv2 fv0 fv3))
                (fv2 (fv0 fv1 fv3))
                (fv3 (fv2 fv1 fv3)))))
            (begin
                (set! tmp-ra.2 r15)
                (set! rsi 2)
                (set! rdi 1)
                (set! r15 tmp-ra.2)
                (jump L.swap.1 rbp r15 rdi rsi))))

            '(module
                ((new-frames ())
                (locals (tmp-ra.2))
                (call-undead (fv2 fv1 fv0)) ; <-- some fvars in the call-undead set
                (undead-out
                    ((tmp-ra.2 rbp)
                    (tmp-ra.2 rsi rbp)
                    (tmp-ra.2 rsi rdi rbp)
                    (rsi rdi r15 rbp)
                    (rsi rdi r15 rbp)))
                (conflicts
                    ((fv0 (fv2 fv1 fv3)) 
                    (fv1 (fv2 fv0 fv3))
                    (fv2 (fv0 fv1 fv3))
                    (fv3 (fv2 fv1 fv3))))
                (assignment ())) ; <-- fvars do not get assigned to anything 
                (begin
                    (set! tmp-ra.2 r15)
                    (set! rsi 2)
                    (set! rdi 1)
                    (set! r15 tmp-ra.2)
                    (jump L.swap.1 rbp r15 rdi rsi)))))

(test-case "assign-call-undead-variables 4 - fvars and alocs in call-undead set " 
    (check-equal? 
    (assign-call-undead-variables 
        '(module
            ((new-frames ())
            (locals (tmp-ra.2))
            (call-undead (x.1 y.1 fv0))
            (undead-out
                ((tmp-ra.2 rbp)
                (tmp-ra.2 rsi rbp)
                (tmp-ra.2 rsi rdi rbp)
                (rsi rdi r15 rbp)
                (rsi rdi r15 rbp)))
            (conflicts
                ((fv0 ())
                (x.1 ())
                (y.1 ()))))
            (begin
                (set! tmp-ra.2 r15)
                (set! rsi 2)
                (set! rdi 1)
                (set! r15 tmp-ra.2)
                (jump L.swap.1 rbp r15 rdi rsi))))

            '(module
                ((new-frames ())
                (locals (tmp-ra.2))
                (call-undead (x.1 y.1 fv0)) ; <-- there is a frame variable in call-undead
                (undead-out
                    ((tmp-ra.2 rbp)
                    (tmp-ra.2 rsi rbp)
                    (tmp-ra.2 rsi rdi rbp)
                    (rsi rdi r15 rbp)
                    (rsi rdi r15 rbp)))
                (conflicts ((fv0 ()) (x.1 ()) (y.1 ())))
                (assignment ((y.1 fv0) (x.1 fv0)))) ; <-- we do not assign it 
                (begin
                    (set! tmp-ra.2 r15)
                    (set! rsi 2)
                    (set! rdi 1)
                    (set! r15 tmp-ra.2)
                    (jump L.swap.1 rbp r15 rdi rsi)))))

(test-case "assign-call-undead-variables 5 - fvars, alocs, and registers in call-undead set" 
    (check-equal? 
    (assign-call-undead-variables 
        '(module
            ((new-frames ())
            (locals (tmp-ra.2))
            (call-undead (x.1 y.1 fv0 rax))
            (undead-out
                ((tmp-ra.2 rbp)
                (tmp-ra.2 rsi rbp)
                (tmp-ra.2 rsi rdi rbp)
                (rsi rdi r15 rbp)
                (rsi rdi r15 rbp)))
            (conflicts
                ((fv0 ())
                (x.1 ())
                (y.1 ())
                (rax ()))))
            (begin
                (set! tmp-ra.2 r15)
                (set! rsi 2)
                (set! rdi 1)
                (set! r15 tmp-ra.2)
                (jump L.swap.1 rbp r15 rdi rsi))))

            '(module
                ((new-frames ())
                (locals (tmp-ra.2))
                (call-undead (x.1 y.1 fv0 rax)) ; <-- there is a frame variable and register in call-udnead
                (undead-out
                    ((tmp-ra.2 rbp)
                    (tmp-ra.2 rsi rbp)
                    (tmp-ra.2 rsi rdi rbp)
                    (rsi rdi r15 rbp)
                    (rsi rdi r15 rbp)))
                (conflicts ((fv0 ()) (x.1 ()) (y.1 ()) (rax ())))
                (assignment ((y.1 fv0) (x.1 fv0)))) ; <-- only the alocs are assigned
                (begin
                    (set! tmp-ra.2 r15)
                    (set! rsi 2)
                    (set! rdi 1)
                    (set! r15 tmp-ra.2)
                    (jump L.swap.1 rbp r15 rdi rsi)))))

(test-case "assign-call-undead-variables 6 - simple direct conflict" 
    (check-equal? 
        (assign-call-undead-variables 
        '(module
            ((new-frames ())
            (locals (tmp-ra.2))
            (call-undead (x.1 y.1 z.1))
            (undead-out
                ((tmp-ra.2 rbp)
                (tmp-ra.2 rsi rbp)
                (tmp-ra.2 rsi rdi rbp)
                (rsi rdi r15 rbp)
                (rsi rdi r15 rbp)))
            (conflicts
                ((x.1 (y.1 z.1))
                (y.1 (x.1 z.1))
                (z.1 (x.1 y.1)))))
            (begin
                (set! tmp-ra.2 r15)
                (set! rsi 2)
                (set! rdi 1)
                (set! r15 tmp-ra.2)
                (jump L.swap.1 rbp r15 rdi rsi))))

            '(module
                ((new-frames ())
                (locals (tmp-ra.2))
                (call-undead (x.1 y.1 z.1))
                (undead-out
                    ((tmp-ra.2 rbp)
                    (tmp-ra.2 rsi rbp)
                    (tmp-ra.2 rsi rdi rbp)
                    (rsi rdi r15 rbp)
                    (rsi rdi r15 rbp)))
                (conflicts ((x.1 (y.1 z.1)) (y.1 (x.1 z.1)) (z.1 (x.1 y.1))))
                (assignment ((z.1 fv0) (y.1 fv1) (x.1 fv2))))
                (begin
                    (set! tmp-ra.2 r15)
                    (set! rsi 2)
                    (set! rdi 1)
                    (set! r15 tmp-ra.2)
                    (jump L.swap.1 rbp r15 rdi rsi)))))

(test-case "assign-call-undead-variables 7 - multiple assignments in define block"
    (check-equal? 
        (assign-call-undead-variables
            '(module
            ((new-frames ())
            (locals (tmp-ra.2))
            (call-undead ())
            (undead-out
                ((tmp-ra.2 rbp)
                (tmp-ra.2 rsi rbp)
                (tmp-ra.2 rsi rdi rbp)
                (rsi rdi r15 rbp)
                (rsi rdi r15 rbp)))
            (conflicts
                ((tmp-ra.2 (rdi rsi rbp))
                (rbp (r15 rdi rsi tmp-ra.2))
                (rsi (r15 rdi rbp tmp-ra.2))
                (rdi (r15 rbp rsi tmp-ra.2))
                (r15 (rbp rdi rsi)))))
            (define L.swap.1
                ((new-frames (()))
                (locals (tmp-ra.1))
                (undead-out
                ((rdi rsi tmp-ra.1 rbp)
                (rsi x.1 tmp-ra.1 rbp)
                (y.2 x.1 tmp-ra.1 rbp)
                ((y.2 x.1 tmp-ra.1 rbp)
                    ((tmp-ra.1 rax rbp) (rax rbp))
                    (((rax tmp-ra.1 rbp)
                    ((y.2 rsi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                    (z.3 tmp-ra.1 rbp)
                    (tmp-ra.1 rax rbp)
                    (rax rbp)))))
                (call-undead (tmp-ra.1 z.3 z.4))
                (conflicts
                ((y.2 (rbp tmp-ra.1 x.1 rsi))
                (x.1 (y.2 rbp tmp-ra.1 rsi))
                (tmp-ra.1 (fv0 fv1 fv2 fv3 fv4 fv5 y.2 x.1 rbp rsi rdi rax z.3))
                (z.3 (rbp tmp-ra.1))
                (rsi (x.1 tmp-ra.1 r15 rdi rbp y.2))
                (rbp (y.2 x.1 tmp-ra.1 rax z.3 r15 rdi rsi))
                (rdi (tmp-ra.1 r15 rbp rsi))
                (r15 (rbp rdi rsi))
                (rax (rbp tmp-ra.1)))))
                (begin
                (set! tmp-ra.1 r15)
                (set! x.1 rdi)
                (set! y.2 rsi)
                (if (< y.2 x.1)
                    (begin (set! rax x.1) (jump tmp-ra.1 rbp rax))
                    (begin
                    (return-point L.rp.1
                        (begin
                        (set! rsi x.1)
                        (set! rdi y.2)
                        (set! r15 L.rp.1)
                        (jump L.swap.1 rbp r15 rdi rsi)))
                    (set! z.3 rax)
                    (set! rax z.3)
                    (jump tmp-ra.1 rbp rax)))))
            (begin
                (set! tmp-ra.2 r15)
                (set! rsi 2)
                (set! rdi 1)
                (set! r15 tmp-ra.2)
                (jump L.swap.1 rbp r15 rdi rsi))))

                '(module
                    ((new-frames ())
                    (locals (tmp-ra.2))
                    (call-undead ())
                    (undead-out
                        ((tmp-ra.2 rbp)
                        (tmp-ra.2 rsi rbp)
                        (tmp-ra.2 rsi rdi rbp)
                        (rsi rdi r15 rbp)
                        (rsi rdi r15 rbp)))
                    (conflicts
                        ((tmp-ra.2 (rdi rsi rbp))
                        (rbp (r15 rdi rsi tmp-ra.2))
                        (rsi (r15 rdi rbp tmp-ra.2))
                        (rdi (r15 rbp rsi tmp-ra.2))
                        (r15 (rbp rdi rsi))))
                    (assignment ()))
                    (define L.swap.1
                        ((new-frames (()))
                        (locals ())
                        (undead-out
                        ((rdi rsi tmp-ra.1 rbp)
                        (rsi x.1 tmp-ra.1 rbp)
                        (y.2 x.1 tmp-ra.1 rbp)
                        ((y.2 x.1 tmp-ra.1 rbp)
                            ((tmp-ra.1 rax rbp) (rax rbp))
                            (((rax tmp-ra.1 rbp)
                            ((y.2 rsi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                            (z.3 tmp-ra.1 rbp)
                            (tmp-ra.1 rax rbp)
                            (rax rbp)))))
                        (call-undead (tmp-ra.1 z.3 z.4))
                        (conflicts
                        ((y.2 (rbp tmp-ra.1 x.1 rsi))
                        (x.1 (y.2 rbp tmp-ra.1 rsi))
                        (tmp-ra.1 (fv0 fv1 fv2 fv3 fv4 fv5 y.2 x.1 rbp rsi rdi rax z.3))
                        (z.3 (rbp tmp-ra.1))
                        (rsi (x.1 tmp-ra.1 r15 rdi rbp y.2))
                        (rbp (y.2 x.1 tmp-ra.1 rax z.3 r15 rdi rsi))
                        (rdi (tmp-ra.1 r15 rbp rsi))
                        (r15 (rbp rdi rsi))
                        (rax (rbp tmp-ra.1))))
                        (assignment ((z.4 fv0) (z.3 fv0) (tmp-ra.1 fv6))))
                        (begin
                        (set! tmp-ra.1 r15)
                        (set! x.1 rdi)
                        (set! y.2 rsi)
                        (if (< y.2 x.1)
                            (begin (set! rax x.1) (jump tmp-ra.1 rbp rax))
                            (begin
                            (return-point
                            L.rp.1
                            (begin
                                (set! rsi x.1)
                                (set! rdi y.2)
                                (set! r15 L.rp.1)
                                (jump L.swap.1 rbp r15 rdi rsi)))
                            (set! z.3 rax)
                            (set! rax z.3)
                            (jump tmp-ra.1 rbp rax)))))
                    (begin
                        (set! tmp-ra.2 r15)
                        (set! rsi 2)
                        (set! rdi 1)
                        (set! r15 tmp-ra.2)
                        (jump L.swap.1 rbp r15 rdi rsi)))))

(test-case "assign-call-undead-variables 8 - multiple assignments in define block"
    (check-equal? 
                (assign-call-undead-variables
            '(module
            ((new-frames ())
            (locals (tmp-ra.2 z.3 z.4 z.5 z.6))
            (call-undead ())
            (undead-out
                ((tmp-ra.2 rbp)
                (tmp-ra.2 rsi rbp)
                (tmp-ra.2 rsi rdi rbp)
                (rsi rdi r15 rbp)
                (rsi rdi r15 rbp)))
            (conflicts
                ((tmp-ra.2 (rdi rsi rbp))
                (rbp (r15 rdi rsi tmp-ra.2))
                (rsi (r15 rdi rbp tmp-ra.2))
                (rdi (r15 rbp rsi tmp-ra.2))
                (r15 (rbp rdi rsi)))))
            (define L.swap.1
                ((new-frames (()))
                (locals (tmp-ra.1 z.5 z.4 z.3 z.2))
                (undead-out
                ((rdi rsi tmp-ra.1 rbp)
                (rsi x.1 tmp-ra.1 rbp)
                (y.2 x.1 tmp-ra.1 rbp)
                ((y.2 x.1 tmp-ra.1 rbp)
                    ((tmp-ra.1 rax rbp) (rax rbp))
                    (((rax tmp-ra.1 rbp)
                    ((y.2 rsi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                    (z.3 tmp-ra.1 rbp)
                    (tmp-ra.1 rax rbp)
                    (rax rbp)))))
                (call-undead (tmp-ra.1 z.3 z.4))
                (conflicts
                ((y.2 (rbp tmp-ra.1 x.1 rsi))
                (x.1 (y.2 rbp tmp-ra.1 rsi))
                (tmp-ra.1 (fv0 fv1 fv2 fv3 fv4 fv5 y.2 x.1 rbp rsi rdi rax z.3))
                (z.3 (rbp tmp-ra.1))
                (rsi (x.1 tmp-ra.1 r15 rdi rbp y.2))
                (rbp (y.2 x.1 tmp-ra.1 rax z.3 r15 rdi rsi))
                (rdi (tmp-ra.1 r15 rbp rsi))
                (r15 (rbp rdi rsi))
                (rax (rbp tmp-ra.1)))))
                (begin
                (set! tmp-ra.1 r15)
                (set! x.1 rdi)
                (set! y.2 rsi)
                (if (< y.2 x.1)
                    (begin (set! rax x.1) (jump tmp-ra.1 rbp rax))
                    (begin
                    (return-point L.rp.1
                        (begin
                        (set! rsi x.1)
                        (set! rdi y.2)
                        (set! r15 L.rp.1)
                        (jump L.swap.1 rbp r15 rdi rsi)))
                    (set! z.3 rax)
                    (set! rax z.3)
                    (jump tmp-ra.1 rbp rax)))))
            (begin
                (set! tmp-ra.2 r15)
                (set! rsi 2)
                (set! rdi 1)
                (set! r15 tmp-ra.2)
                (jump L.swap.1 rbp r15 rdi rsi))))

                '(module
                    ((new-frames ())
                    (locals (z.6 z.5 z.4 z.3 tmp-ra.2))
                    (call-undead ())
                    (undead-out
                        ((tmp-ra.2 rbp)
                        (tmp-ra.2 rsi rbp)
                        (tmp-ra.2 rsi rdi rbp)
                        (rsi rdi r15 rbp)
                        (rsi rdi r15 rbp)))
                    (conflicts
                        ((tmp-ra.2 (rdi rsi rbp))
                        (rbp (r15 rdi rsi tmp-ra.2))
                        (rsi (r15 rdi rbp tmp-ra.2))
                        (rdi (r15 rbp rsi tmp-ra.2))
                        (r15 (rbp rdi rsi))))
                    (assignment ()))
                    (define L.swap.1
                        ((new-frames (()))
                        (locals (z.2 z.5))
                        (undead-out
                        ((rdi rsi tmp-ra.1 rbp)
                        (rsi x.1 tmp-ra.1 rbp)
                        (y.2 x.1 tmp-ra.1 rbp)
                        ((y.2 x.1 tmp-ra.1 rbp)
                            ((tmp-ra.1 rax rbp) (rax rbp))
                            (((rax tmp-ra.1 rbp)
                            ((y.2 rsi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                            (z.3 tmp-ra.1 rbp)
                            (tmp-ra.1 rax rbp)
                            (rax rbp)))))
                        (call-undead (tmp-ra.1 z.3 z.4))
                        (conflicts
                        ((y.2 (rbp tmp-ra.1 x.1 rsi))
                        (x.1 (y.2 rbp tmp-ra.1 rsi))
                        (tmp-ra.1 (fv0 fv1 fv2 fv3 fv4 fv5 y.2 x.1 rbp rsi rdi rax z.3))
                        (z.3 (rbp tmp-ra.1))
                        (rsi (x.1 tmp-ra.1 r15 rdi rbp y.2))
                        (rbp (y.2 x.1 tmp-ra.1 rax z.3 r15 rdi rsi))
                        (rdi (tmp-ra.1 r15 rbp rsi))
                        (r15 (rbp rdi rsi))
                        (rax (rbp tmp-ra.1))))
                        (assignment ((z.4 fv0) (z.3 fv0) (tmp-ra.1 fv6))))
                        (begin
                        (set! tmp-ra.1 r15)
                        (set! x.1 rdi)
                        (set! y.2 rsi)
                        (if (< y.2 x.1)
                            (begin (set! rax x.1) (jump tmp-ra.1 rbp rax))
                            (begin
                            (return-point
                            L.rp.1
                            (begin
                                (set! rsi x.1)
                                (set! rdi y.2)
                                (set! r15 L.rp.1)
                                (jump L.swap.1 rbp r15 rdi rsi)))
                            (set! z.3 rax)
                            (set! rax z.3)
                            (jump tmp-ra.1 rbp rax)))))
                    (begin
                        (set! tmp-ra.2 r15)
                        (set! rsi 2)
                        (set! rdi 1)
                        (set! r15 tmp-ra.2)
                        (jump L.swap.1 rbp r15 rdi rsi)))))

; M7 Tests

(test-case "assign-call-undead-variables 9 - extend binops" 
    (check-equal? 
        (assign-call-undead-variables 
        '(module
            ((new-frames ())
            (locals (tmp-ra.2))
            (call-undead (x.1)) 
            (undead-out
                ((tmp-ra.2 rbp)
                (tmp-ra.2 rsi rbp)
                (tmp-ra.2 rsi rdi rbp)
                (rsi rdi r15 rbp)
                (rsi rdi r15 rbp)))
            (conflicts
                ((tmp-ra.2 (rdi rsi rbp))
                (rbp (r15 rdi rsi tmp-ra.2))
                (rsi (r15 rdi rbp tmp-ra.2))
                (rdi (r15 rbp rsi tmp-ra.2))
                (r15 (rbp rdi rsi)))))
            (begin
                (set! tmp-ra.2 r15)
                (set! rsi (+ rsi 2))
                (set! rdi (arithmetic-shift-right rdi 5))
                (set! r15 tmp-ra.2)
                (jump L.swap.1 rbp r15 rdi rsi))))

            '(module
                ((new-frames ())
                (locals (tmp-ra.2))
                (call-undead (x.1))
                (undead-out
                    ((tmp-ra.2 rbp)
                    (tmp-ra.2 rsi rbp)
                    (tmp-ra.2 rsi rdi rbp)
                    (rsi rdi r15 rbp)
                    (rsi rdi r15 rbp)))
                (conflicts
                    ((tmp-ra.2 (rdi rsi rbp))
                    (rbp (r15 rdi rsi tmp-ra.2))
                    (rsi (r15 rdi rbp tmp-ra.2))
                    (rdi (r15 rbp rsi tmp-ra.2))
                    (r15 (rbp rdi rsi))))
                (assignment ((x.1 fv0))))
                (begin
                    (set! tmp-ra.2 r15)
                    (set! rsi (+ rsi 2))
                    (set! rdi (arithmetic-shift-right rdi 5))
                    (set! r15 tmp-ra.2)
                    (jump L.swap.1 rbp r15 rdi rsi)))))