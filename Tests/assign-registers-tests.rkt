#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt"
)

(test-case "assign-registers 1 - textbook example"
    (check-equal? 
        (assign-registers 
        '(module
            ((locals (tmp-ra.14))
            (conflicts
                ((tmp-ra.14 (fv0 fv1 rbp))
                (rbp (r15 fv0 fv1 tmp-ra.14))
                (fv1 (r15 fv0 rbp tmp-ra.14))
                (fv0 (r15 rbp fv1 tmp-ra.14))
                (r15 (rbp fv0 fv1))))
            (assignment ()))

            (define L.swap.1

                ((locals (z.3 x.1 y.2))

                (conflicts
                ((y.2 (rbp tmp-ra.11 x.1 nfv.13))
                (x.1 (y.2 rbp tmp-ra.11 fv1))
                (tmp-ra.11 (y.2 x.1 rbp fv1 fv0 rax z.3))
                (z.3 (rbp tmp-ra.11))
                (nfv.13 (r15 nfv.12 rbp y.2))
                (nfv.12 (r15 rbp nfv.13))
                (rbp (y.2 x.1 tmp-ra.11 rax z.3 r15 nfv.12 nfv.13))
                (r15 (rbp nfv.12 nfv.13))
                (rax (rbp tmp-ra.11))
                (fv0 (tmp-ra.11))
                (fv1 (x.1 tmp-ra.11))))

                (assignment ((tmp-ra.11 fv2) (nfv.12 fv3) (nfv.13 fv4))))

                (begin
                (set! tmp-ra.11 r15)
                (set! x.1 fv0)
                (set! y.2 fv1)
                (if (< y.2 x.1)
                    (begin (set! rax x.1) (jump tmp-ra.11 rbp rax))
                    (begin
                    (begin
                        (set! rbp (- rbp 24))
                        (return-point L.rp.4
                        (begin
                            (set! nfv.13 x.1)
                            (set! nfv.12 y.2)
                            (set! r15 L.rp.4)
                            (jump L.swap.1 rbp r15 nfv.12 nfv.13)))
                        (set! rbp (+ rbp 24)))
                    (set! z.3 rax)
                    (set! rax z.3)
                    (jump tmp-ra.11 rbp rax)))))
            (begin
                (set! tmp-ra.14 r15)
                (set! fv1 2)
                (set! fv0 1)
                (set! r15 tmp-ra.14)
                (jump L.swap.1 rbp r15 fv0 fv1))))

        '(module
            ((locals ())
            (conflicts
                ((tmp-ra.14 (fv0 fv1 rbp))
                (rbp (r15 fv0 fv1 tmp-ra.14))
                (fv1 (r15 fv0 rbp tmp-ra.14))
                (fv0 (r15 rbp fv1 tmp-ra.14))
                (r15 (rbp fv0 fv1))))

            (assignment ((tmp-ra.14 r15))))

            (define L.swap.1

                ((locals ())

                (conflicts
                ((y.2 (rbp tmp-ra.11 x.1 nfv.13))
                (x.1 (y.2 rbp tmp-ra.11 fv1))
                (tmp-ra.11 (y.2 x.1 rbp fv1 fv0 rax z.3))
                (z.3 (rbp tmp-ra.11))
                (nfv.13 (r15 nfv.12 rbp y.2))
                (nfv.12 (r15 rbp nfv.13))
                (rbp (y.2 x.1 tmp-ra.11 rax z.3 r15 nfv.12 nfv.13))
                (r15 (rbp nfv.12 nfv.13))
                (rax (rbp tmp-ra.11))
                (fv0 (tmp-ra.11))
                (fv1 (x.1 tmp-ra.11))))

                (assignment
                ((tmp-ra.11 fv2) (nfv.12 fv3) (nfv.13 fv4) (z.3 r15) (x.1 r14) (y.2 r15))))

                (begin
                (set! tmp-ra.11 r15)
                (set! x.1 fv0)
                (set! y.2 fv1)
                (if (< y.2 x.1)
                    (begin (set! rax x.1) (jump tmp-ra.11 rbp rax))
                    (begin
                    (begin
                        (set! rbp (- rbp 24))
                        (return-point L.rp.4
                            (begin
                            (set! nfv.13 x.1)
                            (set! nfv.12 y.2)
                            (set! r15 L.rp.4)
                            (jump L.swap.1 rbp r15 nfv.12 nfv.13)))
                        (set! rbp (+ rbp 24)))
                    (set! z.3 rax)
                    (set! rax z.3)
                    (jump tmp-ra.11 rbp rax)))))
            (begin
                (set! tmp-ra.14 r15)
                (set! fv1 2)
                (set! fv0 1)
                (set! r15 tmp-ra.14)
                (jump L.swap.1 rbp r15 fv0 fv1)))))

(test-case "assign-registers 2 - simple single assignment"
    (check-equal? 
        (assign-registers 
            '(module
                ((locals (tmp-ra.14))
                 (conflicts
                    ((tmp-ra.14 (fv0 fv1 rbp))
                     (rbp (r15 fv0 fv1 tmp-ra.14))
                     (fv1 (r15 fv0 rbp tmp-ra.14))
                     (fv0 (r15 rbp fv1 tmp-ra.14))
                     (r15 (rbp fv0 fv1))))
                 (assignment ()))

                (begin
                    (set! tmp-ra.14 r15)
                    (set! fv1 2)
                    (set! fv0 1)
                    (set! r15 tmp-ra.14)
                    (jump L.swap.1 rbp r15 fv0 fv1))))

        '(module
            ((locals ())
             (conflicts
                ((tmp-ra.14 (fv0 fv1 rbp))
                 (rbp (r15 fv0 fv1 tmp-ra.14))
                 (fv1 (r15 fv0 rbp tmp-ra.14))
                 (fv0 (r15 rbp fv1 tmp-ra.14))
                 (r15 (rbp fv0 fv1))))
            (assignment ((tmp-ra.14 r15))))
            (begin
                (set! tmp-ra.14 r15)
                (set! fv1 2)
                (set! fv0 1)
                (set! r15 tmp-ra.14)
                (jump L.swap.1 rbp r15 fv0 fv1)))))

(test-case "assign-registers 3 - no registers available, empty assignment"
    (check-equal? 
        (parameterize   ([current-assignable-registers '(r15)])         
                        (assign-registers 
                        '(module
                            ((locals (tmp-ra.14))
                            (conflicts
                                ((tmp-ra.14 (r15 fv0 fv1 rbp))
                                (rbp (r15 fv0 fv1 tmp-ra.14))
                                (fv1 (r15 fv0 rbp tmp-ra.14))
                                (fv0 (r15 rbp fv1 tmp-ra.14))
                                (r15 (rbp fv0 fv1))))
                            (assignment ()))

                            (begin
                                (set! tmp-ra.14 r15)
                                (set! fv1 2)
                                (set! fv0 1)
                                (set! r15 tmp-ra.14)
                                (jump L.swap.1 rbp r15 fv0 fv1)))))


        '(module
            ((locals (tmp-ra.14))
            (conflicts
                ((tmp-ra.14 (r15 fv0 fv1 rbp))
                (rbp (r15 fv0 fv1 tmp-ra.14))
                (fv1 (r15 fv0 rbp tmp-ra.14))
                (fv0 (r15 rbp fv1 tmp-ra.14))
                (r15 (rbp fv0 fv1))))
            (assignment ()))
            (begin
                (set! tmp-ra.14 r15)
                (set! fv1 2)
                (set! fv0 1)
                (set! r15 tmp-ra.14)
                (jump L.swap.1 rbp r15 fv0 fv1)))))

(test-case "assign-registers 4 - three registers available, single assignment"
    (check-equal? 
        (parameterize   ([current-assignable-registers '(r15 rax rcx)])         
                        (assign-registers  
                        '(module
                            ((locals (tmp-ra.14))
                            (conflicts
                                ((tmp-ra.14 (r15 fv0 fv1 rbp))
                                (rbp (r15 fv0 fv1 tmp-ra.14))
                                (fv1 (r15 fv0 rbp tmp-ra.14))
                                (fv0 (r15 rbp fv1 tmp-ra.14))
                                (r15 (rbp fv0 fv1))))
                            (assignment ()))

                            (begin
                                (set! tmp-ra.14 r15)
                                (set! fv1 2)
                                (set! fv0 1)
                                (set! r15 tmp-ra.14)
                                (jump L.swap.1 rbp r15 fv0 fv1)))))

        '(module
            ((locals ())
            (conflicts
                ((tmp-ra.14 (r15 fv0 fv1 rbp))
                (rbp (r15 fv0 fv1 tmp-ra.14))
                (fv1 (r15 fv0 rbp tmp-ra.14))
                (fv0 (r15 rbp fv1 tmp-ra.14))
                (r15 (rbp fv0 fv1))))
            (assignment ((tmp-ra.14 rcx))))
            (begin
                (set! tmp-ra.14 r15)
                (set! fv1 2)
                (set! fv0 1)
                (set! r15 tmp-ra.14)
                (jump L.swap.1 rbp r15 fv0 fv1)))))

(test-case "assign-registers 5 - three registers available, double assignment"
    (check-equal? 
        (parameterize   ([current-assignable-registers '(rax rcx r15)])         
                                (assign-registers  
                                '(module
                                    ((locals (tmp-ra.14 tmp-ra.15))
                                    (conflicts
                                        ((tmp-ra.14 (r15 fv0 fv1 rbp))
                                        (tmp-ra.15 (rax tmp-ra.14))
                                        (rbp (r15 fv0 fv1 tmp-ra.14))
                                        (fv1 (r15 fv0 rbp tmp-ra.14))
                                        (fv0 (r15 rbp fv1 tmp-ra.14))
                                        (r15 (rbp fv0 fv1))))
                                    (assignment ()))

                                    (begin
                                        (set! tmp-ra.14 r15)
                                        (set! fv1 2)
                                        (set! fv0 1)
                                        (set! r15 tmp-ra.14)
                                        (jump L.swap.1 rbp r15 fv0 fv1)))))


        '(module
            ((locals ())
            (conflicts
                ((tmp-ra.14 (r15 fv0 fv1 rbp))
                (tmp-ra.15 (rax tmp-ra.14))
                (rbp (r15 fv0 fv1 tmp-ra.14))
                (fv1 (r15 fv0 rbp tmp-ra.14))
                (fv0 (r15 rbp fv1 tmp-ra.14))
                (r15 (rbp fv0 fv1))))
            (assignment ((tmp-ra.14 rcx) (tmp-ra.15 r15))))
            (begin
                (set! tmp-ra.14 r15)
                (set! fv1 2)
                (set! fv0 1)
                (set! r15 tmp-ra.14)
                (jump L.swap.1 rbp r15 fv0 fv1)))))

(test-case "assign-registers 6 - three registers available, double assignment, two unmatched locals"
    (check-equal? 
        (parameterize   ([current-assignable-registers '(rax rcx r15)])         
                                (assign-registers  
                                '(module
                                    ((locals (tmp-ra.14 tmp-ra.15 tmp-ra.16 tmp-ra.17))
                                    (conflicts
                                        ((tmp-ra.14 (r15 fv0 fv1 rbp tmp-ra.15))
                                        (tmp-ra.15 (rax tmp-ra.14))
                                        (tmp-ra.16 (r15 rax rcx))
                                        (tmp-ra.17 (r15 rax rcx)) ))
                                    (assignment ()))

                                    (begin
                                        (set! tmp-ra.14 r15)
                                        (set! fv1 2)
                                        (set! fv0 1)
                                        (set! r15 tmp-ra.14)
                                        (jump L.swap.1 rbp r15 fv0 fv1)))))


        '(module
            ((locals (tmp-ra.16 tmp-ra.17))
            (conflicts
                ((tmp-ra.14 (r15 fv0 fv1 rbp tmp-ra.15))
                (tmp-ra.15 (rax tmp-ra.14))
                (tmp-ra.16 (r15 rax rcx))
                (tmp-ra.17 (r15 rax rcx))))
            (assignment ((tmp-ra.14 rcx) (tmp-ra.15 r15))))
            (begin
                (set! tmp-ra.14 r15)
                (set! fv1 2)
                (set! fv0 1)
                (set! r15 tmp-ra.14)
                (jump L.swap.1 rbp r15 fv0 fv1)))))


