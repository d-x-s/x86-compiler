#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt")

; M6 Tests

(test-case "optimize-predicates 1 - if true"
    (check-match
        (optimize-predicates
            `(module 
                (begin 
                    (set! r14 5) 
                    (if (true) 
                        (begin (set! r15 r14) (set! r15 (+ r15 17)) (set! r15 12)) 
                        (begin (set! r15 15))) 
                    (jump r15)))
        )

     `(module
        (begin
            (set! r14 5)
            (begin (set! r15 r14) (set! r15 (+ r15 17)) (set! r15 12))
            (jump r15)))))

(test-case "optimize-predicates 2 - if not"
    (check-match
        (optimize-predicates
            `(module 
                (begin 
                    (set! r8 0) 
                    (set! r9 0) 
                    (if (not (if (true) (> r8 5) (< r9 6))) 
                        (set! r12 15) 
                        (set! r12 90)) 
                    (jump r12)))
        )

     `(module (begin (set! r8 0) (set! r9 0) (set! r12 15) (jump r12)))))

(test-case "optimize-predicates 3 - if registers equal"
    (check-match
        (optimize-predicates
            `(module 
                (begin 
                    (set! r15 5) 
                    (set! r15 r15) 
                    (begin 
                        (set! r14 r15) 
                        (set! r14 (+ r14 r15)) 
                        (set! r15 r14) 
                        (if (= r15 r14) 
                            (jump r15) 
                            (begin (set! r15 r15) (jump r15))))))
        )

     `(module
        (begin
            (set! r15 5)
            (set! r15 r15)
            (begin 
                (set! r14 r15)
                (set! r14 (+ r14 r15)) 
                (set! r15 r14) 
                (jump r15))))))

(test-case "optimize-predicates 4 - if branches are begins"
    (check-match
        (optimize-predicates
            `(module 
                (begin 
                    (set! r14 0) 
                    (if (true) 
                        (begin (set! r15 r14) (set! r15 (+ r15 17)) (set! r15 12)) 
                        (begin (set! r15 15))) (jump r15)))
        )

     `(module
        (begin
            (set! r14 0)
            (begin (set! r15 r14) (set! r15 (+ r15 17)) (set! r15 12))
            (jump r15)))))

(test-case "optimize-predicates 5 - register not depending on branch"
    (check-match
        (optimize-predicates
            `(module 
                (begin 
                    (set! r8 12) 
                    (set! r9 12) 
                    (if (not (true)) 
                        (set! r12 15) 
                        (set! r13 90))
                    (if (= r8 12)
                        (set! r12 1)
                        (set! r12 2))
                    (jump r8)))
        )

     `(module (begin (set! r8 12) (set! r9 12) (set! r13 90) (set! r12 1) (jump r8)))))

(test-case "optimize-predicates 6 - register depending on branch"
    (check-match
        (optimize-predicates
            `(module 
                (begin 
                    (if (true) 
                        (begin (set! r12 14) (set! r15 12)) 
                        (begin (set! r15 15)))
                    (if (= r15 12)
                        (set! r12 1)
                        (set! r12 2))
                    (jump r15)))
        )

     `(module
        (begin
            (begin (set! r12 14) (set! r15 12))
            (if (= r15 12) (set! r12 1) (set! r12 2))
            (jump r15)))))

(test-case "optimize-predicates 7 - define functions"
    (check-match
        (optimize-predicates
            `(module 
                (define L.start.1 (begin 
                                    (set! r8 12) 
                                    (set! r9 12) 
                                    (if (not (true)) 
                                        (set! r12 15) 
                                        (set! r13 90))
                                    (jump r8)))
                (define L.start.2 (begin 
                                    (set! r14 0) 
                                    (if (true) 
                                        (begin (set! r15 r14) (set! r15 (+ r15 17)) (set! r15 12)) 
                                        (begin (set! r15 15))) (jump r15)))
                (begin 
                    (if (true) 
                        (begin (set! r15 14) (set! r15 12)) 
                        (begin (set! r15 15))) 
                    (jump r15)))
        )

     `(module
        (define L.start.1 (begin (set! r8 12) (set! r9 12) (set! r13 90) (jump r8)))
        (define L.start.2
            (begin
            (set! r14 0)
            (begin (set! r15 r14) (set! r15 (+ r15 17)) (set! r15 12))
            (jump r15)))
        (begin (begin (set! r15 14) (set! r15 12)) (jump r15)))))

(test-case "optimize-predicates 8 - register value unknown after branch"
    (check-match
        (optimize-predicates
            `(module 
                (begin 
                    (set! r15 0) 
                    (if (true) 
                        (begin (set! r12 r15) (set! r12 (+ r12 17)) (set! r12 12)) 
                        (begin (set! r15 15)))
                    (if (= r15 12)
                        (set! r12 1)
                        (set! r12 2))
                    (jump r15)))
        )

     `(module
        (begin
            (set! r15 0)
            (begin (set! r12 r15) (set! r12 (+ r12 17)) (set! r12 12))
            (if (= r15 12) (set! r12 1) (set! r12 2))
            (jump r15)))))

(test-case "optimize-predicates 9 - complex test"
    (check-match
        (optimize-predicates
            `(module 
                (define L.odd?.6 (begin 
                                    (set! r15 rdi) 
                                    (if (= r15 0) 
                                        (jump 0) 
                                        (begin (set! r15 r15) (set! r15 (+ r15 -1)) (set! rdi r15) (jump L.even?.7))))) 
                (define L.even?.7 (begin 
                                    (set! r15 rdi) 
                                    (if (= r15 0) 
                                        (jump 1) 
                                        (begin (set! r15 r15) (set! r15 (+ r15 -1)) (set! rdi r15) (jump L.odd?.6))))) 
                (begin (set! rdi 5) (jump L.even?.7)))
        )

     `(module
        (define L.odd?.6
            (begin
            (set! r15 rdi)
            (if (= r15 0)
                (jump 0)
                (begin
                (set! r15 r15)
                (set! r15 (+ r15 -1))
                (set! rdi r15)
                (jump L.even?.7)))))
        (define L.even?.7
            (begin
            (set! r15 rdi)
            (if (= r15 0)
                (jump 1)
                (begin
                (set! r15 r15)
                (set! r15 (+ r15 -1))
                (set! rdi r15)
                (jump L.odd?.6)))))
        (begin (set! rdi 5) (jump L.even?.7)))))

(test-case "optimize-predicates 10 - binop is subtraction"
    (check-match
        (optimize-predicates
            `(module 
                (begin 
                    (set! r14 5)
                    (set! r14 (- r14 1)) 
                    (if (= r14 4) 
                        (begin (set! r15 r14) (set! r15 (+ r15 17)) (set! r15 12)) 
                        (begin (set! r15 15))) 
                    (jump r15)))
        )

     `(module
        (begin
            (set! r14 5)
            (set! r14 (- r14 1))
            (begin (set! r15 r14) (set! r15 (+ r15 17)) (set! r15 12))
            (jump r15)))))

(test-case "optimize-predicates 11 - simple return-points"
    (check-match
        (optimize-predicates
            `(module 
                (begin 
                    (set! r14 5)  
                    (return-point L.one.1 (jump L.one.1))
                    (return-point L.two.2 (begin (set! r14 1) (jump L.one.1)))
                    (return-point L.three.3 (if (false) (jump L.one.1) (jump L.one.2)))
                    (if (= r14 1)
                        (set! r15 1)
                        (set! r15 0)) 
                    (jump r15)))
        )

     `(module
        (begin
            (set! r14 5)
            (return-point L.one.1 (jump L.one.1))
            (return-point L.two.2 (begin (set! r14 1) (jump L.one.1)))
            (return-point L.three.3 (jump L.one.2))
            (if (= r14 1) (set! r15 1) (set! r15 0))
            (jump r15)))))

(test-case "optimize-predicates 12 - use env inside return-point"
    (check-match
        (optimize-predicates
            `(module 
                (begin 
                    (set! r14 5)
                    (set! r15 15)  
                    (return-point 
                        L.two.2 
                        (begin 
                            (set! r14 1)
                            (if (= r15 15) (set! fv1 1) (set! fv1 2))
                            (jump L.one.1)))
                    (if (= r14 1)
                        (set! r15 1)
                        (set! r15 0)) 
                    (jump r15)))
        )

     `(module
        (begin
            (set! r14 5)
            (set! r15 15)
            (return-point L.two.2 (begin (set! r14 1) (set! fv1 1) (jump L.one.1)))
            (if (= r14 1) (set! r15 1) (set! r15 0))
            (jump r15)))))

; M8 Tests

(test-case "optimize-predicates 13 - mset"
    (check-match
        (optimize-predicates
            `(module
                (begin
                    (mset! rsp 0 5)
                    (if (> rsp 5)
                        (jump L.s.1)
                        (jump L.s.2)))))

     `(module 
        (begin 
            (mset! rsp 0 5) 
            (if (> rsp 5) (jump L.s.1) (jump L.s.2))))))

(test-case "optimize-predicates 14 - mref"
    (check-match
        (optimize-predicates
            `(module
                (begin
                    (set! fv1 0)
                    (set! rsp (mref fv1 0))
                    (if (> rsp 5)
                        (jump L.s.1)
                        (jump L.s.2)))))

     `(module
        (begin
            (set! fv1 0)
            (set! rsp (mref fv1 0))
            (if (> rsp 5) (jump L.s.1) (jump L.s.2))))))