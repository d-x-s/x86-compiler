#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt")

; M6 Tests

(test-case "expose-basic-blocks 1 - simple jump"
    (check-match
        (expose-basic-blocks
            `(module (jump r8))
        )

     `(module (define L.__main.1 (jump r8)))))

(test-case "expose-basic-blocks 2 - simple begin"
    (check-match
        (expose-basic-blocks
            `(module 
                (begin 
                    (set! rax 10)
                    (jump r8)))
        )

     `(module (define L.__main.2 (begin (set! rax 10) (jump r8))))))

(test-case "expose-basic-blocks 3 - begin with multiple effects"
    (check-match
        (expose-basic-blocks
            `(module 
                (begin 
                    (set! rax 10)
                    (set! r12 (* r12 rdx))
                    (jump r8)))
        )

     `(module (define L.__main.3 (begin (set! rax 10) (set! r12 (* r12 rdx)) (jump r8))))))

(test-case "expose-basic-blocks 4 - effect is a begin"
    (check-match
        (expose-basic-blocks
            `(module 
                (begin 
                    (set! rax 10)
                    (set! r12 (* r12 rdx))
                    (begin (set! rbx 10) 
                           (set! r13 (+ r13 rax)))
                    (jump r8)))
        )

     `(module
        (define L.__main.4
            (begin
            (set! rax 10)
            (set! r12 (* r12 rdx))
            (set! rbx 10)
            (set! r13 (+ r13 rax))
            (jump r8))))))

(test-case "expose-basic-blocks 5 - effect is an if"
    (check-match
        (expose-basic-blocks
            `(module 
                (begin 
                    (set! rax 10)
                    (set! r12 (* r12 rdx))
                    (begin (set! rbx 10) 
                            (set! r13 (+ r13 rax))
                            (if (true) (set! r9 2) (set! r12 3)))
                    (jump r8)))
        )

     `(module
        (define L.__main.8
            (begin
                (set! rax 10)
                (set! r12 (* r12 rdx))
                (set! rbx 10)
                (set! r13 (+ r13 rax))
                (if (true) (jump L.tmp.5) (jump L.tmp.6))))
        (define L.tmp.5 (begin (set! r9 2) (jump L.tmp.7)))
        (define L.tmp.6 (begin (set! r12 3) (jump L.tmp.7)))
        (define L.tmp.7 (jump r8)))))

(test-case "expose-basic-blocks 6 - nested effect ifs"
    (check-match
        (expose-basic-blocks
            `(module 
                (begin 
                    (set! rax 10)
                    (set! r12 (* r12 rdx))
                    (begin (set! rbx 10) 
                            (set! r13 (+ r13 rax))
                            (if (true) (set! r9 0) (set! r8 1))
                            (if (false) 
                                (if (true) (set! r12 2) (set! r13 3))
                                (if (true) (set! r14 4) (set! r15 5))))
                    (jump rax)))
        )

    '(module 
        (define L.__main.21 
            (begin (set! rax 10) (set! r12 (* r12 rdx)) (set! rbx 10) (set! r13 (+ r13 rax)) 
                (if (true) (jump L.tmp.18) (jump L.tmp.19)))) 
        (define L.tmp.18 (begin (set! r9 0) (jump L.tmp.20))) 
        (define L.tmp.19 (begin (set! r8 1) (jump L.tmp.20))) 
        (define L.tmp.20 (if (false) (jump L.tmp.9) (jump L.tmp.10))) 
        (define L.tmp.9 (if (true) (jump L.tmp.12) (jump L.tmp.13))) 
        (define L.tmp.10 (if (true) (jump L.tmp.15) (jump L.tmp.16))) 
        (define L.tmp.11 (jump rax)) 
        (define L.tmp.15 (begin (set! r14 4) (jump L.tmp.17))) 
        (define L.tmp.16 (begin (set! r15 5) (jump L.tmp.17))) 
        (define L.tmp.17 (jump L.tmp.11)) 
        (define L.tmp.12 (begin (set! r12 2) (jump L.tmp.14))) 
        (define L.tmp.13 (begin (set! r13 3) (jump L.tmp.14))) 
        (define L.tmp.14 (jump L.tmp.11)))))

(test-case "expose-basic-blocks 7 - effects after branching"
    (check-match
        (expose-basic-blocks
            '(module 
                (begin 
                    (set! rax 10)
                    (set! r12 (* r12 rdx))
                    (begin (set! rbx 10) 
                            (set! r13 (+ r13 rax))
                            (if (true) (set! r8 0) (set! r9 1))
                            (if (false) 
                                (if (true) (set! r12 2) (set! r13 3))
                                (if (true) (set! r14 4) (set! r15 5))))
                    (set! rdi 11)
                    (set! r9 (+ r9 r8))
                    (jump rax)))
        )
    '(module 
        (define L.__main.34 
            (begin 
                (set! rax 10) 
                (set! r12 (* r12 rdx)) 
                (set! rbx 10) 
                (set! r13 (+ r13 rax)) 
                (if (true) (jump L.tmp.31) (jump L.tmp.32)))) 
        (define L.tmp.31 (begin (set! r8 0) (jump L.tmp.33))) 
        (define L.tmp.32 (begin (set! r9 1) (jump L.tmp.33))) 
        (define L.tmp.33 (if (false) (jump L.tmp.22) (jump L.tmp.23))) 
        (define L.tmp.22 (if (true) (jump L.tmp.25) (jump L.tmp.26))) 
        (define L.tmp.23 (if (true) (jump L.tmp.28) (jump L.tmp.29))) 
        (define L.tmp.24 (begin (set! rdi 11) (set! r9 (+ r9 r8)) (jump rax))) 
        (define L.tmp.28 (begin (set! r14 4) (jump L.tmp.30))) 
        (define L.tmp.29 (begin (set! r15 5) (jump L.tmp.30))) 
        (define L.tmp.30 (jump L.tmp.24)) 
        (define L.tmp.25 (begin (set! r12 2) (jump L.tmp.27))) 
        (define L.tmp.26 (begin (set! r13 3) (jump L.tmp.27))) 
        (define L.tmp.27 (jump L.tmp.24)))))

(test-case "expose-basic-blocks 8 - tail is if"
    (check-match
        (expose-basic-blocks
           `(module 
               (if (true) (jump r8) (jump r9)))
        )
    `(module
        (define L.__main.37 (if (true) (jump L.__nested.35) (jump L.__nested.36)))
        (define L.__nested.35 (jump r8))
        (define L.__nested.36 (jump r9)))))

(test-case "expose-basic-blocks 9 - pred is if"
    (check-match
        (expose-basic-blocks
           '(module 
               (if (if (true) (true) (false)) 
                   (jump r8) 
                   (jump r9)))
        )
    `(module
        (define L.__main.42 (if (true) (jump L.tmp.40) (jump L.tmp.41)))
        (define L.tmp.40 (if (true) (jump L.__nested.38) (jump L.__nested.39)))
        (define L.tmp.41 (if (false) (jump L.__nested.38) (jump L.__nested.39)))
        (define L.__nested.38 (jump r8))
        (define L.__nested.39 (jump r9)))))

(test-case "expose-basic-blocks 10 - if true"
    (check-match
        (expose-basic-blocks
            `(module 
                (begin 
                    (set! rax 10)
                    (if (true) (set! r9 9) (set! r8 8))
                    (set! rax 11)
                    (jump rax)))
        )
    `(module
        (define L.__main.46
            (begin (set! rax 10) (if (true) (jump L.tmp.43) (jump L.tmp.44))))
        (define L.tmp.43 (begin (set! r9 9) (jump L.tmp.45)))
        (define L.tmp.44 (begin (set! r8 8) (jump L.tmp.45)))
        (define L.tmp.45 (begin (set! rax 11) (jump rax))))))

(test-case "expose-basic-blocks 11 - if not"
    (check-match
        (expose-basic-blocks
            `(module 
                (begin 
                    (set! rax 10)
                    (if (not (true)) (set! r9 9) (set! r8 8))
                    (set! rax 11)
                    (jump rax)))
        )
    `(module
        (define L.__main.50
            (begin (set! rax 10) (if (true) (jump L.tmp.48) (jump L.tmp.47))))
        (define L.tmp.47 (begin (set! r9 9) (jump L.tmp.49)))
        (define L.tmp.48 (begin (set! r8 8) (jump L.tmp.49)))
        (define L.tmp.49 (begin (set! rax 11) (jump rax))))))

(test-case "expose-basic-blocks 12 - if begin"
    (check-match
        (expose-basic-blocks
            `(module (if (begin (set! r15 0) (= r15 0)) (jump r8) (jump r9)))
        )
    '(module 
        (define L.__main.53 (begin (set! r15 0) (if (= r15 0) (jump L.__nested.51) (jump L.__nested.52)))) 
        (define L.__nested.51 (jump r8)) 
        (define L.__nested.52 (jump r9)))))

(test-case "expose-basic-blocks 13 - multiple nested begins"
    (check-match
        (expose-basic-blocks
            `(module 
                (begin 
                    (begin 
                        (set! r8 1) 
                        (set! r9 2)) 
                    (begin (begin (set! r8 (+ r8 r9)))) 
                    (jump r8)))
        )
    '(module
        (define L.__main.54
            (begin 
                (set! r8 1) 
                (set! r9 2) 
                (set! r8 (+ r8 r9)) 
                (jump r8))))))

(test-case "expose-basic-blocks 14 - define function"
    (check-match
        (expose-basic-blocks
            `(module 
                (define L.start.1 (begin 
                                    (set! rsp 2)
                                    (jump rsp)))
                (begin (set! rax 10) (jump rax)))
        )
    `(module
        (define L.__main.55 (begin (set! rax 10) (jump rax)))
        (define L.start.1 (begin (set! rsp 2) (jump rsp))))))

(test-case "expose-basic-blocks 15 - define functions, tail is jump"
    (check-match
        (expose-basic-blocks
            `(module 
                (define L.start.1 (begin 
                                    (set! rsp 2)
                                    (jump rsp)))
                (define L.start.2 (begin 
                                    (set! rsp 2)
                                    (jump L.start.1)))
                (begin (set! rax 10) (jump L.start.2)))
        )
    `(module
        (define L.__main.56 (begin (set! rax 10) (jump L.start.2)))
        (define L.start.1 (begin (set! rsp 2) (jump rsp)))
        (define L.start.2 (begin (set! rsp 2) (jump L.start.1))))))

(test-case "expose-basic-blocks 16 - branches in functions"
    (check-match
        (expose-basic-blocks
            `(module 
                (define L.start.1 (begin 
                                    (set! rax 10)
                                    (if (not (true)) (set! r9 9) (set! r8 8))
                                    (set! rax 11)
                                    (jump r8)))
                (define L.start.2 (begin 
                                    (set! rsp 2)
                                    (jump L.start.1)))
                (begin (set! rax 10) (jump L.start.2)))
        )
    `(module
        (define L.__main.60 (begin (set! rax 10) (jump L.start.2)))
        (define L.start.1
            (begin (set! rax 10) (if (true) (jump L.tmp.58) (jump L.tmp.57))))
        (define L.tmp.57 (begin (set! r9 9) (jump L.tmp.59)))
        (define L.tmp.58 (begin (set! r8 8) (jump L.tmp.59)))
        (define L.tmp.59 (begin (set! rax 11) (jump r8)))
        (define L.start.2 (begin (set! rsp 2) (jump L.start.1))))))

(test-case "expose-basic-blocks 17 - binop is subtraction"
    (check-match
        (expose-basic-blocks
            `(module 
                (begin 
                    (begin 
                        (set! r8 1) 
                        (set! r9 2)) 
                    (begin (begin (set! r8 (- r8 r9)))) 
                    (jump r8)))
        )
    '(module
        (define L.__main.61
            (begin 
                (set! r8 1) 
                (set! r9 2) 
                (set! r8 (- r8 r9)) 
                (jump r8))))))

(test-case "expose-basic-blocks 18 - simple return points"
    (check-match
        (expose-basic-blocks
            `(module 
                (begin 
                    (begin 
                        (set! r8 1) 
                        (set! r9 2))
                    (return-point L.one.1 (jump L.y.1))
                    (return-point L.two.2 (begin (set! r8 r9) (jump L.y.4)))
                    (return-point L.three.3 (if (false) (jump L.y.5) (jump L.y.6))) 
                    (set! r8 (- r8 r9))
                    (jump r8)))
        )
    '(module
        (define L.__main.64 (begin (set! r8 1) (set! r9 2) (jump L.y.1)))
        (define L.one.1 (begin (set! r8 r9) (jump L.y.4)))
        (define L.two.2 (if (false) (jump L.__nested.62) (jump L.__nested.63)))
        (define L.three.3 (begin (set! r8 (- r8 r9)) (jump r8)))
        (define L.__nested.62 (jump L.y.5))
        (define L.__nested.63 (jump L.y.6)))))

(test-case "expose-basic-blocks 19 - complex test"
    (check-match
        (expose-basic-blocks
            `(module
                (define L.swap.1
                    (begin
                        (set! (rbp - 16) r15)
                        (set! r14 (rbp - 0))
                        (set! r15 (rbp - 8))
                        (if (< r15 r14)
                            (begin (set! rax r14) (jump (rbp - 16)))
                            (begin (begin
                                        (set! rbp (- rbp 24))
                                        (return-point L.rp.1
                                            (begin
                                                (set! (rbp - 8) r14)
                                                (set! (rbp - 0) r15)
                                                (set! r15 L.rp.1)
                                                (jump L.swap.1)))
                                        (set! rbp (+ rbp 24)))
                                    (set! r15 rax)
                                    (set! rax r15)
                                    (jump (rbp - 16))))))
                (begin
                    (set! r15 r15)
                    (set! (rbp - 8) 2)
                    (set! (rbp - 0) 1)
                    (set! r15 r15)
                    (jump L.swap.1)))
        )
    '(module
        (define L.__main.67
            (begin
            (set! r15 r15)
            (set! (rbp - 8) 2)
            (set! (rbp - 0) 1)
            (set! r15 r15)
            (jump L.swap.1)))
        (define L.swap.1
            (begin
            (set! (rbp - 16) r15)
            (set! r14 (rbp - 0))
            (set! r15 (rbp - 8))
            (if (< r15 r14) (jump L.__nested.65) (jump L.__nested.66))))
        (define L.__nested.65 (begin (set! rax r14) (jump (rbp - 16))))
        (define L.__nested.66
            (begin
            (set! rbp (- rbp 24))
            (set! (rbp - 8) r14)
            (set! (rbp - 0) r15)
            (set! r15 L.rp.1)
            (jump L.swap.1)))
        (define L.rp.1
            (begin
            (set! rbp (+ rbp 24))
            (set! r15 rax)
            (set! rax r15)
            (jump (rbp - 16)))))))

(test-case "expose-basic-blocks 18 - nested return points, if inside return-point"
    (check-match
        (expose-basic-blocks
            `(module 
                (begin 
                    (begin 
                        (set! r8 1) 
                        (set! r9 2))
                    (return-point L.two.2 (begin 
                                            (set! r8 r9)
                                            (return-point L.two.2 (begin (set! r8 r9) (jump L.y.4)))
                                            (if (true) (set! r9 9) (set! r8 8))
                                            (if (false) (jump L.y.5) (jump L.y.6))))
                    (set! r8 (- r8 r9))
                    (jump r8)))
        )
    '(module
        (define L.__main.73
            (begin (set! r8 1) (set! r9 2) (set! r8 r9) (set! r8 r9) (jump L.y.4)))
        (define L.two.2 (begin (set! r8 (- r8 r9)) (jump r8)))
        (define L.two.2 (if (true) (jump L.tmp.70) (jump L.tmp.71)))
        (define L.tmp.70 (begin (set! r9 9) (jump L.tmp.72)))
        (define L.tmp.71 (begin (set! r8 8) (jump L.tmp.72)))
        (define L.tmp.72 (if (false) (jump L.__nested.68) (jump L.__nested.69)))
        (define L.__nested.68 (jump L.y.5))
        (define L.__nested.69 (jump L.y.6)))))

; M7 Tests

(test-case "expose-basic-blocks 19 - extend binops"
    (check-match
        (expose-basic-blocks
            `(module 
                (begin 
                    (set! rax 10)
                    (set! r12 (bitwise-ior r12 rdx))
                    (jump r8))))

     `(module
        (define L.__main.74
            (begin (set! rax 10) (set! r12 (bitwise-ior r12 rdx)) (jump r8))))))