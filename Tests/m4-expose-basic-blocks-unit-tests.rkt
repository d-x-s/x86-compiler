#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt"
)

(test-case "expose-basic-blocks 1"
    (check-match
        (expose-basic-blocks
            `(module (halt 10))
        )

     `(module (define L.__main.1 (halt 10)))))

(test-case "expose-basic-blocks 2"
    (check-match
        (expose-basic-blocks
            `(module 
                (begin 
                    (set! rax 10)
                    (halt 10)))
        )

     `(module (define L.__main.2 (begin (set! rax 10) (halt 10))))))

(test-case "expose-basic-blocks 3"
    (check-match
        (expose-basic-blocks
            `(module 
                (begin 
                    (set! rax 10)
                    (set! r12 (* r12 rdx))
                    (halt 10)
                )
            )
        )

     `(module (define L.__main.3 (begin (set! rax 10) (set! r12 (* r12 rdx)) (halt 10))))))

(test-case "expose-basic-blocks 4"
    (check-match
        (expose-basic-blocks
            `(module 
                (begin 
                    (set! rax 10)
                    (set! r12 (* r12 rdx))
                    (begin (set! rbx 10) 
                           (set! r13 (+ r13 rax)))
                    (halt 10)
                )
            )
        )

     `(module
        (define L.__main.4
            (begin
            (set! rax 10)
            (set! r12 (* r12 rdx))
            (set! rbx 10)
            (set! r13 (+ r13 rax))
            (halt 10))))))

(test-case "expose-basic-blocks 5"
    (check-match
        (expose-basic-blocks
            `(module 
                (begin 
                    (set! rax 10)
                    (set! r12 (* r12 rdx))
                    (begin (set! rbx 10) 
                            (set! r13 (+ r13 rax))
                            (if (true) (set! fv0 2) (set! fv1 3)))
                    (halt 10)
                )
            )
        )

     `(module
        (define L.__main.8
            (begin
                (set! rax 10)
                (set! r12 (* r12 rdx))
                (set! rbx 10)
                (set! r13 (+ r13 rax))
                (if (true) (jump L.tmp.5) (jump L.tmp.6))))
        (define L.tmp.5 (begin (set! fv0 2) (jump L.tmp.7)))
        (define L.tmp.6 (begin (set! fv1 3) (jump L.tmp.7)))
        (define L.tmp.7 (halt 10)))))

(test-case "expose-basic-blocks 6"
    (check-match
        (expose-basic-blocks
            `(module 
                (begin 
                    (set! rax 10)
                    (set! r12 (* r12 rdx))
                    (begin (set! rbx 10) 
                            (set! r13 (+ r13 rax))
                            (if (true) (set! fv0 0) (set! fv1 1))
                            (if (false) 
                                (if (true) (set! fv2 2) (set! fv3 3))
                                (if (true) (set! fv4 4) (set! fv5 5))))
                    (halt 10)
                )
            )
        )

    '(module 
        (define L.__main.21 
            (begin (set! rax 10) (set! r12 (* r12 rdx)) (set! rbx 10) (set! r13 (+ r13 rax)) 
                (if (true) (jump L.tmp.18) (jump L.tmp.19)))) 
        (define L.tmp.18 (begin (set! fv0 0) (jump L.tmp.20))) 
        (define L.tmp.19 (begin (set! fv1 1) (jump L.tmp.20))) 
        (define L.tmp.20 (if (false) (jump L.tmp.9) (jump L.tmp.10))) 
        (define L.tmp.9 (if (true) (jump L.tmp.12) (jump L.tmp.13))) 
        (define L.tmp.10 (if (true) (jump L.tmp.15) (jump L.tmp.16))) 
        (define L.tmp.11 (halt 10)) 
        (define L.tmp.15 (begin (set! fv4 4) (jump L.tmp.17))) 
        (define L.tmp.16 (begin (set! fv5 5) (jump L.tmp.17))) 
        (define L.tmp.17 (jump L.tmp.11)) 
        (define L.tmp.12 (begin (set! fv2 2) (jump L.tmp.14))) 
        (define L.tmp.13 (begin (set! fv3 3) (jump L.tmp.14))) 
        (define L.tmp.14 (jump L.tmp.11)))))

(test-case "expose-basic-blocks 7"
    (check-match
        (expose-basic-blocks
            '(module 
                (begin 
                    (set! rax 10)
                    (set! r12 (* r12 rdx))
                    (begin (set! rbx 10) 
                            (set! r13 (+ r13 rax))
                            (if (true) (set! fv0 0) (set! fv1 1))
                            (if (false) 
                                (if (true) (set! fv2 2) (set! fv3 3))
                                (if (true) (set! fv4 4) (set! fv5 5))))
                    (set! rdi 11)
                    (set! r9 (+ r9 r8))
                    (halt 10)
                )
            )
        )
    '(module 
        (define L.__main.34 
            (begin 
                (set! rax 10) 
                (set! r12 (* r12 rdx)) 
                (set! rbx 10) 
                (set! r13 (+ r13 rax)) 
                (if (true) (jump L.tmp.31) (jump L.tmp.32)))) 
        (define L.tmp.31 (begin (set! fv0 0) (jump L.tmp.33))) 
        (define L.tmp.32 (begin (set! fv1 1) (jump L.tmp.33))) 
        (define L.tmp.33 (if (false) (jump L.tmp.22) (jump L.tmp.23))) 
        (define L.tmp.22 (if (true) (jump L.tmp.25) (jump L.tmp.26))) 
        (define L.tmp.23 (if (true) (jump L.tmp.28) (jump L.tmp.29))) 
        (define L.tmp.24 (begin (set! rdi 11) (set! r9 (+ r9 r8)) (halt 10))) 
        (define L.tmp.28 (begin (set! fv4 4) (jump L.tmp.30))) 
        (define L.tmp.29 (begin (set! fv5 5) (jump L.tmp.30))) 
        (define L.tmp.30 (jump L.tmp.24)) 
        (define L.tmp.25 (begin (set! fv2 2) (jump L.tmp.27))) 
        (define L.tmp.26 (begin (set! fv3 3) (jump L.tmp.27))) 
        (define L.tmp.27 (jump L.tmp.24)))))

(test-case "expose-basic-blocks 8"
    (check-match
        (expose-basic-blocks
           `(module 
               (if (true) (halt 1) (halt 2))
            )
        )
    `(module
        (define L.__main.37 (if (true) (jump L.__nested.35) (jump L.__nested.36)))
        (define L.__nested.35 (halt 1))
        (define L.__nested.36 (halt 2)))))

(test-case "expose-basic-blocks 9"
    (check-match
        (expose-basic-blocks
           '(module 
               (if (if (true) (true) (false)) 
                   (halt 1) 
                   (halt 2))
            )
        )
    `(module
        (define L.__main.42 (if (true) (jump L.tmp.40) (jump L.tmp.41)))
        (define L.tmp.40 (if (true) (jump L.__nested.38) (jump L.__nested.39)))
        (define L.tmp.41 (if (false) (jump L.__nested.38) (jump L.__nested.39)))
        (define L.__nested.38 (halt 1))
        (define L.__nested.39 (halt 2)))))

(test-case "expose-basic-blocks 10"
    (check-match
        (expose-basic-blocks
            `(module 
                (begin 
                    (set! rax 10)
                    (if (true) (set! r9 9) (set! r8 8))
                    (set! rax 11)
                    (halt 10) 
                )
            )
        )
    `(module
        (define L.__main.46
            (begin (set! rax 10) (if (true) (jump L.tmp.43) (jump L.tmp.44))))
        (define L.tmp.43 (begin (set! r9 9) (jump L.tmp.45)))
        (define L.tmp.44 (begin (set! r8 8) (jump L.tmp.45)))
        (define L.tmp.45 (begin (set! rax 11) (halt 10))))))

(test-case "expose-basic-blocks 11"
    (check-match
        (expose-basic-blocks
            `(module 
                (begin 
                    (set! rax 10)
                    (if (not (true)) (set! r9 9) (set! r8 8))
                    (set! rax 11)
                    (halt 10) 
                )
            )
        )
    `(module
        (define L.__main.50
            (begin (set! rax 10) (if (true) (jump L.tmp.48) (jump L.tmp.47))))
        (define L.tmp.47 (begin (set! r9 9) (jump L.tmp.49)))
        (define L.tmp.48 (begin (set! r8 8) (jump L.tmp.49)))
        (define L.tmp.49 (begin (set! rax 11) (halt 10))))))

(test-case "expose-basic-blocks 12"
    (check-match
        (expose-basic-blocks
            `(module (if (begin (set! r15 0) (= r15 0)) (halt 0) (halt 1)))
        )
    '(module 
        (define L.__main.53 (begin (set! r15 0) (if (= r15 0) (jump L.__nested.51) (jump L.__nested.52)))) 
        (define L.__nested.51 (halt 0)) 
        (define L.__nested.52 (halt 1)))))

(test-case "expose-basic-blocks 13"
    (check-match
        (expose-basic-blocks
            `(module 
                (begin 
                    (begin 
                        (set! fv0 1) 
                        (set! fv1 2)) 
                    (begin (begin (set! fv0 (+ fv0 fv1)))) 
                    (halt fv0)))
        )
    '(module
        (define L.__main.54
            (begin 
                (set! fv0 1) 
                (set! fv1 2) 
                (set! fv0 (+ fv0 fv1)) 
                (halt fv0))))))



