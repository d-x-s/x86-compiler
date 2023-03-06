#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../../compiler.rkt"
)

(test-case "expose-basic-blocks 1"
    (check-match
        (expose-basic-blocks
            '(module 
                (halt 10))
        )

     `(module (define L.__main.1 (halt 10)))
    )
)

(test-case "expose-basic-blocks 2"
    (check-match
        (expose-basic-blocks
            '(module 
                (begin 
                    (set! rax 10)
                    (halt 10)
                )
            )
        )

     `(module (define L.__main.1 (begin (set! rax 10) (halt 10))))
    )
)

(test-case "expose-basic-blocks 3"
    (check-match
        (expose-basic-blocks
            '(module 
                (begin 
                    (set! rax 10)
                    (set! r12 (* r12 rdx))
                    (halt 10)
                )
            )
        )

     `(module (define L.__main.1 (begin (set! rax 10) (set! r12 (* r12 rdx)) (halt 10))))
    )
)

(test-case "expose-basic-blocks 4"
    (check-match
        (expose-basic-blocks
            '(module 
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
        (define L.__main.1
            (begin
            (set! rax 10)
            (set! r12 (* r12 rdx))
            (set! rbx 10)
            (set! r13 (+ r13 rax))
            (halt 10))))
    )
)

(test-case "expose-basic-blocks 4"
    (check-match
        (expose-basic-blocks
            '(module 
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
        (define L.__main.4
            (begin
            (set! rax 10)
            (set! r12 (* r12 rdx))
            (set! rbx 10)
            (set! r13 (+ r13 rax))
            (if (true) (jump L.tmp.1) (jump L.tmp.2))))
        (define L.tmp.1 (begin (set! fv0 2) (jump L.tmp.3)))
        (define L.tmp.2 (begin (set! fv1 3) (jump L.tmp.3)))
        (define L.tmp.3 (halt 10)))
    )
)

(test-case "expose-basic-blocks 5"
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
                    (halt 10)
                )
            )
        )
    `(module
        (define L.__main.13
            (begin
            (set! rax 10)
            (set! r12 (* r12 rdx))
            (set! rbx 10)
            (set! r13 (+ r13 rax))
            (if (true) (jump L.tmp.10) (jump L.tmp.11))))
        (define L.tmp.10 (begin (set! fv0 0) (jump L.tmp.12)))
        (define L.tmp.11 (begin (set! fv1 1) (jump L.tmp.12)))
        (define L.tmp.12 (if (false) (jump L.tmp.1) (jump L.tmp.2)))
        (define L.tmp.4 (begin (set! fv2 2) (jump L.tmp.6)))
        (define L.tmp.5 (begin (set! fv3 3) (jump L.tmp.6)))
        (define L.tmp.6 (jump L.tmp.3))
        (define L.tmp.7 (begin (set! fv4 4) (jump L.tmp.9)))
        (define L.tmp.8 (begin (set! fv5 5) (jump L.tmp.9)))
        (define L.tmp.9 (jump L.tmp.3))
        (define L.tmp.1 (if (true) (jump L.tmp.4) (jump L.tmp.5)))
        (define L.tmp.2 (if (true) (jump L.tmp.7) (jump L.tmp.8)))
        (define L.tmp.3 (halt 10)))
    )
)

(test-case "expose-basic-blocks 6"
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
    `(module
        (define L.__main.13
            (begin
            (set! rax 10)
            (set! r12 (* r12 rdx))
            (set! rbx 10)
            (set! r13 (+ r13 rax))
            (if (true) (jump L.tmp.10) (jump L.tmp.11))))
        (define L.tmp.10 (begin (set! fv0 0) (jump L.tmp.12)))
        (define L.tmp.11 (begin (set! fv1 1) (jump L.tmp.12)))
        (define L.tmp.12 (if (false) (jump L.tmp.1) (jump L.tmp.2)))
        (define L.tmp.4 (begin (set! fv2 2) (jump L.tmp.6)))
        (define L.tmp.5 (begin (set! fv3 3) (jump L.tmp.6)))
        (define L.tmp.6 (jump L.tmp.3))
        (define L.tmp.7 (begin (set! fv4 4) (jump L.tmp.9)))
        (define L.tmp.8 (begin (set! fv5 5) (jump L.tmp.9)))
        (define L.tmp.9 (jump L.tmp.3))
        (define L.tmp.1 (if (true) (jump L.tmp.4) (jump L.tmp.5)))
        (define L.tmp.2 (if (true) (jump L.tmp.7) (jump L.tmp.8)))
        (define L.tmp.3 (begin (set! rdi 11) (set! r9 (+ r9 r8)) (halt 10))))
    )
)

(test-case "expose-basic-blocks 7"
    (check-match
        (expose-basic-blocks
           '(module 
               (if (true) (halt 1) (halt 2))
            )
        )
    `(module
        (define L.__main.3 (if (true) (jump L.__nested.1) (jump L.__nested.2)))
        (define L.__nested.1 (halt 1))
        (define L.__nested.2 (halt 2)))
    )
)

(test-case "expose-basic-blocks 8"
    (check-match
        (expose-basic-blocks
            '(module 
                (begin 
                    (set! rax 10)
                    (if (true) (set! r9 9) (set! r8 8))
                    (set! rax 11)
                    (halt 10) 
                )
            )
        )
    `(module
        (define L.__main.4
            (begin (set! rax 10) (if (true) (jump L.tmp.1) (jump L.tmp.2))))
        (define L.tmp.1 (begin (set! r9 9) (jump L.tmp.3)))
        (define L.tmp.2 (begin (set! r8 8) (jump L.tmp.3)))
        (define L.tmp.3 (begin (set! rax 11) (halt 10))))
    )
)

(test-case "expose-basic-blocks 9"
    (check-match
        (expose-basic-blocks
        '(module 
            (begin 
                (set! rax 10)
                (if (not (true)) (set! r9 9) (set! r8 8))
                (set! rax 11)
                (halt 10) 
            )
            )
        )
    `(module
        (define L.__main.4
            (begin (set! rax 10) (if (true) (jump L.tmp.2) (jump L.tmp.1))))
        (define L.tmp.1 (begin (set! r9 9) (jump L.tmp.3)))
        (define L.tmp.2 (begin (set! r8 8) (jump L.tmp.3)))
        (define L.tmp.3 (begin (set! rax 11) (halt 10))))
    )
)




