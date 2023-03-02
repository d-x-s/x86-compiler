#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../../compiler.rkt"
)

(test-case "flatten-program 1"
    (check-equal?
    (flatten-program
        '(module
            (define L.tmp.1 (halt 1))
        )
    )

   '(begin 
        (with-label L.tmp.1 (halt 1)))
    )
)

(test-case "flatten-program 2"
    (check-equal?
    (flatten-program
        '(module
            (define L.tmp.1 (halt 1))
            (define L.tmp.2 (halt rax))
        )
    )

   '(begin 
        (with-label L.tmp.1 (halt 1)) 
        (with-label L.tmp.2 (halt rax)))
    )
)

(test-case "flatten-program 3"
    (check-equal?
    (flatten-program
        '(module
            (define L.tmp.1 (halt 1))
            (define L.tmp.2 (halt rax))
            (define L.tmp.3 (halt fv1))

            (define L.tmp.4 (jump L.tmp.5))
            (define L.tmp.6 (jump rbx))
            (define L.tmp.6 (jump fv2))
        )
    )

   '(begin
        (with-label L.tmp.1 (halt 1))
        (with-label L.tmp.2 (halt rax))
        (with-label L.tmp.3 (halt fv1))
        (with-label L.tmp.4 (jump L.tmp.5))
        (with-label L.tmp.6 (jump rbx))
        (with-label L.tmp.6 (jump fv2)))
    )
)

(test-case "flatten-program 4"
    (check-equal?
    (flatten-program
        '(module
            (define L.tmp.1 (halt 1))
            (define L.tmp.2 (halt rax))
            (define L.tmp.3 (halt fv1))

            (define L.tmp.4 (jump L.tmp.5))
            (define L.tmp.6 (jump rbx))
            (define L.tmp.6 (jump fv2))

            (define L.tmp.6 (begin 
                            (set! rax 10) 
                            (halt 10)))
        )
    )

   '(begin
        (with-label L.tmp.1 (halt 1))
        (with-label L.tmp.2 (halt rax))
        (with-label L.tmp.3 (halt fv1))
        (with-label L.tmp.4 (jump L.tmp.5))
        (with-label L.tmp.6 (jump rbx))
        (with-label L.tmp.6 (jump fv2))
        (with-label L.tmp.6 (set! rax 10))
        (halt 10))
    )
)

(test-case "flatten-program 5"
    (check-equal?
    (flatten-program
        '(module
            (define L.tmp.1 (halt 1))
            (define L.tmp.2 (halt rax))
            (define L.tmp.3 (halt fv1))

            (define L.tmp.4 (jump L.tmp.5))
            (define L.tmp.6 (jump rbx))
            (define L.tmp.6 (jump fv2))

            (define L.tmp.9 (begin  (set! rax 10)
                                    (set! fv3 L.tmp.7)
                                    (set! rax rcx)
                                    (set! fv3 fv4)
                                    (set! rdx (+ rdx 10))
                                    (set! r12 (+ r12 20))
                                    (set! r13 (* r13 r14))
                                    (set! r15 (* r15 fv5))
                                    (halt 10)))
        )
    )

   '(begin
        (with-label L.tmp.1 (halt 1))
        (with-label L.tmp.2 (halt rax))
        (with-label L.tmp.3 (halt fv1))
        (with-label L.tmp.4 (jump L.tmp.5))
        (with-label L.tmp.6 (jump rbx))
        (with-label L.tmp.6 (jump fv2))
        (with-label L.tmp.9 (set! rax 10))
        (set! fv3 L.tmp.7)
        (set! rax rcx)
        (set! fv3 fv4)
        (set! rdx (+ rdx 10))
        (set! r12 (+ r12 20))
        (set! r13 (* r13 r14))
        (set! r15 (* r15 fv5))
        (halt 10))
    )
)

(test-case "flatten-program 6"
    (check-equal?
    (flatten-program
        '(module
            (define L.tmp.10 (if (< rax 10) 
                                (jump rbx) 
                                (jump L.tmp.1)))
        )
    )

   '(begin 
        (with-label L.tmp.10 (compare rax 10)) 
        (jump-if < rbx) 
        (jump L.tmp.1))
    )
)

(test-case "flatten-program 7"
    (check-equal?
    (flatten-program
    '(module
        (define L.tmp.6 (begin 
                            (set! rax 10) 
                            (begin 
                                (set! rax 11) 
                                (halt 11)
                            )
                        )
        )
    )
    )

   '(begin 
        (with-label L.tmp.6 (set! rax 10)) 
        (set! rax 11) 
        (halt 11))
    )
)