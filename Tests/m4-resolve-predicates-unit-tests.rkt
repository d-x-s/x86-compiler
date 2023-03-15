#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt"
)

(test-case "resolve 1"
    (check-equal?
    (resolve-predicates 
    '(module
        (define L.tmp.1 (halt 10))
        (define L.tmp.2 (halt rax))
        (define L.tmp.3 (halt fv1))
        )
    )

    '(module
        (define L.tmp.1 (halt 10))
        (define L.tmp.2 (halt rax))
        (define L.tmp.3 (halt fv1)))
    )
)

(test-case "resolve 2"
    (check-equal?
    (resolve-predicates 
    '(module
        (define L.tmp.4 (jump L.tmp.1))
        (define L.tmp.5 (jump rbx))
        (define L.tmp.6 (jump fv2))
        )
    )

    '(module
        (define L.tmp.4 (jump L.tmp.1))
        (define L.tmp.5 (jump rbx))
        (define L.tmp.6 (jump fv2)))
    )
)

(test-case "resolve 3"
    (check-equal?
    (resolve-predicates 
   '(module
       (define L.tmp.7 (begin (set! rax 10)
                              (set! rbx rcx)
                              (set! rcx fv1)
                              (set! r12 L.tmp.1)
                              (set! fv3 L.tmp.2)
                              (set! rdi (+ rdi 10))
                              (set! rcx (* rcx fv3))
                              (halt 10)
                       )
       )
    )
    )

    '(module
        (define L.tmp.7
            (begin
            (set! rax 10)
            (set! rbx rcx)
            (set! rcx fv1)
            (set! r12 L.tmp.1)
            (set! fv3 L.tmp.2)
            (set! rdi (+ rdi 10))
            (set! rcx (* rcx fv3))
            (halt 10))))
    )
)

(test-case "resolve 4"
    (check-equal?
    (resolve-predicates 
    '(module
        (define L.tmp.7 (begin (set! rax 10)
                                (set! rbx rcx)
                                (set! rcx fv1)
                                (set! r12 L.tmp.1)
                                (set! fv3 L.tmp.2)
                                (set! rdi (+ rdi 10))
                                (set! rcx (* rcx fv3))
                                (begin 
                                    (set! rax 10)
                                    (set! rbx rcx)
                                    (set! rcx fv1)
                                    (set! r12 L.tmp.1)
                                    (set! fv3 L.tmp.2)
                                    (set! rdi (+ rdi 10))
                                    (set! rcx (* rcx fv3))
                                    (halt 10)
                                )
                        )                       
        )
        )
    )

    '(module
        (define L.tmp.7
            (begin
            (set! rax 10)
            (set! rbx rcx)
            (set! rcx fv1)
            (set! r12 L.tmp.1)
            (set! fv3 L.tmp.2)
            (set! rdi (+ rdi 10))
            (set! rcx (* rcx fv3))
            (begin
                (set! rax 10)
                (set! rbx rcx)
                (set! rcx fv1)
                (set! r12 L.tmp.1)
                (set! fv3 L.tmp.2)
                (set! rdi (+ rdi 10))
                (set! rcx (* rcx fv3))
                (halt 10)))))
    )
)

(test-case "resolve 5"
    (check-equal?
    (resolve-predicates 
    '(module
        (define L.tmp.8  (if (> rax 20)       (jump L.tmp.1) (jump L.tmp.2)))
        (define L.tmp.9  (if (true)           (jump rcx)     (jump rdx)))
        (define L.tmp.10 (if (false)          (jump fv1)     (jump fv2)))
        (define L.tmp.11 (if (not (< rbx 10)) (jump L.tmp.3)  (jump rax)))
        (define L.tmp.12 (if (not (true))     (jump L.true.1) (jump L.false.1)))
        (define L.tmp.13 (if (not (false))    (jump L.true.2) (jump L.false.2))) 
        )
    )

    '(module
        (define L.tmp.8 (if (> rax 20) (jump L.tmp.1) (jump L.tmp.2)))
        (define L.tmp.9 (jump rcx))
        (define L.tmp.10 (jump fv2))
        (define L.tmp.11 (if (< rbx 10) (jump rax) (jump L.tmp.3)))
        (define L.tmp.12 (jump L.false.1))
        (define L.tmp.13 (jump L.true.2)))
    )
)