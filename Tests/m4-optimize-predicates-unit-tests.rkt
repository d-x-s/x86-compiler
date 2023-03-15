#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt"
)

(test-case "optimize-predicates 1"
    (check-match
        (optimize-predicates
            `(module 
                (begin 
                    (set! r14 5) 
                    (if (true) 
                        (begin (set! r15 r14) (set! r15 (+ r15 17)) (set! r15 12)) 
                        (begin (set! r15 15))) 
                    (halt r15)))
        )

     `(module
        (begin
            (set! r14 5)
            (begin (set! r15 r14) (set! r15 (+ r15 17)) (set! r15 12))
            (halt r15)))))

(test-case "optimize-predicates 2"
    (check-match
        (optimize-predicates
            `(module 
                (begin 
                    (set! r8 0) 
                    (set! r9 0) 
                    (if (not (if (true) (> r8 5) (< r9 6))) 
                        (set! r12 15) 
                        (set! r12 90)) 
                    (halt r12)))
        )

     `(module (begin (set! r8 0) (set! r9 0) (set! r12 15) (halt r12)))))

(test-case "optimize-predicates 3"
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
                            (halt r15) 
                            (begin (set! r15 r15) (halt r15))))))
        )

     `(module
        (begin
            (set! r15 5)
            (set! r15 r15)
            (begin 
                (set! r14 r15)
                (set! r14 (+ r14 r15)) 
                (set! r15 r14) 
                (halt 10))))))

(test-case "optimize-predicates 4"
    (check-match
        (optimize-predicates
            `(module 
                (begin 
                    (set! r14 0) 
                    (if (true) 
                        (begin (set! r15 r14) (set! r15 (+ r15 17)) (set! r15 12)) 
                        (begin (set! r15 15))) (halt r15)))
        )

     `(module
        (begin
            (set! r14 0)
            (begin (set! r15 r14) (set! r15 (+ r15 17)) (set! r15 12))
            (halt r15)))))

(test-case "optimize-predicates 5"
    (check-match
        (optimize-predicates
            `(module 
                (begin 
                    (set! r8 12) 
                    (set! r9 12) 
                    (if (not (true)) 
                        (set! r12 15) 
                        (set! r13 90))
                    (halt r8)))
        )

     `(module (begin (set! r8 12) (set! r9 12) (set! r13 90) (halt 12)))))

(test-case "optimize-predicates 6"
    (check-match
        (optimize-predicates
            `(module 
                (begin 
                    (if (true) 
                        (begin (set! r15 14) (set! r15 12)) 
                        (begin (set! r15 15))) 
                    (halt r15)))
        )

     `(module (begin (begin (set! r15 14) (set! r15 12)) (halt r15)))))

