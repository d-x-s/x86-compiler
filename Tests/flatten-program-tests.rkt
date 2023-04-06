#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt")

; M7 Tests

(test-case "flatten-program 1 - jump"
    (check-equal?
        (flatten-program
            '(module
                (define L.tmp.4 (jump L.tmp.5))
                (define L.tmp.6 (jump rbx))
                (define L.tmp.7 (jump (rbp - 0)))))

        '(begin
            (with-label L.tmp.4 (jump L.tmp.5))
            (with-label L.tmp.6 (jump rbx))
            (with-label L.tmp.7 (jump (rbp - 0))))))

(test-case "flatten-program 2 - begin effects"
    (check-equal?
        (flatten-program
            '(module
                (define L.tmp.9 (begin  (set! rax 10)
                                        (set! (rbp - 8) L.tmp.7)
                                        (set! rax rcx)
                                        (set! rdx (+ rdx 10))
                                        (set! r13 (bitwise-xor r13 (rbp - 8)))
                                        (jump L.start.1)))))

        '(begin
            (with-label L.tmp.9 (set! rax 10))
            (set! (rbp - 8) L.tmp.7)
            (set! rax rcx)
            (set! rdx (+ rdx 10))
            (set! r13 (bitwise-xor r13 (rbp - 8)))
            (jump L.start.1))))

(test-case "flatten-program 3 - if"
    (check-equal?
        (flatten-program
            '(module
                (define L.tmp.10 (if (< rax 10) 
                                     (jump rbx) 
                                     (jump L.tmp.1)))
                (define L.tmp.11 (if (!= rax 10) 
                                     (jump rbx) 
                                     (jump L.tmp.1)))))

        '(begin
            (with-label L.tmp.10 (compare rax 10))
            (jump-if < rbx)
            (jump L.tmp.1)
            (with-label L.tmp.11 (compare rax 10))
            (jump-if != rbx)
            (jump L.tmp.1))))

(test-case "flatten-program 4 - nested tail"
    (check-equal?
        (flatten-program
            '(module
                (define L.tmp.6 
                    (begin 
                        (set! rax 10) 
                        (begin 
                            (set! rax 11) 
                            (jump L.start.1))))))

        '(begin 
            (with-label L.tmp.6 (set! rax 10)) 
            (set! rax 11) 
            (jump L.start.1))))