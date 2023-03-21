#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt")

(test-case "select 1 - tail is number"
   (check-equal?
        (select-instructions
            `(module 2))
        
        `(module () (halt 2))))

(test-case "select 2 - tail is binop"
   (check-equal?
        (select-instructions
            `(module (+ 1 x.1)))
        
        `(module () (begin (set! tmp.2 1) (set! tmp.2 (+ tmp.2 x.1)) (halt tmp.2)))))

(test-case "select 3 - nested begins"
   (check-equal?
        (select-instructions
            `(module (begin 
                        (set! x.1 2)
                        (set! x.2 x.3)
                        (begin 
                            (set! z.1 2)
                            (set! z.1 z.4))
                        (set! x.3 (+ y.1 y.2))
                        2)))
        
        `(module
            ()
            (begin
                (set! x.1 2)
                (set! x.2 x.3)
                (set! z.1 2)
                (set! z.1 z.4)
                (set! x.3 y.1)
                (set! x.3 (+ x.3 y.2))
                (halt 2)))))

(test-case "select 4 - nested begins, tail is begin"
   (check-equal?
        (select-instructions
            `(module (begin 
                        (set! x.1 2)
                        (set! x.2 x.3)
                        (begin 
                            (set! z.1 2)
                            (set! z.1 z.4))
                        (set! x.3 (+ y.1 y.2))
                        (begin 
                            (set! z.1 2)
                            x.2))))
        
        `(module
            ()
            (begin
                (set! x.1 2)
                (set! x.2 x.3)
                (set! z.1 2)
                (set! z.1 z.4)
                (set! x.3 y.1)
                (set! x.3 (+ x.3 y.2))
                (set! z.1 2)
                (halt x.2)))))

; M4 Tests

(test-case "select 5 - tail is if"
   (check-equal?
        (select-instructions
            `(module (if (true) 0 (+ 0 1))))
        
        `(module ()
            (if (true)
                (halt 0)
                (begin (set! tmp.4 0) (set! tmp.4 (+ tmp.4 1)) (halt tmp.4))))))

(test-case "select 6 - pred is number comparison"
   (check-equal?
        (select-instructions
            `(module (if (> 0 1) 0 (+ 0 1))))
        
        `(module
            ()
            (if (begin (set! tmp.8 0) (> tmp.8 1))
                (halt 0)
                (begin (set! tmp.7 0) (set! tmp.7 (+ tmp.7 1)) (halt tmp.7))))))

(test-case "select 7 - pred is not"
   (check-equal?
        (select-instructions
            `(module (if (not (> 0 1)) 0 (+ 0 1))))
        
        `(module ()
            (if (not (begin (set! tmp.12 0) (> tmp.12 1)))
                (halt 0)
                (begin (set! tmp.11 0) (set! tmp.11 (+ tmp.11 1)) (halt tmp.11))))))

; Note: this test differs from interrogator result in that it flattens begins in (begin effects ... pred).
(test-case "select 8 - pred is begin"
   (check-equal?
        (select-instructions
            `(module (if (begin 
                            (set! x.1 2) 
                            (set! x.2 (+ 1 2)) 
                            (begin (set! y.2 2)) 
                            (true)) 
                         0 
                         (+ 0 1))))
        
        `(module
            ()
            (if (begin
                    (set! x.1 2)
                    (set! x.2 1)
                    (set! x.2 (+ x.2 2))
                    (set! y.2 2)
                    (true))
                (halt 0)
                (begin (set! tmp.14 0) (set! tmp.14 (+ tmp.14 1)) (halt tmp.14))))))

(test-case "select 9 - pred is if"
   (check-equal?
        (select-instructions
            `(module (if (if (> 1 2) (true) (false)) 
                         0 
                         (+ 0 1))))
        
        `(module ()
            (if (if (begin (set! tmp.18 1) (> tmp.18 2)) (true) (false))
                (halt 0)
                (begin (set! tmp.17 0) (set! tmp.17 (+ tmp.17 1)) (halt tmp.17))))))

(test-case "select 10 - effect is if"
   (check-equal?
        (select-instructions
            `(module (begin 
                        (set! x.1 2)
                        (if (true) (set! x.2 (+ 1 2)) (set! x.3 3))
                        2)))
        
        `(module ()
            (begin
                (set! x.1 2)
                (if (true) (begin (set! x.2 1) (set! x.2 (+ x.2 2))) (set! x.3 3))
                (halt 2)))))

(test-case "select 11 - effect if has begins as branches"
   (check-equal?
        (select-instructions
            `(module 
                (begin 
                    (set! x.1 5) 
                    (if (true) 
                        (begin (set! y.2 (+ x.1 17)) (set! x.5 12)) 
                        (begin (set! x.5 15))) 
                    x.5)))
        
        `(module
            ()
            (begin
                (set! x.1 5)
                (if (true)
                    (begin (set! y.2 x.1) (set! y.2 (+ y.2 17)) (set! x.5 12))
                    (begin (set! x.5 15)))
                (halt x.5)))))

; M5 Tests

(test-case "select 12 - define functions, jump as tail"
   (check-equal?
        (select-instructions
            `(module 
                (define L.id.5 (begin (set! x.32 rdi) x.32)) 
                (begin (set! y.33 L.id.5) (begin (set! rdi 5) (jump y.33 rbp rdi)))))
        
        `(module
            ()
            (define L.id.5 () (begin (set! x.32 rdi) (halt x.32)))
            (begin (set! y.33 L.id.5) (set! rdi 5) (jump y.33 rbp rdi)))))

(test-case "select 13 - define functions, binop"
   (check-equal?
        (select-instructions
            `(module 
                (define L.zero.1 (begin 
                                    (set! v0.5 rdi) 
                                    (set! v1.6 rsi) 
                                    (set! v2.7 rdx) 
                                    (set! v3.8 rcx) 
                                    (+ 1 2))) 
                0))
        
        `(module
            ()
            (define L.zero.1
                ()
                (begin
                (set! v0.5 rdi)
                (set! v1.6 rsi)
                (set! v2.7 rdx)
                (set! v3.8 rcx)
                (set! tmp.20 1)
                (set! tmp.20 (+ tmp.20 2))
                (halt tmp.20)))
            (halt 0))))

(test-case "select 14 - nested begins"
   (check-equal?
        (select-instructions
            `(module 
                (define L.id1.2 (begin (set! x.10 rdi) x.10)) 
                (define L.id2.3 (begin (set! x.11 rdi) x.11)) 
                (begin 
                    (if (true) (set! y.12 L.id1.2) (set! y.12 L.id2.3)) 
                    (begin (set! rdi 5) (jump y.12 rbp rdi)))))
        
        `(module
            ()
            (define L.id1.2 () (begin (set! x.10 rdi) (halt x.10)))
            (define L.id2.3 () (begin (set! x.11 rdi) (halt x.11)))
            (begin
                (if (true) (set! y.12 L.id1.2) (set! y.12 L.id2.3))
                (set! rdi 5)
                (jump y.12 rbp rdi)))))

(test-case "select 15 - tail is aloc"
   (check-equal?
        (select-instructions
            `(module 
                (define L.id.4 (begin (set! x.31 rdi) x.31)) 
                (begin (set! rdi 5) (jump L.id.4 rbp rdi))))
        
        `(module
            ()
            (define L.id.4 () (begin (set! x.31 rdi) (halt x.31)))
            (begin (set! rdi 5) (jump L.id.4 rbp rdi)))))

(test-case "select 16 - complex test"
   (check-equal?
        (select-instructions
            `(module 
                (define L.odd?.6 (begin 
                                    (set! x.58 rdi) 
                                    (if (= x.58 0) 
                                        0 
                                        (begin 
                                            (set! y.59 (+ x.58 -1)) 
                                            (begin (set! rdi y.59) (jump L.even?.7 rbp rdi)))))) 
                (define L.even?.7 (begin 
                                    (set! x.60 rdi) 
                                    (if (= x.60 0) 
                                        1 
                                        (begin (set! y.61 (+ x.60 -1)) (begin (set! rdi y.61) (jump L.odd?.6 rbp rdi)))))) 
                (begin (set! rdi 5) (jump L.even?.7 rbp rdi))))
        
        `(module
            ()
            (define L.odd?.6
                ()
                (begin
                (set! x.58 rdi)
                (if (= x.58 0)
                    (halt 0)
                    (begin
                    (set! y.59 x.58)
                    (set! y.59 (+ y.59 -1))
                    (set! rdi y.59)
                    (jump L.even?.7 rbp rdi)))))
            (define L.even?.7
                ()
                (begin
                (set! x.60 rdi)
                (if (= x.60 0)
                    (halt 1)
                    (begin
                    (set! y.61 x.60)
                    (set! y.61 (+ y.61 -1))
                    (set! rdi y.61)
                    (jump L.odd?.6 rbp rdi)))))
            (begin (set! rdi 5) (jump L.even?.7 rbp rdi)))))
