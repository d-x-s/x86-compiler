#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt")

 (test-case "sequentialize 0 - stack smash"
   (check-equal?
    (sequentialize-let
    '(module
    (define L.+.5
        (lambda (tmp.16 tmp.17)
        (if (let ((tmp.1
                    (if (let ((tmp.2 (bitwise-and tmp.17 7))) (= tmp.2 0)) 14 6)))
                (!= tmp.1 6))
            (if (let ((tmp.3
                    (if (let ((tmp.4 (bitwise-and tmp.16 7))) (= tmp.4 0))
                        14
                        6)))
                (!= tmp.3 6))
            (+ tmp.16 tmp.17)
            574)
            574)))
    (define L.F.4
        (lambda (a.7 b.6 c.5 d.4 e.3 f.2 g.1)
        (let ((tmp.5 (call L.G.5 a.7 b.6 c.5 d.4 e.3 f.2 g.1 64)))
            (call L.+.5 80 tmp.5))))
    (define L.G.5
        (lambda (a.15 b.14 c.13 d.12 e.11 f.10 g.9 h.8)
        (call L.H.6 a.15 b.14 c.13 d.12 e.11 f.10 g.9 h.8 72)))
    (define L.H.6
        (lambda (a.24 b.23 c.22 d.21 e.20 f.19 g.18 h.17 j.16)
        (let ((r1.25 (call L.+.5 a.24 b.23)))
            (let ((r2.26 (call L.+.5 r1.25 c.22)))
            (let ((r3.27 (call L.+.5 r2.26 d.21)))
                (let ((r4.28 (call L.+.5 r3.27 e.20)))
                (let ((r5.29 (call L.+.5 r4.28 f.19)))
                    (let ((r6.30 (call L.+.5 r5.29 g.18)))
                    (let ((r7.31 (call L.+.5 r6.30 h.17)))
                        (call L.+.5 r7.31 j.16))))))))))
    (call L.F.4 8 16 24 32 40 48 56)))
        
        '(module
            (define L.+.5
                (lambda (tmp.16 tmp.17)
                (if (begin
                        (set! tmp.1
                        (if (begin (set! tmp.2 (bitwise-and tmp.17 7)) (= tmp.2 0))
                            14
                            6))
                        (!= tmp.1 6))
                    (if (begin
                        (set! tmp.3
                            (if (begin (set! tmp.4 (bitwise-and tmp.16 7)) (= tmp.4 0))
                            14
                            6))
                        (!= tmp.3 6))
                    (+ tmp.16 tmp.17)
                    574)
                    574)))
            (define L.F.4
                (lambda (a.7 b.6 c.5 d.4 e.3 f.2 g.1)
                (begin
                    (set! tmp.5 (call L.G.5 a.7 b.6 c.5 d.4 e.3 f.2 g.1 64))
                    (call L.+.5 80 tmp.5))))
            (define L.G.5
                (lambda (a.15 b.14 c.13 d.12 e.11 f.10 g.9 h.8)
                (call L.H.6 a.15 b.14 c.13 d.12 e.11 f.10 g.9 h.8 72)))
            (define L.H.6
                (lambda (a.24 b.23 c.22 d.21 e.20 f.19 g.18 h.17 j.16)
                (begin
                    (set! r1.25 (call L.+.5 a.24 b.23))
                    (begin
                    (set! r2.26 (call L.+.5 r1.25 c.22))
                    (begin
                        (set! r3.27 (call L.+.5 r2.26 d.21))
                        (begin
                        (set! r4.28 (call L.+.5 r3.27 e.20))
                        (begin
                            (set! r5.29 (call L.+.5 r4.28 f.19))
                            (begin
                            (set! r6.30 (call L.+.5 r5.29 g.18))
                            (begin
                                (set! r7.31 (call L.+.5 r6.30 h.17))
                                (call L.+.5 r7.31 j.16))))))))))
            (call L.F.4 8 16 24 32 40 48 56))))

(test-case "sequentialize 1 - tail is number"
   (check-equal?
        (sequentialize-let
            `(module 6))
        
        `(module 6)))

(test-case "sequentialize 2 - tail is binop"
   (check-equal?
        (sequentialize-let
            `(module (* 2 3)))
        
        `(module (* 2 3))))

(test-case "sequentialize 3 - empty let"
   (check-equal?
        (sequentialize-let
            `(module (let () 2)))
        
        `(module (begin 2))))

(test-case "sequentialize 4 - simple let"
   (check-equal?
        (sequentialize-let
            `(module (let ([x.1 1] [x.2 2]) (+ x.1 x.2))))
        
        `(module (begin (set! x.1 1) (set! x.2 2) (+ x.1 x.2)))))

(test-case "sequentialize 5 - nested let"
   (check-equal?
        (sequentialize-let
            `(module (let ([x.1 1] [x.2 2]) 
                          (let ([x.1 0] [x.2 1]) 2))))
        
        `(module (begin (set! x.1 1) (set! x.2 2) (begin (set! x.1 0) (set! x.2 1) 2)))))

(test-case "sequentialize 6 - binop, aloc as let value"
   (check-equal?
        (sequentialize-let
            `(module (let ([x.1 (+ 0 2)] [x.2 x.1]) 
                          (let ([x.1 0] [x.2 1]) 2))))
        
        `(module (begin (set! x.1 (+ 0 2)) (set! x.2 x.1) (begin (set! x.1 0) (set! x.2 1) 2)))))

(test-case "sequentialize 7 - let as let value"
   (check-equal?
        (sequentialize-let
            `(module (let ([x.3 (let () 2)]) 
                          (let ([x.1 (let ([y.1 1] [y.2 2]) y.2)] 
                                [x.2 1]) 
                                2))))
        
        `(module (begin (set! x.3 (begin 2)) (begin (set! x.1 (begin (set! y.1 1) (set! y.2 2) y.2)) (set! x.2 1) 2)))))


; M4 Tests

(test-case "sequentialize 8 - tail is if"
   (check-equal?
        (sequentialize-let
            `(module (if (= 0 0) 0 1)))
        
        `(module (if (= 0 0) 0 1))))

(test-case "sequentialize 9 - simple pred"
   (check-equal?
        (sequentialize-let
            `(module (let ([y.1 200]) 
                          (if (< 3 y.1) 1 0))))
        
        `(module (begin (set! y.1 200) 
                        (if (< 3 y.1) 1 0)))))

(test-case "sequentialize 10 - not, if, and let as pred, nested if"
   (check-equal?
        (sequentialize-let
            `(module (let ([x.37 20] [y.38 21]) 
                          (if (not (> x.37 12)) 
                              (if (if (let ([z.39 x.37]) (< y.38 z.39)) 
                                      (true) 
                                      (false))
                                  10 
                                  12) 
                              (+ x.37 y.38)))))
        
        `(module
            (begin
                (set! x.37 20)
                (set! y.38 21)
                (if (not (> x.37 12))
                    (if (if (begin (set! z.39 x.37) (< y.38 z.39)) 
                            (true) 
                            (false)) 
                        10 
                        12)
                    (+ x.37 y.38))))))

(test-case "sequentialize 11 - multi-nested let"
   (check-equal?
        (sequentialize-let
            `(module (let ((x.48 1)) 
                          (let ((y.49 (let ((z.50 3)) z.50))) 
                               (let ((z.51 (let ((y.52 2)) (+ y.52 y.52)))) 
                                    (if (let ((x.53 6)) (> x.53 7)) 
                                        9 
                                        10))))))
        
        `(module
            (begin
                (set! x.48 1)
                (begin
                (set! y.49 (begin (set! z.50 3) z.50))
                (begin
                    (set! z.51 (begin (set! y.52 2) (+ y.52 y.52)))
                    (if (begin (set! x.53 6) (> x.53 7)) 9 10)))))))

(test-case "sequentialize 12 - let and if as if branches"
   (check-equal?
        (sequentialize-let
            `(module (if (true) 
                         (if (let ((y.54 11) (z.55 15)) (> y.54 z.55)) 
                             14 
                             15) 
                         (let ((z.56 12)) 
                              (let ((z.57 15) (y.58 1)) (+ z.57 y.58))))))
        
        `(module
            (if (true)
                (if (begin 
                        (set! y.54 11) 
                        (set! z.55 15) 
                        (> y.54 z.55)) 
                    14 
                    15)
                (begin 
                    (set! z.56 12) 
                    (begin (set! z.57 15) (set! y.58 1) (+ z.57 y.58)))))))

; M5 tests

(test-case "sequentialize 13 - tail is call"
   (check-equal?
        (sequentialize-let
            `(module (let ((x.5 10)) (call x.5 2 3 4))))
        
        `(module (begin (set! x.5 10) (call x.5 2 3 4)))))

(test-case "sequentialize 14 - define lambda functions"
   (check-equal?
        (sequentialize-let
            `(module 
                (define L.id1.2 (lambda (x.10) x.10)) 
                (define L.id2.3 (lambda (x.11) x.11)) 
                (let ((y.12 (if (true) L.id1.2 L.id2.3))) (call y.12 5))))
        
        `(module
            (define L.id1.2 (lambda (x.10) x.10))
            (define L.id2.3 (lambda (x.11) x.11))
            (begin (set! y.12 (if (true) L.id1.2 L.id2.3)) (call y.12 5)))))

(test-case "sequentialize 15 - complex test"
   (check-equal?
        (sequentialize-let
            `(module 
                (define L.id1.2 (lambda (x.10) x.10)) 
                (define L.id2.3 (lambda (x.11) (let ((x.48 1)) 
                                                    (let ((y.49 (let ((z.50 3)) z.50))) 
                                                        (let ((z.51 (let ((y.52 2)) (+ y.52 y.52)))) 
                                                                (if (let ((x.53 6)) (> x.53 7)) 
                                                                    9 
                                                                    10)))))) 
                (let ((y.12 (if (true) L.id1.2 L.id2.3))) (call y.12 5))))
        
        `(module
            (define L.id1.2 (lambda (x.10) x.10))
            (define L.id2.3
                (lambda (x.11)
                (begin
                    (set! x.48 1)
                    (begin
                    (set! y.49 (begin (set! z.50 3) z.50))
                    (begin
                        (set! z.51 (begin (set! y.52 2) (+ y.52 y.52)))
                        (if (begin (set! x.53 6) (> x.53 7)) 9 10))))))
            (begin (set! y.12 (if (true) L.id1.2 L.id2.3)) (call y.12 5)))))

; M6 Tests

(test-case "sequentialize 16 - tail is subtraction binop"
   (check-equal?
        (sequentialize-let
            `(module (- 2 3)))
        
        `(module (- 2 3))))

(test-case "sequentialize 17 - tail let value is call"
   (check-equal?
        (sequentialize-let
            `(module (let ((x.5 (call x.2 1 x.1))) (call x.5 2 3 4))))
        
        `(module (begin (set! x.5 (call x.2 1 x.1)) (call x.5 2 3 4)))))

(test-case "sequentialize 18 - value let value is call"
   (check-equal?
        (sequentialize-let
            `(module (let ((x.5 (let ((x.5 (call x.3))) (- 2 1)))) 
                          (call x.5 2 3 4))))
        
        `(module
            (begin (set! x.5 (begin (set! x.5 (call x.3)) (- 2 1))) (call x.5 2 3 4)))))

(test-case "sequentialize 19 - value if value is call"
   (check-equal?
        (sequentialize-let
            `(module (let ((x.5 (if (> 1 2) (call x.8) (call x.9 1 2 3)))) 
                          (call x.5 2 3 4))))
        
        `(module
            (begin (set! x.5 (if (> 1 2) (call x.8) (call x.9 1 2 3))) (call x.5 2 3 4)))))

; M7 Tests

(test-case "sequentialize 20 - binop extension"
   (check-equal?
        (sequentialize-let
            `(module (bitwise-and 2 3)))
        
        `(module (bitwise-and 2 3))))

; M8 Tests

(test-case "sequentialize 21 - tail is begin, value is begin"
   (check-equal?
        (sequentialize-let
            `(module (begin 
                        (mset! y.1 z.1 18)
                        (mset! y.1 5 (+ 1 2))
                        (mset! y.1 5 (alloc x.1)) 
                        (mset! y.1 5 (mref x.1 2))
                        (mset! y.1 5 (let ([y.1 200] [y.2 300]) x.5))
                        (mset! y.1 5 (begin (mset! y.1 5 (if (> 1 2) 1 2)) x.5))
                        L.start.1)))
        
        `(module
            (begin
                (mset! y.1 z.1 18)
                (mset! y.1 5 (+ 1 2))
                (mset! y.1 5 (alloc x.1))
                (mset! y.1 5 (mref x.1 2))
                (mset! y.1 5 (begin (set! y.1 200) (set! y.2 300) x.5))
                (mset! y.1 5 (begin (mset! y.1 5 (if (> 1 2) 1 2)) x.5))
                L.start.1))))

(test-case "sequentialize 22 - effect is begin, effect is let"
   (check-equal?
        (sequentialize-let
            `(module (begin
                        (mset! y.1 5 (begin (mset! y.1 5 (if (> 1 2) 1 2)) x.5))
                        (begin 
                            (mset! y.1 5 (mref x.1 2))
                            (mset! y.1 5 (let ([y.1 200] [y.2 300]) x.5)))
                        (let ([y.1 1] [y.2 (mref x.1 5)] [y.3 (alloc y.7)])
                             (mset! y.1 5 (mref x.1 2)))
                        L.start.1)))
        
        `(module
            (begin
                (mset! y.1 5 (begin (mset! y.1 5 (if (> 1 2) 1 2)) x.5))
                (begin
                (mset! y.1 5 (mref x.1 2))
                (mset! y.1 5 (begin (set! y.1 200) (set! y.2 300) x.5)))
                (begin
                (set! y.1 1)
                (set! y.2 (mref x.1 5))
                (set! y.3 (alloc y.7))
                (mset! y.1 5 (mref x.1 2)))
                L.start.1))))

(test-case "sequentialize 23 - pred is begin"
   (check-equal?
        (sequentialize-let
            `(module 
                (if (let ((x.1 (alloc 8)) (y.1 (alloc 16)) (z.1 0)) 
                         (begin 
                            (mset! x.1 0 (let ((tmp.135 (let ((t.1 32)) (let ((tmp.136 (+ t.1 8))) (+ t.1 tmp.136))))) 
                                              (alloc tmp.135))) 
                            (mset! y.1 z.1 18) 
                            (let ((tmp.137 (+ z.1 8))) (mset! y.1 tmp.137 40)) 
                            (let ((tmp.138 (mref y.1 z.1))) 
                                 (let ((tmp.139 (let ((tmp.140 (+ z.1 8))) (mref y.1 tmp.140)))) (= tmp.138 tmp.139))))) 
                    8 16)))
        
        `(module
            (if (begin
                    (set! x.1 (alloc 8))
                    (set! y.1 (alloc 16))
                    (set! z.1 0)
                    (begin
                    (mset! x.1 0
                    (begin
                        (set! tmp.135
                        (begin
                            (set! t.1 32)
                            (begin (set! tmp.136 (+ t.1 8)) (+ t.1 tmp.136))))
                        (alloc tmp.135)))
                    (mset! y.1 z.1 18)
                    (begin (set! tmp.137 (+ z.1 8)) (mset! y.1 tmp.137 40))
                    (begin
                        (set! tmp.138 (mref y.1 z.1))
                        (begin
                        (set! tmp.139
                            (begin (set! tmp.140 (+ z.1 8)) (mref y.1 tmp.140)))
                        (= tmp.138 tmp.139)))))
                8 16))))
