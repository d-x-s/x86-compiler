#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt")

 (test-case "normalize 0 - stack smash"
   (check-equal?
        (normalize-bind
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
        (call L.F.4 8 16 24 32 40 48 56)))
        
        `(module x.1)))

(test-case "normalize 1 - tail is aloc"
   (check-equal?
        (normalize-bind
            `(module x.1))
        
        `(module x.1)))

(test-case "normalize 2 - tail is binop"
   (check-equal?
        (normalize-bind
            `(module (+ 1 2)))
        
        `(module (+ 1 2))))

(test-case "normalize 3 - tail is begin"
   (check-equal?
        (normalize-bind
            `(module (begin (set! x.1 0) 2)))
        
        `(module (begin (set! x.1 0) 2))))

(test-case "normalize 4 - effect is begin"
   (check-equal?
        (normalize-bind
            `(module (begin (set! x.1 0) (begin (set! x.1 0)) 2)))
        
        `(module (begin (set! x.1 0) (begin (set! x.1 0)) 2))))

(test-case "normalize 5 - value is begin"
   (check-equal?
        (normalize-bind
            `(module (begin 
                        (set! x.1 (begin 2))
                        x.1)))
        
        `(module (begin (begin (set! x.1 2)) x.1))))

(test-case "normalize 6 - value is begin, multiple effects"
   (check-equal?
        (normalize-bind
            `(module (begin 
                        (set! x.1 (begin (+ 1 2)))
                        (set! x.2 5)
                        x.1)))
        
        `(module (begin (begin (set! x.1 (+ 1 2))) (set! x.2 5) x.1))))

(test-case "normalize 7 - nested effects"
   (check-equal?
        (normalize-bind
            `(module (begin 
                        (set! x.1 (begin (set! x.2 3) x.2))
                        x.1)))
        
        `(module (begin (begin (set! x.2 3) (set! x.1 x.2)) x.1))))

(test-case "normalize 8 - multi-nested effects"
   (check-equal?
        (normalize-bind
            `(module (begin 
                        (set! x.1 (begin (set! x.2 (begin (set! x.3 7) x.3)) x.2))
                        x.1)))
        
        `(module (begin (begin (begin (set! x.3 7) (set! x.2 x.3)) (set! x.1 x.2)) x.1))))

; M4 Tests

(test-case "normalize 9 - tail is if, if-branch is begin, pred"
   (check-equal?
        (normalize-bind
            `(module (if (true)
                         (begin (set! x.1 1) 2)
                         (if (> 1 2) 3 4))))
        
        `(module (if (true) (begin (set! x.1 1) 2) (if (> 1 2) 3 4)))))

(test-case "normalize 10 - effect value is if"
   (check-equal?
        (normalize-bind
            `(module (begin 
                        (set! x.5 (if (true) 
                                      (begin (set! y.2 14) 12) 
                                      (begin 15))) 
                        x.5)))
        
        `(module
            (begin
                (if (true) (begin (set! y.2 14) (set! x.5 12)) (begin (set! x.5 15)))
                x.5))))

(test-case "normalize 11 - nesting"
   (check-equal?
        (normalize-bind
            `(module (begin 
                        (set! x.37 1) 
                        (begin 
                            (set! x.37 x.37) 
                            (set! y.39 (+ x.37 x.37)) 
                            (set! z.38 (begin (set! x.37 8) x.37)) 
                            (if (true) 
                                (begin (set! z.38 (+ x.37 7)) 8) 
                                9)))))
        
        `(module
            (begin
                (set! x.37 1)
                (begin
                (set! x.37 x.37)
                (set! y.39 (+ x.37 x.37))
                (begin (set! x.37 8) (set! z.38 x.37))
                (if (true) (begin (set! z.38 (+ x.37 7)) 8) 9))))))

(test-case "normalize 12 - effect is if, value is begin"
   (check-equal?
        (normalize-bind
            `(module (begin 
                        (if (true) 
                            (set! x.3 (begin 
                                        (set! y.4 (begin (set! z.4 (+ 4 5)) z.4)) 
                                        y.4)) 
                            (set! x.3 y.7)) x.3)))
        
        `(module
            (begin
                (if (true)
                (begin (begin (set! z.4 (+ 4 5)) (set! y.4 z.4)) (set! x.3 y.4))
                (set! x.3 y.7))
                x.3))))

(test-case "normalize 13 - value is if"
   (check-equal?
        (normalize-bind
            `(module (begin 
                        (set! x.1 0) 
                        (set! x.5 (if (true) 
                                      (begin (set! y.2 (+ x.1 17)) 12) 
                                      (begin 15))) 
                        x.5)))
        
        `(module
            (begin
                (set! x.1 0)
                (if (true)
                    (begin (set! y.2 (+ x.1 17)) (set! x.5 12))
                    (begin (set! x.5 15)))
                x.5))))

(test-case "normalize 14 - effect is if, nested if"
   (check-equal?
        (normalize-bind
            `(module (begin 
                        (set! x.37 20) 
                        (set! y.38 21) 
                        (if (not (> x.37 12)) 
                            (if (if (begin (set! z.39 x.37) (< y.38 z.39)) 
                                    (true) (false)) 
                                10 12) 
                            (+ x.37 y.38)))))
        
        `(module
            (begin
                (set! x.37 20)
                (set! y.38 21)
                (if (not (> x.37 12))
                (if (if (begin (set! z.39 x.37) (< y.38 z.39)) (true) (false)) 10 12)
                (+ x.37 y.38))))))

; M5 Tests

(test-case "normalize 15 - tail is call"
   (check-equal?
        (normalize-bind
            `(module (call x.5 1 2 3)))
        
        `(module (call x.5 1 2 3))))

(test-case "normalize 16 - define functions"
   (check-equal?
        (normalize-bind
            `(module 
                (define L.start.1 (lambda (x.1 x.2)
                                          (begin 
                                            (set! x.5 (if (true) 
                                                        (begin (set! y.2 14) 12) 
                                                        (begin 15))) 
                                            x.5)))
                (define L.start.2 (lambda () (begin 
                                                (if (true) 
                                                    (set! x.3 (begin 
                                                                (set! y.4 (begin (set! z.4 (+ 4 5)) z.4)) 
                                                                y.4)) 
                                                    (set! x.3 y.7)) x.3)))
                (call x.5 1 2 3)))
        
        `(module
            (define L.start.1
                (lambda (x.1 x.2)
                (begin
                    (if (true) (begin (set! y.2 14) (set! x.5 12)) (begin (set! x.5 15)))
                    x.5)))
            (define L.start.2
                (lambda ()
                (begin
                    (if (true)
                    (begin (begin (set! z.4 (+ 4 5)) (set! y.4 z.4)) (set! x.3 y.4))
                    (set! x.3 y.7))
                    x.3)))
            (call x.5 1 2 3))))

; M6 Tests
(test-case "normalize 17 - tail is subtraction binop"
   (check-equal?
        (normalize-bind
            `(module (- 1 2)))
        
        `(module (- 1 2))))

(test-case "normalize 18 - effect value is call"
   (check-equal?
        (normalize-bind
            `(module (begin (set! x.1 (call x.2 1 x.4 5)) 2)))
        
        `(module (begin (set! x.1 (call x.2 1 x.4 5)) 2))))

; M7 Tests

(test-case "normalize 19 - binop extension"
   (check-equal?
        (normalize-bind
            `(module (bitwise-ior 1 2)))
        
        `(module (bitwise-ior 1 2))))

; M8 Tests

(test-case "normalize 20 - value is mref"
   (check-equal?
        (normalize-bind
            `(module (mref x.1 x.2)))
        
        `(module (mref x.1 x.2))))

(test-case "normalize 21 - value is alloc"
   (check-equal?
        (normalize-bind
            `(module (alloc x.1)))
        
        `(module (alloc x.1))))

(test-case "normalize 22 - effect is mset"
   (check-equal?
        (normalize-bind
            `(module (begin 
                        (mset! x.1 5 (+ 1 2))
                        (mset! x.1 5 (mref x.1 2))
                        (mset! x.1 5 (alloc 5))
                        (mset! x.1 5 (begin 
                                        (mset! x.1 5 0) 
                                        (mset! x.1 x.2 0) 
                                        (mset! x.2 x.3 (begin 
                                                            (mset! x.1 5 L.start.1)
                                                            (mset! x.1 5 (call L.s.1 x.1 1 2)) 5)) 6))
                        (mset! x.1 5 (if (not (> x.37 12)) 
                            (if (if (begin (set! z.39 x.37) (< y.38 z.39)) 
                                    (true) (false)) 
                                10 12) 
                            (+ x.37 y.38)))

                        2)))
        
        `(module
            (begin
                (begin (set! tmp.1 (+ 1 2)) (mset! x.1 5 tmp.1))
                (begin (set! tmp.2 (mref x.1 2)) (mset! x.1 5 tmp.2))
                (begin (set! tmp.3 (alloc 5)) (mset! x.1 5 tmp.3))
                (begin
                (mset! x.1 5 0)
                (mset! x.1 x.2 0)
                (begin
                    (mset! x.1 5 L.start.1)
                    (begin (set! tmp.4 (call L.s.1 x.1 1 2)) (mset! x.1 5 tmp.4))
                    (mset! x.2 x.3 5))
                (mset! x.1 5 6))
                (if (not (> x.37 12))
                (if (if (begin (set! z.39 x.37) (< y.38 z.39)) (true) (false))
                    (mset! x.1 5 10)
                    (mset! x.1 5 12))
                (begin (set! tmp.5 (+ x.37 y.38)) (mset! x.1 5 tmp.5)))
                2))))

(test-case "normalize 23 - effect is set"
   (check-equal?
        (normalize-bind
            `(module (begin 
                        (set! x.1 (+ 1 2))
                        (set! x.1 (mref x.1 2))
                        (set! x.1 (alloc 5))
                        (set! x.1 (begin 
                                        (set! x.1 0) 
                                        (set! x.1 0) 
                                        (set! x.2 (begin 
                                                            (set! x.1 L.start.1)
                                                            (set! x.1 (call L.s.1 x.1 1 2)) 5)) 6))
                        (set! x.1 (if (not (> x.37 12)) 
                            (if (if (begin (set! z.39 x.37) (< y.38 z.39)) 
                                    (true) (false)) 
                                10 12) 
                            (+ x.37 y.38)))

                        2)))
        
        `(module
            (begin
                (set! x.1 (+ 1 2))
                (set! x.1 (mref x.1 2))
                (set! x.1 (alloc 5))
                (begin
                (set! x.1 0)
                (set! x.1 0)
                (begin (set! x.1 L.start.1) (set! x.1 (call L.s.1 x.1 1 2)) (set! x.2 5))
                (set! x.1 6))
                (if (not (> x.37 12))
                (if (if (begin (set! z.39 x.37) (< y.38 z.39)) (true) (false))
                    (set! x.1 10)
                    (set! x.1 12))
                (set! x.1 (+ x.37 y.38)))
                2))))
