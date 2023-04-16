#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt")

; M8 Tests
(test-case "remop 0 - stack smash"
    (check-match
        (remove-complex-opera*
            '(module
            (define L.+.5
                (lambda (tmp.16 tmp.17)
                (if (!= (if (= (bitwise-and tmp.17 7) 0) 14 6) 6)
                    (if (!= (if (= (bitwise-and tmp.16 7) 0) 14 6) 6)
                    (+ tmp.16 tmp.17)
                    574)
                    574)))
            (define L.F.4
                (lambda (a.7 b.6 c.5 d.4 e.3 f.2 g.1)
                (call L.+.5 80 (call L.G.5 a.7 b.6 c.5 d.4 e.3 f.2 g.1 64))))
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

     `(module
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
        (call L.F.4 8 16 24 32 40 48 56))))

; M7 Tests

(test-case "remop 1 - simple binop"
    (check-match
        (remove-complex-opera*
            `(module (+ 1 2)))

     `(module (+ 1 2))))

(test-case "remop 2 - binop val1 is atomic"
    (check-match
        (remove-complex-opera*
            `(module (+ 2 (- 4 1))))

     `(module (let ([tmp.1 (- 4 1)]) (+ 2 tmp.1)))))

(test-case "remop 3 - binop val2 is atomic"
    (check-match
        (remove-complex-opera*
            `(module (+ (- 4 1) 2)))

     `(module (let ([tmp.2 (- 4 1)]) (+ tmp.2 2)))))

(test-case "remop 4 - binop val1 and val2 are not atomic"
    (check-match
        (remove-complex-opera*
            `(module (+ (call x.1) (call x.2))))

     `(module (let ([tmp.3 (call x.1)]) (let ([tmp.4 (call x.2)]) (+ tmp.3 tmp.4))))))     

(test-case "remop 5 - complex nested binops"
    (check-match
        (remove-complex-opera*
            `(module (+ (+ 3 (* y.1 y.3)) (+ (- x.3 x.4) (+ 1 2)))))

     `(module
        (let ([tmp.5 (let ([tmp.6 (* y.1 y.3)]) (+ 3 tmp.6))])
            (let ([tmp.7
                (let ([tmp.8 (- x.3 x.4)])
                    (let ([tmp.9 (+ 1 2)]) (+ tmp.8 tmp.9)))])
            (+ tmp.5 tmp.7))))))

(test-case "remop 6 - value is if"
    (check-match
        (remove-complex-opera*
            `(module (if (true) 
                         (if (false) 
                             (+ 1 (call x.1)) 
                             L.start.1) 
                         3)))

     `(module
        (if (true) (if (false) (let ((tmp.10 (call x.1))) (+ 1 tmp.10)) L.start.1) 3))))

(test-case "remop 7 - value is let"
    (check-match
        (remove-complex-opera*
            `(module (let ([x.1 1] 
                           [x.2 (call x.1 (+ 1 2))]) 
                          x.1)))

     `(module (let ((x.1 1) (x.2 (let ((tmp.11 (+ 1 2))) (call x.1 tmp.11)))) x.1))))

(test-case "remop 8 - value is call with non-atomic params"
    (check-match
        (remove-complex-opera*
            `(module (call L.start.1 1 2 (+ 3 (- 3 4)) (+ 4 5) y.1 (call x.1))))

     `(module
        (let ([tmp.12 (let ((tmp.13 (- 3 4))) (+ 3 tmp.13))])
            (let ([tmp.14 (+ 4 5)])
            (let ([tmp.15 (call x.1)]) (call L.start.1 1 2 tmp.12 tmp.14 y.1 tmp.15)))))))

(test-case "remop 9 - value is if, pred is if, pred is let"
    (check-match
        (remove-complex-opera*
            `(module (if (> 1 2) 
                         (call x.1 1 (+ 1 2)) 
                         (if (not (true))
                             (let ([x.1 (+ 1 (* 2 3))]) x.1)
                             (if (if (let ([x.3 5] [x.4 6]) (= x.3 x.4)) (true) (false))
                                 L.start.1
                                 L.start.2)))))

     `(module
        (if (> 1 2)
            (let ((tmp.16 (+ 1 2))) (call x.1 1 tmp.16))
            (if (not (true))
            (let ((x.1 (let ((tmp.17 (* 2 3))) (+ 1 tmp.17)))) x.1)
            (if (if (let ((x.3 5) (x.4 6)) (= x.3 x.4)) (true) (false))
                L.start.1
                L.start.2))))))

(test-case "remop 10 - define functions"
    (check-match
        (remove-complex-opera*
            `(module
                (define L.fn.1 (lambda (x.1 x.2) (call L.fn.2 3 (call L.fn.2 4 5))))
                (define L.fn.2 (lambda (x.3 x.4) (if (let ([x.1 1] [x.2 2]) (<= x.1 x.2)) 
                                   (* 24 (* 11 5))
                                   (* (+ 55 2) (if (>= 3 4) 3 4)))))
                (call L.fn.1 1 (+ 1 2) 3)))

     `(module
        (define L.fn.1
            (lambda (x.1 x.2) (let ((tmp.18 (call L.fn.2 4 5))) (call L.fn.2 3 tmp.18))))
        (define L.fn.2
            (lambda (x.3 x.4)
            (if (let ((x.1 1) (x.2 2)) (<= x.1 x.2))
                (let ((tmp.19 (* 11 5))) (* 24 tmp.19))
                (let ((tmp.20 (+ 55 2)))
                (let ((tmp.21 (if (>= 3 4) 3 4))) (* tmp.20 tmp.21))))))
        (let ((tmp.22 (+ 1 2))) (call L.fn.1 1 tmp.22 3)))))

(test-case "remop 11 - extend with bitwise ops and shift"
    (check-match
        (remove-complex-opera*
            `(module (call x.1
                        (bitwise-and 1 2)
                        (bitwise-ior 1 2)
                        (bitwise-xor 1 2)
                        (arithmetic-shift-right 1 2))))

     `(module
        (let ((tmp.23 (bitwise-and 1 2)))
            (let ((tmp.24 (bitwise-ior 1 2)))
            (let ((tmp.25 (bitwise-xor 1 2)))
                (let ((tmp.26 (arithmetic-shift-right 1 2)))
                (call x.1 tmp.23 tmp.24 tmp.25 tmp.26))))))))

(test-case "remop 12 - complex factorial"
    (check-match
        (remove-complex-opera*
            `(module 
                (define L.*.4 (lambda (tmp.1 tmp.2) 
                                      (if (!= (if (= (bitwise-and tmp.2 7) 0) 14 6) 6) 
                                          (if (!= (if (= (bitwise-and tmp.1 7) 0) 14 6) 6) (* tmp.1 (arithmetic-shift-right tmp.2 3)) 318) 
                                           318))) 
                (define L.+.3 (lambda (tmp.3 tmp.4) 
                                      (if (!= (if (= (bitwise-and tmp.4 7) 0) 14 6) 6) 
                                          (if (!= (if (= (bitwise-and tmp.3 7) 0) 14 6) 6) (+ tmp.3 tmp.4) 574) 
                                          574))) 
                (define L.-.2 (lambda (tmp.5 tmp.6) 
                                      (if (!= (if (= (bitwise-and tmp.6 7) 0) 14 6) 6) 
                                          (if (!= (if (= (bitwise-and tmp.5 7) 0) 14 6) 6) (- tmp.5 tmp.6) 830) 
                                          830))) 
                (define L.eq?.1 (lambda (tmp.15 tmp.16) 
                                        (if (= tmp.15 tmp.16) 14 6))) 
                (define L.identity.7 (lambda (x.16) 
                                             (if (!= (call L.eq?.1 x.16 0) 6) 
                                                  0 
                                                 (let ((y.17 (call L.-.2 x.16 8))) (let ((x.18 (call L.identity.7 y.17))) (call L.+.3 8 x.18)))))) 
                (define L.fact.8 (lambda (x.19) 
                                         (let ((x.20 (call L.identity.7 x.19)) (y.21 (call L.identity.7 0))) 
                                              (if (!= (call L.eq?.1 x.20 y.21) 6) 
                                                  (let ((z.22 (call L.identity.7 8))) z.22) 
                                                  (let ((n.23 (call L.identity.7 8))) 
                                                       (let ((z.24 (call L.-.2 x.20 n.23))) 
                                                            (let ((y.25 (call L.fact.8 z.24))) 
                                                                 (call L.*.4 x.20 y.25)))))))) 
                (call L.fact.8 40)))

     `(module
        (define L.*.4
            (lambda (tmp.1 tmp.2)
            (if (let ((tmp.27
                        (if (let ((tmp.28 (bitwise-and tmp.2 7))) (= tmp.28 0)) 14 6)))
                    (!= tmp.27 6))
                (if (let ((tmp.29
                        (if (let ((tmp.30 (bitwise-and tmp.1 7))) (= tmp.30 0))
                            14
                            6)))
                    (!= tmp.29 6))
                (let ((tmp.31 (arithmetic-shift-right tmp.2 3))) (* tmp.1 tmp.31))
                318)
                318)))
        (define L.+.3
            (lambda (tmp.3 tmp.4)
            (if (let ((tmp.32
                        (if (let ((tmp.33 (bitwise-and tmp.4 7))) (= tmp.33 0)) 14 6)))
                    (!= tmp.32 6))
                (if (let ((tmp.34
                        (if (let ((tmp.35 (bitwise-and tmp.3 7))) (= tmp.35 0))
                            14
                            6)))
                    (!= tmp.34 6))
                (+ tmp.3 tmp.4)
                574)
                574)))
        (define L.-.2
            (lambda (tmp.5 tmp.6)
            (if (let ((tmp.36
                        (if (let ((tmp.37 (bitwise-and tmp.6 7))) (= tmp.37 0))
                        14
                        6)))
                    (!= tmp.36 6))
                (if (let ((tmp.38
                        (if (let ((tmp.39 (bitwise-and tmp.5 7))) (= tmp.39 0))
                            14
                            6)))
                    (!= tmp.38 6))
                (- tmp.5 tmp.6)
                830)
                830)))
        (define L.eq?.1 (lambda (tmp.15 tmp.16) (if (= tmp.15 tmp.16) 14 6)))
        (define L.identity.7
            (lambda (x.16)
            (if (let ((tmp.40 (call L.eq?.1 x.16 0))) (!= tmp.40 6))
                0
                (let ((y.17 (call L.-.2 x.16 8)))
                (let ((x.18 (call L.identity.7 y.17))) (call L.+.3 8 x.18))))))
        (define L.fact.8
            (lambda (x.19)
            (let ((x.20 (call L.identity.7 x.19)) (y.21 (call L.identity.7 0)))
                (if (let ((tmp.41 (call L.eq?.1 x.20 y.21))) (!= tmp.41 6))
                (let ((z.22 (call L.identity.7 8))) z.22)
                (let ((n.23 (call L.identity.7 8)))
                    (let ((z.24 (call L.-.2 x.20 n.23)))
                    (let ((y.25 (call L.fact.8 z.24))) (call L.*.4 x.20 y.25))))))))
        (call L.fact.8 40))))

; M8 Tests

(test-case "remop 13 - call non-atomic value"
    (check-match
        (remove-complex-opera*
            `(module (call (let ([x.1 1] [x.2 (call x.1 (+ 1 2))]) x.1) 
                            1 2 (+ 3 (- 3 4)))))

     `(module
        (let ((tmp.42
                (let ((x.1 1) (x.2 (let ((tmp.43 (+ 1 2))) (call x.1 tmp.43)))) x.1)))
            (let ((tmp.44 (let ((tmp.45 (- 3 4))) (+ 3 tmp.45))))
            (call tmp.42 1 2 tmp.44))))))

(test-case "remop 14 - value is mref"
    (check-match
        (remove-complex-opera*
            `(module (mref (call x.1) (if (true) x.1 x.2))))

     `(module
        (let ((tmp.46 (call x.1)))
            (let ((tmp.47 (if (true) x.1 x.2))) (mref tmp.46 tmp.47))))))

(test-case "remop 15 - value is alloc"
    (check-match
        (remove-complex-opera*
            `(module (alloc (+ 1 2))))

     `(module (let ((tmp.48 (+ 1 2))) (alloc tmp.48)))))

(test-case "remop 16 - value is begin effects"
    (check-match
        (remove-complex-opera*
            `(module (begin (mset! (call L.s.1 (+ 1 2)) (call L.s.2 (+ 1 2)) (call L.s.3 (+ 1 2)))
                            (begin (mset! (call L.s.1 (+ 1 2)) 1 2)
                                   (mset! z.1 z.2 z.3))
                            (+ (call x.1) (call x.2)))))

     `(module
        (begin
            (let ((tmp.49 (let ((tmp.50 (+ 1 2))) (call L.s.1 tmp.50))))
            (let ((tmp.51 (let ((tmp.52 (+ 1 2))) (call L.s.2 tmp.52))))
                (mset! tmp.49 tmp.51 (let ((tmp.53 (+ 1 2))) (call L.s.3 tmp.53)))))
            (begin
            (let ((tmp.54 (let ((tmp.55 (+ 1 2))) (call L.s.1 tmp.55))))
                (mset! tmp.54 1 2))
            (mset! z.1 z.2 z.3))
            (let ((tmp.56 (call x.1))) (let ((tmp.57 (call x.2))) (+ tmp.56 tmp.57)))))))

(test-case "remop 17 - pred is begin"
    (check-match
        (remove-complex-opera*
            `(module (if (begin (mset! (call L.s.1 (+ 1 2)) x.2 (call L.s.2 (+ 1 2)))
                                (begin (mset! (call L.s.1 (+ 1 2)) 1 2)
                                       (mset! z.1 z.2 z.3))
                                (true)) 
                         1 2)))

     `(module
        (if (begin
                (let ((tmp.58 (let ((tmp.59 (+ 1 2))) (call L.s.1 tmp.59))))
                (mset! tmp.58 x.2 (let ((tmp.60 (+ 1 2))) (call L.s.2 tmp.60))))
                (begin
                    (let ((tmp.61 (let ((tmp.62 (+ 1 2))) (call L.s.1 tmp.62))))
                        (mset! tmp.61 1 2))
                    (mset! z.1 z.2 z.3))
                (true))
            1
            2))))