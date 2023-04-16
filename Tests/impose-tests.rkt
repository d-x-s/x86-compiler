#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt")

(test-case "impose 1 - tail value"
  (check-equal?
    (impose-calling-conventions
      '(module 
        (begin 
          (set! y.4 5)
          (set! x.3 (+ 1 2))
          (set! z.5 2)
          (set! x.3 (+ x.3 y.4))
          (set! x.3 (+ y.4 z.5))
          (set! x.3 x.3)
          x.3)))

    '(module
      ((new-frames ()))
      (begin
        (set! tmp-ra.1 r15)
        (begin
          (set! y.4 5)
          (set! x.3 (+ 1 2))
          (set! z.5 2)
          (set! x.3 (+ x.3 y.4))
          (set! x.3 (+ y.4 z.5))
          (set! x.3 x.3)
          (begin (set! rax x.3) (jump tmp-ra.1 rbp rax)))))))

(test-case "impose 2 - transforming procedure"
  (check-equal?
    (impose-calling-conventions
      '(module (define L.zero.3 (lambda (v0.11 v1.12 v2.13 v3.14) 0)) 0))

   '(module
      ((new-frames ()))
      (define L.zero.3
        ((new-frames ()))
        (begin
          (set! tmp-ra.3 r15)
          (begin
            (set! v0.11 rdi)
            (set! v1.12 rsi)
            (set! v2.13 rdx)
            (set! v3.14 rcx)
            (begin (set! rax 0) (jump tmp-ra.3 rbp rax)))))
          (begin (set! tmp-ra.2 r15) (begin (set! rax 0) (jump tmp-ra.2 rbp rax))))))

(test-case "impose 3 - tail call + non-tail call"
  (check-equal?
    (impose-calling-conventions
      '(module 
        (begin 
          (set! y.4 5)
          (set! x.3 (+ 1 2))
          (set! z.5 2)
          (set! x.3 (+ x.3 y.4))
          (set! x.3 (+ y.4 z.5))
          (set! x.3 (call x.1))
          (call x.3))))

      '(module
        ((new-frames (())))
        (begin
          (set! tmp-ra.4 r15)
          (begin
            (set! y.4 5)
            (set! x.3 (+ 1 2))
            (set! z.5 2)
            (set! x.3 (+ x.3 y.4))
            (set! x.3 (+ y.4 z.5))
            (begin
              (return-point L.rp.1 (begin (set! r15 L.rp.1) (jump x.1 rbp r15)))
              (set! x.3 rax))
            (begin (set! r15 tmp-ra.4) (jump x.3 rbp r15)))))))

(test-case "impose 4 - tail value"
  (check-equal?
    (impose-calling-conventions
      '(module 100))

    '(module
      ((new-frames ()))
      (begin (set! tmp-ra.5 r15) (begin (set! rax 100) (jump tmp-ra.5 rbp rax))))))

(test-case "impose 5 - predicate tail value"
  (check-equal?
    (impose-calling-conventions
      '(module
        (if (true) 100  200)))

    '(module
        ((new-frames ()))
        (begin
          (set! tmp-ra.6 r15)
          (if (true)
            (begin (set! rax 100) (jump tmp-ra.6 rbp rax))
            (begin (set! rax 200) (jump tmp-ra.6 rbp rax)))))))

(test-case "impose 6 - multiple non-tail calls"
  (check-equal?
    (impose-calling-conventions
      '(module 
        (begin 
          (set! y.4 5)
          (set! x.3 (+ 1 2))
          (set! z.5 2)
          (set! x.3 (+ x.3 y.4))
          (set! x.3 (+ y.4 z.5))
          (set! x.3 (call x.1))
          (set! x.2 (call x.2))
          (call x.3))))

    '(module
      ((new-frames (() ())))
      (begin
        (set! tmp-ra.7 r15)
        (begin
          (set! y.4 5)
          (set! x.3 (+ 1 2))
          (set! z.5 2)
          (set! x.3 (+ x.3 y.4))
          (set! x.3 (+ y.4 z.5))
          (begin
            (return-point L.rp.2 (begin (set! r15 L.rp.2) (jump x.1 rbp r15)))
            (set! x.3 rax))
          (begin
            (return-point L.rp.3 (begin (set! r15 L.rp.3) (jump x.2 rbp r15)))
            (set! x.2 rax))
          (begin (set! r15 tmp-ra.7) (jump x.3 rbp r15)))))))

(test-case "impose 7 - predicate traversal"
  (check-equal?
    (impose-calling-conventions
      '(module 
        (begin 
          (if (true) 
              (begin (begin (set! z.4 (+ 4 5)) (set! y.4 z.4)) (set! x.3 y.4)) 
              (set! x.3 y.4)) 
          x.3)))

    '(module
      ((new-frames ()))
      (begin
        (set! tmp-ra.8 r15)
        (begin
          (if (true)
            (begin (begin (set! z.4 (+ 4 5)) (set! y.4 z.4)) (set! x.3 y.4))
            (set! x.3 y.4))
          (begin (set! rax x.3) (jump tmp-ra.8 rbp rax)))))))

(test-case "impose 8 - non-tail call in predicate"
  (check-equal?
    (impose-calling-conventions
      '(module 
        (begin 
          (if (begin (set! x.1 (call x.3)) (true))
              (begin (begin (set! z.4 (+ 4 5)) (set! y.4 z.4)) (set! x.3 y.4)) 
              (set! x.3 y.4))
          x.3)))

    '(module
      ((new-frames (())))
      (begin
        (set! tmp-ra.9 r15)
        (begin
          (if (begin
                (begin
                  (return-point
                  L.rp.4
                  (begin (set! r15 L.rp.4) (jump x.3 rbp r15)))
                  (set! x.1 rax))
                (true))
            (begin (begin (set! z.4 (+ 4 5)) (set! y.4 z.4)) (set! x.3 y.4))
            (set! x.3 y.4))
          (begin (set! rax x.3) (jump tmp-ra.9 rbp rax)))))))

(test-case "impose 9 - extend binops"
  (check-equal?
    (impose-calling-conventions
      '(module (bitwise-xor 1 2)))

    '(module
      ((new-frames ()))
      (begin
        (set! tmp-ra.10 r15)
        (begin (set! rax (bitwise-xor 1 2)) (jump tmp-ra.10 rbp rax))))))

; M8 Tests

(test-case "impose 10 - effect is mset"
  (check-equal?
    (impose-calling-conventions
      '(module (begin
                  (mset! x.1 5 5)
                  (mset! x.1 x.2 x.3)
                  (mset! x.1 5 L.start.1)
                  5)))

    '(module
      ((new-frames ()))
      (begin
        (set! tmp-ra.11 r15)
        (begin
          (mset! x.1 5 5)
          (mset! x.1 x.2 x.3)
          (mset! x.1 5 L.start.1)
          (begin (set! rax 5) (jump tmp-ra.11 rbp rax)))))))


 (test-case "impose 11 - empty frame variables"
  (check-equal?
    (impose-calling-conventions
    '(module
      (define L.F.4
        (lambda (a.7)
          (begin
            (set! tmp.5 (call L.G.5))
            (set! tmp.5 (call L.G.5))
            (set! tmp.5 (call L.G.5))
            (set! tmp.5 (call L.G.5))
            (call L.+.5 80 tmp.5))))
      (call L.F.4)))
      
  '(module
  ((new-frames ()))
  (define L.F.4
    ((new-frames (() () () ())))
    (begin
      (set! tmp-ra.1 r15)
      (begin
        (set! a.7 rdi)
        (begin
          (begin
            (return-point
             L.rp.4
             (begin (set! r15 L.rp.4) (jump L.G.5 rbp r15)))
            (set! tmp.5 rax))
          (begin
            (return-point
             L.rp.5
             (begin (set! r15 L.rp.5) (jump L.G.5 rbp r15)))
            (set! tmp.5 rax))
          (begin
            (return-point
             L.rp.6
             (begin (set! r15 L.rp.6) (jump L.G.5 rbp r15)))
            (set! tmp.5 rax))
          (begin
            (return-point
             L.rp.7
             (begin (set! r15 L.rp.7) (jump L.G.5 rbp r15)))
            (set! tmp.5 rax))
          (begin
            (set! rsi tmp.5)
            (set! rdi 80)
            (set! r15 tmp-ra.1)
            (jump L.+.5 rbp r15 rdi rsi))))))
  (begin (set! tmp-ra.2 r15) (begin (set! r15 tmp-ra.2) (jump L.F.4 rbp r15))))))

 (test-case "impose 12 - multiple frame variables"
  (check-equal?
    (impose-calling-conventions
    '(module
      (define L.F.4
        (lambda (a.7 b.6 c.5 d.4 e.3 f.2 g.1)
          (begin
            (set! tmp.5 (call L.G.5 a.7 b.6 c.5 d.4 e.3 f.2 g.1 64))
            (call L.+.5 80 tmp.5))))
      (call L.F.4)))

      '(module
  ((new-frames ()))
  (define L.F.4
    ((new-frames ((nfv.2 nfv.3))))
    (begin
      (set! tmp-ra.1 r15)
      (begin
        (set! a.7 rdi)
        (set! b.6 rsi)
        (set! c.5 rdx)
        (set! d.4 rcx)
        (set! e.3 r8)
        (set! f.2 r9)
        (set! g.1 fv0)
        (begin
          (begin
            (return-point
             L.rp.4
             (begin
               (set! nfv.3 64)
               (set! nfv.2 g.1)
               (set! r9 f.2)
               (set! r8 e.3)
               (set! rcx d.4)
               (set! rdx c.5)
               (set! rsi b.6)
               (set! rdi a.7)
               (set! r15 L.rp.4)
               (jump L.G.5 rbp r15 rdi rsi rdx rcx r8 r9 nfv.2 nfv.3)))
            (set! tmp.5 rax))
          (begin
            (set! rsi tmp.5)
            (set! rdi 80)
            (set! r15 tmp-ra.1)
            (jump L.+.5 rbp r15 rdi rsi))))))
  (begin (set! tmp-ra.4 r15) (begin (set! r15 tmp-ra.4) (jump L.F.4 rbp r15))))))

   (test-case "impose 0 - stack smash"
  (check-equal?
    (impose-calling-conventions
    '(module
      (define L.+.5
        (lambda (tmp.16 tmp.17)
          (if (begin
                (if (begin (set! tmp.2 (bitwise-and tmp.17 7)) (= tmp.2 0))
                  (set! tmp.1 14)
                  (set! tmp.1 6))
                (!= tmp.1 6))
            (if (begin
                  (if (begin (set! tmp.4 (bitwise-and tmp.16 7)) (= tmp.4 0))
                    (set! tmp.3 14)
                    (set! tmp.3 6))
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

    '(module
      ((new-frames ()))
      (define L.+.5
        ((new-frames ()))
        (begin
          (set! tmp-ra.1 r15)
          (begin
            (set! tmp.16 rdi)
            (set! tmp.17 rsi)
            (if (begin
                  (if (begin (set! tmp.2 (bitwise-and tmp.17 7)) (= tmp.2 0))
                    (set! tmp.1 14)
                    (set! tmp.1 6))
                  (!= tmp.1 6))
              (if (begin
                    (if (begin (set! tmp.4 (bitwise-and tmp.16 7)) (= tmp.4 0))
                      (set! tmp.3 14)
                      (set! tmp.3 6))
                    (!= tmp.3 6))
                (begin (set! rax (+ tmp.16 tmp.17)) (jump tmp-ra.1 rbp rax))
                (begin (set! rax 574) (jump tmp-ra.1 rbp rax)))
              (begin (set! rax 574) (jump tmp-ra.1 rbp rax))))))
      (define L.F.4
        ((new-frames ((nfv.3 nfv.4))))
        (begin
          (set! tmp-ra.2 r15)
          (begin
            (set! a.7 rdi)
            (set! b.6 rsi)
            (set! c.5 rdx)
            (set! d.4 rcx)
            (set! e.3 r8)
            (set! f.2 r9)
            (set! g.1 fv0)
            (begin
              (begin
                (return-point
                L.rp.4
                (begin
                  (set! nfv.4 64)
                  (set! nfv.3 g.1)
                  (set! r9 f.2)
                  (set! r8 e.3)
                  (set! rcx d.4)
                  (set! rdx c.5)
                  (set! rsi b.6)
                  (set! rdi a.7)
                  (set! r15 L.rp.4)
                  (jump L.G.5 rbp r15 rdi rsi rdx rcx r8 r9 nfv.3 nfv.4)))
                (set! tmp.5 rax))
              (begin
                (set! rsi tmp.5)
                (set! rdi 80)
                (set! r15 tmp-ra.2)
                (jump L.+.5 rbp r15 rdi rsi))))))
      (define L.G.5
        ((new-frames ()))
        (begin
          (set! tmp-ra.5 r15)
          (begin
            (set! a.15 rdi)
            (set! b.14 rsi)
            (set! c.13 rdx)
            (set! d.12 rcx)
            (set! e.11 r8)
            (set! f.10 r9)
            (set! g.9 fv0)
            (set! h.8 fv1)
            (begin
              (set! fv2 72)
              (set! fv1 h.8)
              (set! fv0 g.9)
              (set! r9 f.10)
              (set! r8 e.11)
              (set! rcx d.12)
              (set! rdx c.13)
              (set! rsi b.14)
              (set! rdi a.15)
              (set! r15 tmp-ra.5)
              (jump L.H.6 rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2)))))
      (define L.H.6
        ((new-frames (() () () () () () ())))
        (begin
          (set! tmp-ra.6 r15)
          (begin
            (set! a.24 rdi)
            (set! b.23 rsi)
            (set! c.22 rdx)
            (set! d.21 rcx)
            (set! e.20 r8)
            (set! f.19 r9)
            (set! g.18 fv0)
            (set! h.17 fv1)
            (set! j.16 fv2)
            (begin
              (begin
                (return-point
                L.rp.5
                (begin
                  (set! rsi b.23)
                  (set! rdi a.24)
                  (set! r15 L.rp.5)
                  (jump L.+.5 rbp r15 rdi rsi)))
                (set! r1.25 rax))
              (begin
                (begin
                  (return-point
                  L.rp.6
                  (begin
                    (set! rsi c.22)
                    (set! rdi r1.25)
                    (set! r15 L.rp.6)
                    (jump L.+.5 rbp r15 rdi rsi)))
                  (set! r2.26 rax))
                (begin
                  (begin
                    (return-point
                    L.rp.7
                    (begin
                      (set! rsi d.21)
                      (set! rdi r2.26)
                      (set! r15 L.rp.7)
                      (jump L.+.5 rbp r15 rdi rsi)))
                    (set! r3.27 rax))
                  (begin
                    (begin
                      (return-point
                      L.rp.8
                      (begin
                        (set! rsi e.20)
                        (set! rdi r3.27)
                        (set! r15 L.rp.8)
                        (jump L.+.5 rbp r15 rdi rsi)))
                      (set! r4.28 rax))
                    (begin
                      (begin
                        (return-point
                        L.rp.9
                        (begin
                          (set! rsi f.19)
                          (set! rdi r4.28)
                          (set! r15 L.rp.9)
                          (jump L.+.5 rbp r15 rdi rsi)))
                        (set! r5.29 rax))
                      (begin
                        (begin
                          (return-point
                          L.rp.10
                          (begin
                            (set! rsi g.18)
                            (set! rdi r5.29)
                            (set! r15 L.rp.10)
                            (jump L.+.5 rbp r15 rdi rsi)))
                          (set! r6.30 rax))
                        (begin
                          (begin
                            (return-point
                            L.rp.11
                            (begin
                              (set! rsi h.17)
                              (set! rdi r6.30)
                              (set! r15 L.rp.11)
                              (jump L.+.5 rbp r15 rdi rsi)))
                            (set! r7.31 rax))
                          (begin
                            (set! rsi j.16)
                            (set! rdi r7.31)
                            (set! r15 tmp-ra.6)
                            (jump L.+.5 rbp r15 rdi rsi))))))))))))
      (begin
        (set! tmp-ra.7 r15)
        (begin
          (set! fv0 56)
          (set! r9 48)
          (set! r8 40)
          (set! rcx 32)
          (set! rdx 24)
          (set! rsi 16)
          (set! rdi 8)
          (set! r15 tmp-ra.7)
          (jump L.F.4 rbp r15 rdi rsi rdx rcx r8 r9 fv0))))))
