#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt")

 (test-case "uncover 0 - stack smash"
  (check-equal?
    (uncover-locals
    '(module
      ((new-frames ()))
      (define L.+.5
        ((new-frames ()))
        (begin
          (set! tmp-ra.1 r15)
          (set! tmp.16 rdi)
          (set! tmp.17 rsi)
          (if (begin
                (if (begin
                      (begin
                        (set! tmp.2 tmp.17)
                        (set! tmp.2 (bitwise-and tmp.2 7)))
                      (= tmp.2 0))
                  (set! tmp.1 14)
                  (set! tmp.1 6))
                (!= tmp.1 6))
            (if (begin
                  (if (begin
                        (begin
                          (set! tmp.4 tmp.16)
                          (set! tmp.4 (bitwise-and tmp.4 7)))
                        (= tmp.4 0))
                    (set! tmp.3 14)
                    (set! tmp.3 6))
                  (!= tmp.3 6))
              (begin
                (set! rax tmp.16)
                (set! rax (+ rax tmp.17))
                (jump tmp-ra.1 rbp rax))
              (begin (set! rax 574) (jump tmp-ra.1 rbp rax)))
            (begin (set! rax 574) (jump tmp-ra.1 rbp rax)))))
      (define L.F.4
        ((new-frames ((nfv.3 nfv.4))))
        (begin
          (set! tmp-ra.2 r15)
          (set! a.7 rdi)
          (set! b.6 rsi)
          (set! c.5 rdx)
          (set! d.4 rcx)
          (set! e.3 r8)
          (set! f.2 r9)
          (set! g.1 fv0)
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
          (set! tmp.5 rax)
          (set! rsi tmp.5)
          (set! rdi 80)
          (set! r15 tmp-ra.2)
          (jump L.+.5 rbp r15 rdi rsi)))
      (define L.G.5
        ((new-frames ()))
        (begin
          (set! tmp-ra.5 r15)
          (set! a.15 rdi)
          (set! b.14 rsi)
          (set! c.13 rdx)
          (set! d.12 rcx)
          (set! e.11 r8)
          (set! f.10 r9)
          (set! g.9 fv0)
          (set! h.8 fv1)
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
          (jump L.H.6 rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2)))
      (define L.H.6
        ((new-frames (() () () () () () ())))
        (begin
          (set! tmp-ra.6 r15)
          (set! a.24 rdi)
          (set! b.23 rsi)
          (set! c.22 rdx)
          (set! d.21 rcx)
          (set! e.20 r8)
          (set! f.19 r9)
          (set! g.18 fv0)
          (set! h.17 fv1)
          (set! j.16 fv2)
          (return-point
          L.rp.5
          (begin
            (set! rsi b.23)
            (set! rdi a.24)
            (set! r15 L.rp.5)
            (jump L.+.5 rbp r15 rdi rsi)))
          (set! r1.25 rax)
          (return-point
          L.rp.6
          (begin
            (set! rsi c.22)
            (set! rdi r1.25)
            (set! r15 L.rp.6)
            (jump L.+.5 rbp r15 rdi rsi)))
          (set! r2.26 rax)
          (return-point
          L.rp.7
          (begin
            (set! rsi d.21)
            (set! rdi r2.26)
            (set! r15 L.rp.7)
            (jump L.+.5 rbp r15 rdi rsi)))
          (set! r3.27 rax)
          (return-point
          L.rp.8
          (begin
            (set! rsi e.20)
            (set! rdi r3.27)
            (set! r15 L.rp.8)
            (jump L.+.5 rbp r15 rdi rsi)))
          (set! r4.28 rax)
          (return-point
          L.rp.9
          (begin
            (set! rsi f.19)
            (set! rdi r4.28)
            (set! r15 L.rp.9)
            (jump L.+.5 rbp r15 rdi rsi)))
          (set! r5.29 rax)
          (return-point
          L.rp.10
          (begin
            (set! rsi g.18)
            (set! rdi r5.29)
            (set! r15 L.rp.10)
            (jump L.+.5 rbp r15 rdi rsi)))
          (set! r6.30 rax)
          (return-point
          L.rp.11
          (begin
            (set! rsi h.17)
            (set! rdi r6.30)
            (set! r15 L.rp.11)
            (jump L.+.5 rbp r15 rdi rsi)))
          (set! r7.31 rax)
          (set! rsi j.16)
          (set! rdi r7.31)
          (set! r15 tmp-ra.6)
          (jump L.+.5 rbp r15 rdi rsi)))
      (begin
        (set! tmp-ra.7 r15)
        (set! fv0 56)
        (set! r9 48)
        (set! r8 40)
        (set! rcx 32)
        (set! rdx 24)
        (set! rsi 16)
        (set! rdi 8)
        (set! r15 tmp-ra.7)
        (jump L.F.4 rbp r15 rdi rsi rdx rcx r8 r9 fv0)))
    )
    
    '(module
      ((new-frames ()) (locals (tmp-ra.7)))
      (define L.+.5
        ((new-frames ()) (locals (tmp.3 tmp.16 tmp.4 tmp-ra.1 tmp.2 tmp.17 tmp.1)))
        (begin
          (set! tmp-ra.1 r15)
          (set! tmp.16 rdi)
          (set! tmp.17 rsi)
          (if (begin
                (if (begin
                      (begin
                        (set! tmp.2 tmp.17)
                        (set! tmp.2 (bitwise-and tmp.2 7)))
                      (= tmp.2 0))
                  (set! tmp.1 14)
                  (set! tmp.1 6))
                (!= tmp.1 6))
            (if (begin
                  (if (begin
                        (begin
                          (set! tmp.4 tmp.16)
                          (set! tmp.4 (bitwise-and tmp.4 7)))
                        (= tmp.4 0))
                    (set! tmp.3 14)
                    (set! tmp.3 6))
                  (!= tmp.3 6))
              (begin
                (set! rax tmp.16)
                (set! rax (+ rax tmp.17))
                (jump tmp-ra.1 rbp rax))
              (begin (set! rax 574) (jump tmp-ra.1 rbp rax)))
            (begin (set! rax 574) (jump tmp-ra.1 rbp rax)))))
      (define L.F.4
        ((new-frames ((nfv.3 nfv.4)))
        (locals (tmp.5 nfv.4 nfv.3 g.1 f.2 e.3 d.4 c.5 b.6 a.7 tmp-ra.2)))
        (begin
          (set! tmp-ra.2 r15)
          (set! a.7 rdi)
          (set! b.6 rsi)
          (set! c.5 rdx)
          (set! d.4 rcx)
          (set! e.3 r8)
          (set! f.2 r9)
          (set! g.1 fv0)
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
          (set! tmp.5 rax)
          (set! rsi tmp.5)
          (set! rdi 80)
          (set! r15 tmp-ra.2)
          (jump L.+.5 rbp r15 rdi rsi)))
      (define L.G.5
        ((new-frames ()) (locals (h.8 g.9 f.10 e.11 d.12 c.13 b.14 a.15 tmp-ra.5)))
        (begin
          (set! tmp-ra.5 r15)
          (set! a.15 rdi)
          (set! b.14 rsi)
          (set! c.13 rdx)
          (set! d.12 rcx)
          (set! e.11 r8)
          (set! f.10 r9)
          (set! g.9 fv0)
          (set! h.8 fv1)
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
          (jump L.H.6 rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2)))
      (define L.H.6
        ((new-frames (() () () () () () ()))
        (locals
          (r7.31
          r6.30
          r5.29
          r4.28
          r3.27
          r2.26
          r1.25
          j.16
          h.17
          g.18
          f.19
          e.20
          d.21
          c.22
          b.23
          a.24
          tmp-ra.6)))
        (begin
          (set! tmp-ra.6 r15)
          (set! a.24 rdi)
          (set! b.23 rsi)
          (set! c.22 rdx)
          (set! d.21 rcx)
          (set! e.20 r8)
          (set! f.19 r9)
          (set! g.18 fv0)
          (set! h.17 fv1)
          (set! j.16 fv2)
          (return-point
          L.rp.5
          (begin
            (set! rsi b.23)
            (set! rdi a.24)
            (set! r15 L.rp.5)
            (jump L.+.5 rbp r15 rdi rsi)))
          (set! r1.25 rax)
          (return-point
          L.rp.6
          (begin
            (set! rsi c.22)
            (set! rdi r1.25)
            (set! r15 L.rp.6)
            (jump L.+.5 rbp r15 rdi rsi)))
          (set! r2.26 rax)
          (return-point
          L.rp.7
          (begin
            (set! rsi d.21)
            (set! rdi r2.26)
            (set! r15 L.rp.7)
            (jump L.+.5 rbp r15 rdi rsi)))
          (set! r3.27 rax)
          (return-point
          L.rp.8
          (begin
            (set! rsi e.20)
            (set! rdi r3.27)
            (set! r15 L.rp.8)
            (jump L.+.5 rbp r15 rdi rsi)))
          (set! r4.28 rax)
          (return-point
          L.rp.9
          (begin
            (set! rsi f.19)
            (set! rdi r4.28)
            (set! r15 L.rp.9)
            (jump L.+.5 rbp r15 rdi rsi)))
          (set! r5.29 rax)
          (return-point
          L.rp.10
          (begin
            (set! rsi g.18)
            (set! rdi r5.29)
            (set! r15 L.rp.10)
            (jump L.+.5 rbp r15 rdi rsi)))
          (set! r6.30 rax)
          (return-point
          L.rp.11
          (begin
            (set! rsi h.17)
            (set! rdi r6.30)
            (set! r15 L.rp.11)
            (jump L.+.5 rbp r15 rdi rsi)))
          (set! r7.31 rax)
          (set! rsi j.16)
          (set! rdi r7.31)
          (set! r15 tmp-ra.6)
          (jump L.+.5 rbp r15 rdi rsi)))
      (begin
        (set! tmp-ra.7 r15)
        (set! fv0 56)
        (set! r9 48)
        (set! r8 40)
        (set! rcx 32)
        (set! rdx 24)
        (set! rsi 16)
        (set! rdi 8)
        (set! r15 tmp-ra.7)
        (jump L.F.4 rbp r15 rdi rsi rdx rcx r8 r9 fv0)))))

; M6 Tests

(test-case "uncover 1 - set"
  (check-equal?
    (uncover-locals
      '(module 
        ((new-frames (()))) 
        (begin
          (set! rax L.start.1)
          (set! x.1 x.2)
          (set! rax (+ rax x.3))
          (set! x.4 (+ x.4 x.5))
          (if (> y.1 y.2) 
              (begin (set! rcx 0) (set! y.3 y.4)) 
              (set! y.5 0))
          (jump rax))))
      
    '(module
      ((new-frames (())) 
       (locals (y.5 x.2 y.4 x.3 y.3 x.4 y.2 x.5 y.1 x.1)))
      (begin
        (set! rax L.start.1)
        (set! x.1 x.2)
        (set! rax (+ rax x.3))
        (set! x.4 (+ x.4 x.5))
        (if (> y.1 y.2) (begin (set! rcx 0) (set! y.3 y.4)) (set! y.5 0))
        (jump rax)))))

(test-case "uncover 2 - pred with alocs"
  (check-equal?
    (uncover-locals
      '(module ((new-frames (()))) (if (> x.1 x.3) (jump rax) (jump x.4))))
    
    '(module
      ((new-frames (())) (locals (x.1 x.3 x.4)))
      (if (> x.1 x.3) (jump rax) (jump x.4)))))

(test-case "uncover 3 - function definitions, jump with aloc params"
  (check-equal?
    (uncover-locals
      '(module ((new-frames (()))) 
          (define L.start.1 ((new-frames (()))) (jump rax rax x.1 x.5 rcx x.3)) 
          (define L.start.3 ((new-frames (()))) (jump y.1 rax x.1 x.5 rcx x.3)) 
          (begin (set! x.1 (+ x.1 x.3)) (jump rax))))
    
    '(module
      ((new-frames (())) (locals (x.1 x.3)))
      (define L.start.1
        ((new-frames (())) (locals ()))
        (jump rax rax x.1 x.5 rcx x.3))
      (define L.start.3
        ((new-frames (())) (locals (y.1)))
        (jump y.1 rax x.1 x.5 rcx x.3))
      (begin (set! x.1 (+ x.1 x.3)) (jump rax)))))

(test-case "uncover 4 - pred is not"
  (check-equal?
    (uncover-locals
      '(module ((new-frames (())))
       (begin
         (set! x.1 0)
         (if (not (= x.3 0))
             (jump x.1)
             (jump x.2)))))
    
      '(module
        ((new-frames (())) (locals (x.1 x.2 x.3)))
        (begin (set! x.1 0) (if (not (= x.3 0)) (jump x.1) (jump x.2))))))

(test-case "uncover 5 - pred is begin"
  (check-equal?
    (uncover-locals
      '(module ((new-frames (())))
        (begin
          (if
            (begin (set! x.1 0) (set! x.2 1) (> x.2 x.1))
            (set! x.3 2)
            (set! x.3 3))
          (jump x.4))))
    
      '(module
        ((new-frames (())) (locals (x.1 x.2 x.3 x.4)))
        (begin
          (if (begin (set! x.1 0) (set! x.2 1) (> x.2 x.1))
            (set! x.3 2)
            (set! x.3 3))
          (jump x.4)))))

(test-case "uncover 6 - pred is if, tail is if, nested predicates"
  (check-equal?
    (uncover-locals 
      '(module ((new-frames (())))
        (begin
          (set! x.1 1)
          (if (if (not (= x.1 1))
                  (true)
                  (if (> x.1 0)
                      (false)
                      (begin
                        (set! y.2 2)
                        (begin
                          (set! z.3 3)
                          (if (< y.2 z.3)
                              (!= x.1 z.3)
                              (= x.1 y.2))))))
              (jump fv1)
              (jump fv2)))))
    
      '(module
        ((new-frames (())) (locals (x.1 z.3 y.2)))
        (begin
          (set! x.1 1)
          (if (if (not (= x.1 1))
                (true)
                (if (> x.1 0)
                  (false)
                  (begin
                    (set! y.2 2)
                    (begin (set! z.3 3) (if (< y.2 z.3) (!= x.1 z.3) (= x.1 y.2))))))
            (jump fv1)
            (jump fv2))))))

(test-case "uncover 7 - various return-points"
  (check-equal?
    (uncover-locals
      '(module ((new-frames (()))) 
               (begin 
                (set! rax (+ rax x.3))
                (return-point L.one.1 (jump y.1))
                (return-point L.two.2 (begin (set! y.2 y.3) (jump y.4)))
                (return-point L.three.3 (if (false) (jump y.5) (jump y.6)))
                (jump rax))))
      
    '(module
      ((new-frames (())) (locals (y.4 x.3 y.1 y.5 y.3 y.2 y.6)))
      (begin
        (set! rax (+ rax x.3))
        (return-point L.one.1 (jump y.1))
        (return-point L.two.2 (begin (set! y.2 y.3) (jump y.4)))
        (return-point L.three.3 (if (false) (jump y.5) (jump y.6)))
        (jump rax)))))

; M7 Tests

(test-case "uncover 8 - extend binops"
  (check-equal?
    (uncover-locals
      '(module ((new-frames (()))) (begin (set! rax (bitwise-and rax x.3)) (jump rax))))
      
    '(module
      ((new-frames (())) (locals (x.3)))
      (begin (set! rax (bitwise-and rax x.3)) (jump rax)))))

; M8 Tests

(test-case "uncover 9 - mset, effect is begin"
  (check-equal?
    (uncover-locals
      '(module 
        ((new-frames (()))) 
        (begin
          (mset! rsp 5 L.s.1)
          (mset! fv1 r14 6)
          (begin
            (mset! y.1 x.1 r15)
            (mset! y.2 fv2 fv3)
            (mset! y.3 fv4 x.3))
          (jump rax))))
      
    '(module
      ((new-frames (())) (locals (y.1 x.1 x.3 y.3 y.2)))
      (begin
        (mset! rsp 5 L.s.1)
        (mset! fv1 r14 6)
        (begin (mset! y.1 x.1 r15) (mset! y.2 fv2 fv3) (mset! y.3 fv4 x.3))
        (jump rax)))))

(test-case "uncover 10 - effect is mref"
  (check-equal?
    (uncover-locals
      '(module 
        ((new-frames (()))) 
        (begin
          (set! x.1 (mref fv1 5))
          (set! r14 (mref fv2 rbx))
          (set! fv3 (mref x.2 fv4))
          (set! x.3 (mref x.2 y.1))
          (jump rax))))
      
    '(module
      ((new-frames (())) (locals (y.1 x.1 x.2 x.3)))
      (begin
        (set! x.1 (mref fv1 5))
        (set! r14 (mref fv2 rbx))
        (set! fv3 (mref x.2 fv4))
        (set! x.3 (mref x.2 y.1))
        (jump rax)))))
