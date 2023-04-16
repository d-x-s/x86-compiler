#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt")

 (test-case "implement-expose-allocation 0 - stack smash"
    (check-equal?
        (expose-allocation-pointer
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
       (return-point L.rp.4
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
       (return-point L.rp.5
         (begin
           (set! rsi b.23)
           (set! rdi a.24)
           (set! r15 L.rp.5)
           (jump L.+.5 rbp r15 rdi rsi)))
       (set! r1.25 rax)
       (return-point L.rp.6
         (begin
           (set! rsi c.22)
           (set! rdi r1.25)
           (set! r15 L.rp.6)
           (jump L.+.5 rbp r15 rdi rsi)))
       (set! r2.26 rax)
       (return-point L.rp.7
         (begin
           (set! rsi d.21)
           (set! rdi r2.26)
           (set! r15 L.rp.7)
           (jump L.+.5 rbp r15 rdi rsi)))
       (set! r3.27 rax)
       (return-point L.rp.8
         (begin
           (set! rsi e.20)
           (set! rdi r3.27)
           (set! r15 L.rp.8)
           (jump L.+.5 rbp r15 rdi rsi)))
       (set! r4.28 rax)
       (return-point L.rp.9
         (begin
           (set! rsi f.19)
           (set! rdi r4.28)
           (set! r15 L.rp.9)
           (jump L.+.5 rbp r15 rdi rsi)))
       (set! r5.29 rax)
       (return-point L.rp.10
         (begin
           (set! rsi g.18)
           (set! rdi r5.29)
           (set! r15 L.rp.10)
           (jump L.+.5 rbp r15 rdi rsi)))
       (set! r6.30 rax)
       (return-point L.rp.11
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
)

(test-case "implement-expose-allocation 1 - alloc in tail position"
    (check-equal?
        (expose-allocation-pointer
        '(module
            ((new-frames ()))
            (begin
                (set! x.1 (alloc 10))
                (jump L.done.1)
            )
            )
        )
      '(module
            ((new-frames ()))
                (begin 
                    (begin (set! x.1 r12) (set! r12 (+ r12 10))) 
                    (jump L.done.1)))
    )
)

(test-case "implement-expose-allocation 2 - alloc in tail position, with new frames"
    (check-equal?
        (expose-allocation-pointer
        '(module
            ((new-frames ((x.1) (y.2) (z.3))))
            (begin
                (set! x.1 (alloc 10))
                (jump L.done.1)
            )
            )
        )
      '(module
        ((new-frames ((x.1) (y.2) (z.3))))
        (begin (begin (set! x.1 r12) (set! r12 (+ r12 10))) (jump L.done.1)))
    )
)

(test-case "implement-expose-allocation 3 - trivial, nothing to transform"
    (check-equal?
        (expose-allocation-pointer
        '(module
            ((new-frames ((x.1) (y.2) (z.3))))
            (begin
                (set! x.1 10)
                (jump L.done.1)
            )
            )
        )
      '(module
        ((new-frames ((x.1) (y.2) (z.3))))
        (begin (set! x.1 10) (jump L.done.1)))
    )
)

(test-case "implement-expose-allocation 4 - alloc in pred position"
    (check-equal?
        (expose-allocation-pointer
        '(module
            ((new-frames ((x.1) (y.2) (z.3))))
            (if (begin (set! rax (alloc 10)) (true)) 
                (jump L.done.1) 
                (jump L.done.2)
            )
            )
        )
      '(module
        ((new-frames ((x.1) (y.2) (z.3))))
        (if (begin (begin (set! rax r12) (set! r12 (+ r12 10))) (true))
            (jump L.done.1)
            (jump L.done.2)))
    )
)

(test-case "implement-expose-allocation 5 - alloc in return-point"
    (check-equal?
        (expose-allocation-pointer
        '(module
            ((new-frames ((x.1) (y.2) (z.3))))
            (begin
                (return-point L.done.1             
                   (if (begin (set! rax (alloc 10)) (true)) 
                (jump L.done.1) 
                (jump L.done.2)
            ))
                (jump L.done.1)
            )
            )
        )
      '(module
        ((new-frames ((x.1) (y.2) (z.3))))
        (begin
            (return-point
            L.done.1
            (if (begin (begin (set! rax r12) (set! r12 (+ r12 10))) (true))
            (jump L.done.1)
            (jump L.done.2)))
            (jump L.done.1)))
    )
)
