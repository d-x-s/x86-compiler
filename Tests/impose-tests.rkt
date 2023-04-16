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