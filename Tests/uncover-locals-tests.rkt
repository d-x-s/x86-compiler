#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt")

; M6 Tests

(test-case "uncover 1 - set loc to binop with aloc"
  (check-equal?
    (uncover-locals
      '(module ((new-frames (()))) (begin (set! rax (+ rax x.3)) (jump rax))))
      
    '(module
      ((new-frames (())) (locals (x.3)))
      (begin (set! rax (+ rax x.3)) (jump rax)))))

(test-case "uncover 2 - set loc to binop with multiple alocs"
  (check-equal?
    (uncover-locals
      '(module ((new-frames (()))) (begin (set! x.1 (+ x.1 x.3)) (jump rax))))
    
    '(module
      ((new-frames (())) (locals (x.1 x.3)))
      (begin (set! x.1 (+ x.1 x.3)) (jump rax)))))

(test-case "uncover 3 - pred with alocs"
  (check-equal?
    (uncover-locals
      '(module ((new-frames (()))) (if (> x.1 x.3) (jump rax) (jump x.4))))
    
    '(module
      ((new-frames (())) (locals (x.4 x.3 x.1)))
      (if (> x.1 x.3) (jump rax) (jump x.4)))))

(test-case "uncover 4 - function definitions, jump with multiple params"
  (check-equal?
    (uncover-locals
      '(module ((new-frames (()))) 
          (define L.start.1 ((new-frames (()))) (jump rax rax x.1 x.5 rcx x.3)) 
          (define L.start.3 ((new-frames (()))) (jump L.start.1 rax x.1 x.5 rcx x.3)) 
          (begin (set! x.1 (+ x.1 x.3)) (jump rax))))
    
    '(module
        ((new-frames (())) (locals (x.1 x.3)))
        (define L.start.1
          ((new-frames (())) (locals ()))
          (jump rax rax x.1 x.5 rcx x.3))
        (define L.start.3
          ((new-frames (())) (locals ()))
          (jump L.start.1 rax x.1 x.5 rcx x.3))
        (begin (set! x.1 (+ x.1 x.3)) (jump rax)))))

(test-case "uncover 5 - set and pred with label, register"
  (check-equal?
    (uncover-locals
      '(module ((new-frames (())))  
          (begin 
            (set! rax L.start.1)
            (set! x.1 x.4)
            (if (true) (set! rax 0) (set! x.7 (+ x.7 x.4)))
            (if (> rax 100) (begin (set! rcx 0) (set! x.11 x.12)) (set! r10 0))
            (jump L.start.14 x.11 rax x.100))))
    
    '(module
      ((new-frames (())) (locals (x.1 x.4 x.7 x.12 x.11)))
      (begin
        (set! rax L.start.1)
        (set! x.1 x.4)
        (if (true) (set! rax 0) (set! x.7 (+ x.7 x.4)))
        (if (> rax 100) (begin (set! rcx 0) (set! x.11 x.12)) (set! r10 0))
        (jump L.start.14 x.11 rax x.100)))))

(test-case "uncover 6 - simple - single local value"
  (check-equal?
    (uncover-locals
      '(module 
        ((new-frames (()))) 
        (begin (set! tmp.1 2) (set! tmp.1 (+ tmp.1 2)) (jump tmp.1))))
    
      '(module
        ((new-frames (())) (locals (tmp.1)))
        (begin (set! tmp.1 2) (set! tmp.1 (+ tmp.1 2)) (jump tmp.1)))))

(test-case "uncover 7 - not predicate in tail"
  (check-equal?
    (uncover-locals
      '(module ((new-frames (())))
       (begin
         (set! x.1 0)
         (if (not (= x.3 0))
             (jump x.1)
             (jump x.2)))))
    
      '(module
        ((new-frames (())) (locals (x.2 x.1 x.3)))
        (begin (set! x.1 0) (if (not (= x.3 0)) (jump x.1) (jump x.2))))))

(test-case "uncover 8 - predicate in effect"
  (check-equal?
    (uncover-locals
      '(module ((new-frames (())))
        (begin
          (if
            (begin
              (set! x.1 0)
              (set! x.2 1)
              (> x.2 x.1))
            (set! x.3 2)
            (set! x.3 3))
          (jump x.4))))
    
      '(module
        ((new-frames (())) (locals (x.4 x.2 x.1 x.3)))
        (begin
          (if (begin (set! x.1 0) (set! x.2 1) (> x.2 x.1))
            (set! x.3 2)
            (set! x.3 3))
          (jump x.4)))))

(test-case "uncover 9 - nested predicates"
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
        ((new-frames (())) (locals (z.3 y.2 x.1)))
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

(test-case "uncover 10 - multiple jumps"
  (check-equal?
    (uncover-locals
      '(module
          ((new-frames (())))
          (define L.a.1 ((new-frames (()))) (begin
                              (set! x.5 6)
                              (begin
                                (set! x.3 6)
                                (set! y.6 10))
                              (set! x.2 2)
                              (jump L.b.1 rdi)))
          (define L.b.1 ((new-frames (()))) (begin
                              (set! fv0 22)
                              (jump fv0)))
          (begin
            (set! x.3 1)
            (set! x.2 2)
            (set! x.2 (+ x.2 x.3))
            (set! x.1 x.2)
            (set! x.1 (+ x.1 x.2))
            (set! z.5 L.a.1)
            (jump z.5 rdi rsi rdx))))
    
      '(module
        ((new-frames (())) (locals (x.3 x.1 x.2 z.5)))
        (define L.a.1
          ((new-frames (())) (locals (x.5 x.3 y.6 x.2)))
          (begin
            (set! x.5 6)
            (begin (set! x.3 6) (set! y.6 10))
            (set! x.2 2)
            (jump L.b.1 rdi)))
        (define L.b.1
          ((new-frames (())) (locals ()))
          (begin (set! fv0 22) (jump fv0)))
        (begin
          (set! x.3 1)
          (set! x.2 2)
          (set! x.2 (+ x.2 x.3))
          (set! x.1 x.2)
          (set! x.1 (+ x.1 x.2))
          (set! z.5 L.a.1)
          (jump z.5 rdi rsi rdx)))))

(test-case "uncover 11 - various return-points"
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
      ((new-frames (())) (locals (x.3 y.1 y.3 y.2 y.4 y.5 y.6)))
      (begin
        (set! rax (+ rax x.3))
        (return-point L.one.1 (jump y.1))
        (return-point L.two.2 (begin (set! y.2 y.3) (jump y.4)))
        (return-point L.three.3 (if (false) (jump y.5) (jump y.6)))
        (jump rax)))))

(test-case "uncover 12 - non-empty new-frames"
  (check-equal?
    (uncover-locals
      '(module
        ((new-frames ((nfv.1))))
        (define L.swap.1
          ((new-frames ((nfv.2 nfv.3))))
          (begin
            (set! tmp-ra.1 r15)
            (set! x.1 fv0)
            (set! y.2 fv1)
            (if (< y.2 x.1)
              (begin (set! rax x.1) (jump tmp-ra.1 rbp rax))
              (begin
                (return-point
                L.rp.1
                (begin
                  (set! nfv.3 x.1)
                  (set! nfv.2 y.2)
                  (set! r15 L.rp.1)
                  (jump L.swap.1 rbp r15 nfv.2 nfv.3)))
                (set! z.3 rax)
                (set! rax z.3)
                (jump tmp-ra.1 rbp rax)))))
        (begin
          (set! tmp-ra.4 r15)
          (set! fv1 2)
          (set! fv0 1)
          (set! r15 tmp-ra.4)
          (jump L.swap.1 rbp r15 fv0 fv1))))
      
    '(module
      ((new-frames ((nfv.1))) (locals (tmp-ra.4)))
      (define L.swap.1
        ((new-frames ((nfv.2 nfv.3))) (locals (z.3 nfv.3 nfv.2 tmp-ra.1 x.1 y.2)))
        (begin 
          (set! tmp-ra.1 r15)
          (set! x.1 fv0)
          (set! y.2 fv1)
          (if (< y.2 x.1)
            (begin (set! rax x.1) (jump tmp-ra.1 rbp rax))
            (begin
              (return-point
              L.rp.1
              (begin
                (set! nfv.3 x.1)
                (set! nfv.2 y.2)
                (set! r15 L.rp.1)
                (jump L.swap.1 rbp r15 nfv.2 nfv.3)))
              (set! z.3 rax)
              (set! rax z.3)
              (jump tmp-ra.1 rbp rax)))))
      (begin
        (set! tmp-ra.4 r15)
        (set! fv1 2)
        (set! fv0 1)
        (set! r15 tmp-ra.4)
        (jump L.swap.1 rbp r15 fv0 fv1)))))