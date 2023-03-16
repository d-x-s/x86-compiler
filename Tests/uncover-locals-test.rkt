#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt"
)

(test-case "uncover 1"
   (check-equal?
      (uncover-locals
        '(module () (halt 0)))
    
      '(module ((locals ())) (halt 0))))

(test-case "uncover 2"
  (check-equal?
    (uncover-locals
      '(module () (define L.start.1 () (halt x.2)) (jump x.1 rax)))
    
    '(module
        ((locals (x.1)))
        (define L.start.1 ((locals (x.2))) (halt x.2))
        (jump x.1 rax))))

(test-case "uncover 3"
  (check-equal?
    (uncover-locals
      '(module () (define L.start.1 () (halt x.2)) (begin (set! rax (+ rax x.3)) (halt rax))))
      
    '(module
      ((locals (x.3)))
      (define L.start.1 ((locals (x.2))) (halt x.2))
      (begin (set! rax (+ rax x.3)) (halt rax)))))

(test-case "uncover 4"
  (check-equal?
    (uncover-locals
      '(module () (define L.start.1 () (halt x.2)) (begin (set! x.1 (+ x.1 x.3)) (halt rax))))
    
    '(module
      ((locals (x.1 x.3)))
      (define L.start.1 ((locals (x.2))) (halt x.2))
      (begin (set! x.1 (+ x.1 x.3)) (halt rax)))))

(test-case "uncover 5"
  (check-equal?
    (uncover-locals
      '(module () (if (> x.1 x.3) (halt rax) (halt x.4))))
    
    '(module ((locals (x.4 x.3 x.1))) (if (> x.1 x.3) (halt rax) (halt x.4)))))

(test-case "uncover 6"
  (check-equal?
    (uncover-locals
      '(module () 
          (define L.start.1 () (halt x.2)) 
          (define L.start.3 () (halt x.7)) 
          (begin (set! x.1 (+ x.1 x.3)) (halt rax))))
    
    '(module ((locals (x.1 x.3)))
      (define L.start.1 ((locals (x.2))) (halt x.2))
      (define L.start.3 ((locals (x.7))) (halt x.7))
      (begin (set! x.1 (+ x.1 x.3)) (halt rax)))))

(test-case "uncover 7"
  (check-equal?
    (uncover-locals
      '(module () (jump rax rax x.1 x.5 rcx x.3)))
    
    '(module ((locals ())) (jump rax rax x.1 x.5 rcx x.3))))

(test-case "uncover 8"
  (check-equal?
    (uncover-locals
      '(module () (jump L.start.1 rax x.1 x.5 rcx x.3)))
    
    '(module ((locals ())) (jump L.start.1 rax x.1 x.5 rcx x.3))))

(test-case "uncover 9"
  (check-equal?
    (uncover-locals
      '(module () 
          (begin 
            (set! rax L.start.1)
            (set! x.1 x.4)
            (if (true) (set! rax 0) (set! x.7 (+ x.7 x.4)))
            (if (> rax 100) (begin (set! rcx 0) (set! x.11 x.12)) (set! r10 0))
            (jump L.start.14 x.11 rax x.100))))
    
    '(module
      ((locals (x.1 x.4 x.7 x.12 x.11))) 
      (begin
        (set! rax L.start.1)
        (set! x.1 x.4)
        (if (true) (set! rax 0) (set! x.7 (+ x.7 x.4)))
        (if (> rax 100) (begin (set! rcx 0) (set! x.11 x.12)) (set! r10 0))
        (jump L.start.14 x.11 rax x.100)))))

(test-case "uncover 10 - simple - no locals"
  (check-equal?
    (uncover-locals
      '(module () (begin (halt 5))))
    
      '(module ((locals ())) (begin (halt 5)))))

(test-case "uncover 11 - simple - single local value"
  (check-equal?
    (uncover-locals
      '(module () (begin (set! tmp.1 2) (set! tmp.1 (+ tmp.1 2)) (halt tmp.1))))
    
      '(module ((locals (tmp.1))) (begin (set! tmp.1 2) (set! tmp.1 (+ tmp.1 2)) (halt tmp.1)))))

(test-case "uncover 12 - predicate in tail"
  (check-equal?
    (uncover-locals
      '(module ()
       (begin
         (set! x.1 0)
         (if
          (not (= x.1 0))
          (halt 1)
          (halt 2)))))
    
      '(module ((locals (x.1)))
       (begin
         (set! x.1 0)
         (if
          (not (= x.1 0))
          (halt 1)
          (halt 2))))))

(test-case "uncover 13 - predicate in effect"
  (check-equal?
    (uncover-locals
      '(module ()
        (begin
          (if
            (begin
              (set! x.1 0)
              (set! x.2 1)
              (> x.2 x.1))
            (set! x.3 2)
            (set! x.3 3))
          (halt x.3))))
    
      '(module
        ((locals (x.2 x.1 x.3)))
        (begin
          (if (begin (set! x.1 0) (set! x.2 1) (> x.2 x.1))
            (set! x.3 2)
            (set! x.3 3))
          (halt x.3)))))

(test-case "uncover 14 - nested predicates"
  (check-equal?
    (uncover-locals '(module ()
       (begin
         (set! x.1 1)
         (if
          (if (not (= x.1 1))
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
          (halt 4)
          (halt 5)))))
    
      '(module
  ((locals (z.3 y.2 x.1)))
  (begin
    (set! x.1 1)
    (if (if (not (= x.1 1))
          (true)
          (if (> x.1 0)
            (false)
            (begin
              (set! y.2 2)
              (begin (set! z.3 3) (if (< y.2 z.3) (!= x.1 z.3) (= x.1 y.2))))))
      (halt 4)
      (halt 5))))))

(test-case "uncover 15 - single jump"
  (check-equal?
    (uncover-locals
      '(module
         ()
       (define L.a.1 () (begin
                          (set! x.5 6)
                          (begin
                            (set! x.3 6)
                            (set! y.6 10))
                          (set! x.2 2)
                          (halt x.2)))
       (begin
         (set! x.3 1)
         (set! x.2 2)
         (set! x.2 (+ x.2 x.3))
         (set! x.1 x.2)
         (set! x.1 (+ x.1 x.2))
         (set! z.5 L.a.1)
         (jump z.5 rdi rsi rdx))))
    
      '(module
        ((locals (x.1 x.2 x.3 z.5)))
        (define L.a.1
          ((locals (x.3 y.6 x.5 x.2)))
          (begin
            (set! x.5 6)
            (begin (set! x.3 6) (set! y.6 10))
            (set! x.2 2)
            (halt x.2)))
        (begin
          (set! x.3 1)
          (set! x.2 2)
          (set! x.2 (+ x.2 x.3))
          (set! x.1 x.2)
          (set! x.1 (+ x.1 x.2))
          (set! z.5 L.a.1)
          (jump z.5 rdi rsi rdx)))))

(test-case "uncover 16 - multiple jumps"
  (check-equal?
    (uncover-locals
      '(module
         ()
       (define L.a.1 () (begin
                          (set! x.5 6)
                          (begin
                            (set! x.3 6)
                            (set! y.6 10))
                          (set! x.2 2)
                          (jump L.b.1 rdi)))
       (define L.b.1 () (begin
                          (set! fv0 22)
                          (halt fv0)))
       (begin
         (set! x.3 1)
         (set! x.2 2)
         (set! x.2 (+ x.2 x.3))
         (set! x.1 x.2)
         (set! x.1 (+ x.1 x.2))
         (set! z.5 L.a.1)
         (jump z.5 rdi rsi rdx))))
    
      '(module
  ((locals (x.1 x.2 x.3 z.5)))
  (define L.a.1
    ((locals (x.2 x.3 y.6 x.5)))
    (begin
      (set! x.5 6)
      (begin (set! x.3 6) (set! y.6 10))
      (set! x.2 2)
      (jump L.b.1 rdi)))
  (define L.b.1 ((locals ())) (begin (set! fv0 22) (halt fv0)))
  (begin
    (set! x.3 1)
    (set! x.2 2)
    (set! x.2 (+ x.2 x.3))
    (set! x.1 x.2)
    (set! x.1 (+ x.1 x.2))
    (set! z.5 L.a.1)
    (jump z.5 rdi rsi rdx)))))