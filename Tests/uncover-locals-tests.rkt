#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt")

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
