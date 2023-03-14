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

;locals sets are equivalent but check-equal checks for order
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
      ((locals (x.11 x.12 x.7 x.1 x.4)))
      (begin
        (set! rax L.start.1)
        (set! x.1 x.4)
        (if (true) (set! rax 0) (set! x.7 (+ x.7 x.4)))
        (if (> rax 100) (begin (set! rcx 0) (set! x.11 x.12)) (set! r10 0))
        (jump L.start.14 x.11 rax x.100)))))