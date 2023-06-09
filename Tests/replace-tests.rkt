#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt")


(test-case "replace 1"
  (check-equal?
   (replace-locations
    '(module
      ((locals (x.1)) (assignment ((x.1 rax))))
      (if (> x.1 0) (jump x.1) (jump 0))))
    
    '(module (if (> rax 0) (jump rax) (jump 0)))))

(test-case "replace 2"
  (check-equal?
    (replace-locations
      '(module 
        ((locals (foo.34 foo.35 bar.36 tmp.74))
         (conflicts ((tmp.74 (bar.36)) (bar.36 (tmp.74 foo.35 foo.34)) (foo.35 (bar.36 foo.34)) (foo.34 (bar.36 foo.35))))
         (assignment ((bar.36 r15) (foo.34 r14) (foo.35 r13) (tmp.74 r14))))
        (begin 
          (set! foo.34 1) 
          (set! foo.35 1) 
          (set! foo.35 (+ foo.35 foo.34)) 
          (set! bar.36 2) 
          (set! bar.36 (+ bar.36 foo.34)) 
          (set! tmp.74 foo.35) 
          (set! tmp.74 (+ tmp.74 bar.36)) 
          (jump tmp.74))))
    
    '(module
      (begin
        (set! r14 1)
        (set! r13 1)
        (set! r13 (+ r13 r14))
        (set! r15 2)
        (set! r15 (+ r15 r14))
        (set! r14 r13)
        (set! r14 (+ r14 r15))
        (jump r14)))))

(test-case "replace 3"
  (check-equal?
    (replace-locations
      '(module 
        ((locals (foo.1 bar.1 tmp.71))
         (conflicts ((tmp.71 (bar.1)) (bar.1 (tmp.71)) (foo.1 ()))) 
         (assignment ((bar.1 r15) (tmp.71 r14) (foo.1 r15)))) 
        (begin 
          (set! foo.1 1) 
          (set! bar.1 foo.1) 
          (set! tmp.71 foo.1) 
          (set! tmp.71 (+ tmp.71 bar.1)) 
          (jump tmp.71))))
    
    '(module
      (begin
        (set! r15 1)
        (set! r15 r15)
        (set! r14 r15)
        (set! r14 (+ r14 r15))
        (jump r14)))))

(test-case "replace 4"
  (check-equal?
    (replace-locations
      '(module 
        ((locals (bar.26 foo.24 bar.25 tmp.75)) 
         (conflicts ((tmp.75 (bar.25)) (bar.25 (tmp.75 foo.24)) (foo.24 (bar.25)) (bar.26 ()))) 
         (assignment ((bar.25 r15) (foo.24 r14) (tmp.75 r14) (bar.26 r15)))) 
        (begin 
          (set! bar.26 1) 
          (set! foo.24 bar.26) 
          (set! bar.25 2) 
          (set! tmp.75 foo.24) 
          (set! tmp.75 (+ tmp.75 bar.25)) 
          (jump tmp.75))))
    
    '(module
      (begin
        (set! r15 1)
        (set! r14 r15)
        (set! r15 2)
        (set! r14 r14)
        (set! r14 (+ r14 r15))
        (jump r14)))))

(test-case "replace 6"
  (check-equal?
    (replace-locations
      '(module 
        ((locals (x.5 x.4 x.6)) 
         (conflicts ((x.6 ()) (x.4 ()) (x.5 ()))) 
         (assignment ((x.5 r15) (x.4 r15) (x.6 r15)))) 
        (begin
          (set! x.6 2) 
          (set! x.5 1) 
          (set! x.4 x.5) 
          (set! x.4 2) 
          (set! x.6 x.4) 
          (jump 2))))
    
    '(module
      (begin
        (set! r15 2)
        (set! r15 1)
        (set! r15 r15)
        (set! r15 2)
        (set! r15 r15)
        (jump 2)))))

(test-case "replace 7"
  (check-equal?
    (replace-locations
      '(module 
        ((conflicts ((x.6 ()) (x.4 ()) (x.5 ()))) 
         (locals (x.5 x.4 x.6)) 
         (assignment ((x.5 r15) (x.4 r15) (x.6 r15)))) 
        (begin 
          (set! x.6 2) 
          (set! x.5 1) 
          (set! x.4 x.5) 
          (set! x.4 2) 
          (set! x.6 x.4) 
          (jump 2))))
    
    '(module
      (begin
        (set! r15 2)
        (set! r15 1)
        (set! r15 r15)
        (set! r15 2)
        (set! r15 r15)
        (jump 2)))))

(test-case "replace 8"
  (check-equal?
    (replace-locations
      '(module 
        ((locals (bar.26 foo.24 bar.25 tmp.75)) 
         (conflicts ((tmp.75 (bar.25)) (bar.25 (tmp.75 foo.24)) (foo.24 (bar.25)) (bar.26 ()))) 
         (assignment ((bar.25 r15) (foo.24 r14) (tmp.75 r14) (bar.26 r15))))
        (begin 
          (set! bar.26 1) 
          (set! foo.24 bar.26) 
          (set! bar.25 2) 
          (set! tmp.75 foo.24) 
          (set! tmp.75 (+ tmp.75 bar.25)) 
          (jump bar.25 foo.24 rax))))
    
    '(module
      (begin
        (set! r15 1)
        (set! r14 r15)
        (set! r15 2)
        (set! r14 r14)
        (set! r14 (+ r14 r15))
        (jump r15)))))

(test-case "replace 9"
  (check-equal?
    (replace-locations
      '(module
          ((locals (x.1)) 
           (assignment ((x.1 rax))))
          (define L.start.1 ((locals (x.3)) (assignment ((x.3 rcx)))) (jump x.3))
          (if (> x.1 0) (jump x.1) (jump 0))))
    
    '(module (define L.start.1 (jump rcx)) (if (> rax 0) (jump rax) (jump 0)))))

(test-case "replace 10 - basic traversal with return-point"
  (check-equal?
    (replace-locations
      '(module
        ((locals (x.1)) 
         (assignment ((x.1 rax))))
        (define L.start.1 ((locals (x.3)) (assignment ((x.3 rcx)))) 
                          (begin (return-point L.start.1 (jump x.3)) (jump rax)))
        (jump x.1)))
    
    '(module
      (define L.start.1 (begin (return-point L.start.1 (jump rcx)) (jump rax)))
      (jump rax))))

(test-case "replace 11 - basic traversal with return-point"
  (check-equal?
    (replace-locations
      '(module
          ((locals (x.1) (x.5)) 
           (assignment ((x.1 rax) (x.5 rcx))))
          (define L.start.1 ((locals (x.3)) (assignment ((x.3 rcx)))) 
                            (begin (return-point L.start.1 (jump x.3)) (jump rax)))
          (begin (return-point L.start.1 (jump x.5)) (jump x.5))))
    
    '(module
      (define L.start.1 (begin (return-point L.start.1 (jump rcx)) (jump rax)))
      (begin (return-point L.start.1 (jump rcx)) (jump rcx)))))

; M7 Tests

(test-case "replace 12 - extend binops"
  (check-equal?
    (replace-locations
      '(module
        ((locals (x.1)) 
         (assignment ((x.1 rax))))
        (define L.start.1 ((locals (x.1)) (assignment ((x.1 rcx)))) 
                          (begin (set! x.1 (bitwise-and x.1 2)) (jump rax)))
        (jump x.1)))
    
    '(module
      (define L.start.1 (begin (set! rcx (bitwise-and rcx 2)) (jump rax)))
      (jump rax))))

; M8 Tests

(test-case "replace 13 - mset"
  (check-equal?
    (replace-locations
      '(module
        ((locals (x.1 x.3 y.1 y.2 y.3)) 
         (assignment ((x.1 rsp) (x.3 rbx) (y.1 rcx) (y.2 rdx) (y.3 rsi))))
        (begin
          (mset! rsp 5 L.s.1)
          (mset! fv1 r14 6)
          (mset! y.1 x.1 r15)
          (mset! y.2 fv2 fv3)
          (mset! y.3 fv4 x.3)
          (jump L.s.1))))
    
    '(module
      (begin
        (mset! rsp 5 L.s.1)
        (mset! fv1 r14 6)
        (mset! rcx rsp r15)
        (mset! rdx fv2 fv3)
        (mset! rsi fv4 rbx)
        (jump L.s.1)))))

(test-case "replace 14 - mref"
  (check-equal?
    (replace-locations
      '(module
        ((locals (x.1 x.2 x.3 y.1)) 
         (assignment ((x.1 rsp) (x.2 rbx) (x.3 rcx) (y.1 rdx))))
        (begin
          (set! x.1 (mref fv1 5))
          (set! r14 (mref fv2 rbx))
          (set! fv3 (mref x.2 fv4))
          (set! x.3 (mref x.2 y.1))
          (jump L.s.1))))
    
    '(module
      (begin
        (set! rsp (mref fv1 5))
        (set! r14 (mref fv2 rbx))
        (set! fv3 (mref rbx fv4))
        (set! rcx (mref rbx rdx))
        (jump L.s.1)))))