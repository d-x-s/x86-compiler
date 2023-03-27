#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt")


(test-case "implement 1 - no fvars"
   (check-equal?
        (implement-fvars
 '(module (begin (set! r15 r15) (jump r15))))

        
        `(module (begin (set! r15 r15) (jump r15)))))

(test-case "implement 1.5 - basic traversal"
   (check-equal?
        (implement-fvars
 '(module (begin (set! rbp (- rbp 8)) (jump fv1))))

        
        `(module (begin (set! rbp (- rbp 8)) (jump (rbp - 0))))))

(test-case "implement 2 - basic traversal"
   (check-equal?
        (implement-fvars
 '(module (define L.fact.1 (begin (set! fv0 r15) (set! fv1 rdi) (if (= fv1 0) (begin (set! rax 1) (jump fv0)) (begin (set! r15 fv1) (set! r15 (+ r15 -1)) (begin (set! rbp (- rbp 16)) (return-point L.rp.21 (begin (set! rdi r15) (set! r15 L.rp.21) (jump L.fact.1))) (set! rbp (+ rbp 16))) (set! r15 rax) (set! rax fv1) (set! rax (* rax r15)) (jump fv0))))) (begin (set! r15 r15) (set! rdi 5) (set! r15 r15) (jump L.fact.1))))

        
        `(module
  (define L.fact.1
    (begin
      (set! (rbp - 0) r15)
      (set! (rbp - 8) rdi)
      (if (= (rbp - 8) 0)
        (begin (set! rax 1) (jump (rbp - 0)))
        (begin
          (set! r15 (rbp - 8))
          (set! r15 (+ r15 -1))
          (begin
            (set! rbp (- rbp 16))
            (return-point
             L.rp.21
             (begin (set! rdi r15) (set! r15 L.rp.21) (jump L.fact.1)))
            (set! rbp (+ rbp 16)))
          (set! r15 rax)
          (set! rax (rbp - 8))
          (set! rax (* rax r15))
          (jump (rbp - 0))))))
  (begin (set! r15 r15) (set! rdi 5) (set! r15 r15) (jump L.fact.1)))))



(test-case "implement 3 - basic traversal"
   (check-equal?
        (implement-fvars '(module
  (define L.swap.1
   (begin
    (set! fv4 r15)
    (set! r14 fv0)
    (set! r15 fv1)
    (if (< r15 r14)
      (begin
       (set! rax r14)
       (jump fv4))
      (begin
       (begin
        (set! fv0 40)
        (set! rax (+ rax fv0))
        (return-point L.rp.1
         (begin
          (set! fv5 r15)
          (set! fv6 r14)
          (set! r15 L.rp.1)
          (jump L.swap.1)))
        (set! rax (+ rax 40)))
       (jump fv4)))))
  (begin
   (set! fv1 7)
   (set! fv0 4)
   (jump L.swap.1))))

        
        `(module
  (define L.swap.1
    (begin
      (set! (rbp - 32) r15)
      (set! r14 (rbp - 0))
      (set! r15 (rbp - 8))
      (if (< r15 r14)
        (begin (set! rax r14) (jump (rbp - 32)))
        (begin
          (begin
            (set! (rbp - 0) 40)
            (set! rax (+ rax (rbp - 0)))
            (return-point
             L.rp.1
             (begin
               (set! (rbp - 40) r15)
               (set! (rbp - 48) r14)
               (set! r15 L.rp.1)
               (jump L.swap.1)))
            (set! rax (+ rax 40)))
          (jump (rbp - 32))))))
  (begin (set! (rbp - 8) 7) (set! (rbp - 0) 4) (jump L.swap.1)))))


(test-case "implement 4 - basic fbp offset"
   (check-equal?
        (implement-fvars
 '(module  
 (begin
  (set! rbp (- rbp 8))
  (set! rdi fv3)
  (jump L.r.8))
))

`(module (begin (set! rbp (- rbp 8)) (set! rdi (rbp - 16)) (jump L.r.8)))))


(test-case "implement 5 - nested fbp offset"
   (check-equal?
(implement-fvars '(module
  (define L.swap.1
   (begin
    (set! fv4 r15)
    (set! r14 fv0)
    (set! r15 fv1)
    (if (< r15 r14)
      (begin
       (set! rbp (- rbp 8))
       (set! rax r14)
       (jump fv4))
      (begin
       (begin
        (set! fv0 40)
        (set! rax (+ rax fv0))
        (return-point L.rp.1
         (begin
          (set! fv5 r15)
          (set! fv6 r14)
          (set! r15 L.rp.1)
          (jump L.swap.1)))
        (set! rax (+ rax 40)))
       (jump fv4)))))
  (begin
   (set! fv1 7)
   (set! fv0 4)
   (jump L.swap.1))))

`(module
  (define L.swap.1
    (begin
      (set! (rbp - 32) r15)
      (set! r14 (rbp - 0))
      (set! r15 (rbp - 8))
      (if (< r15 r14)
        (begin (set! rbp (- rbp 8)) (set! rax r14) (jump (rbp - 24)))
        (begin
          (begin
            (set! (rbp - 0) 40)
            (set! rax (+ rax (rbp - 0)))
            (return-point
             L.rp.1
             (begin
               (set! (rbp - 40) r15)
               (set! (rbp - 48) r14)
               (set! r15 L.rp.1)
               (jump L.swap.1)))
            (set! rax (+ rax 40)))
          (jump (rbp - 32))))))
  (begin (set! (rbp - 8) 7) (set! (rbp - 0) 4) (jump L.swap.1)))

))

(test-case "implement 6 - multiple nested fbp offset"
   (check-equal?
(implement-fvars 
`(module 
    (define L.start.1 
    (begin 
        (set! rbp (- rbp 8)) 
        (set! fv1 10) 
        (begin 
            (return-point
            L.rp.1
            (begin
            (set! rbp (- rbp 16)) 
            (set! fv5 30)
            (jump L.start.1)))
            (if (true)
                (return-point
                L.rp.2
                (begin (set! rbp (+ rbp 8))
                       (set! fv11 4)
                       (jump L.start.1)))
                (set! fv11 4)))
        (jump fv0)))
        (jump fv0)))

`(module
  (define L.start.1
    (begin
      (set! rbp (- rbp 8))
      (set! (rbp - 0) 10)
      (begin
        (return-point
         L.rp.1
         (begin (set! rbp (- rbp 16)) (set! (rbp - 16) 30) (jump L.start.1)))
        (if (true)
          (return-point
           L.rp.2
           (begin (set! rbp (+ rbp 8)) (set! (rbp - 88) 4) (jump L.start.1)))
          (set! (rbp - 80) 4)))
      (jump (rbp - -8))))
  (jump (rbp - 0)))
))