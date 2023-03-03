#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../../compiler.rkt"
)

 (test-case "patch 1"
   (check-equal?
   (patch-instructions
      '(begin
      (halt rax)))
      '(begin
        (set! rax rax)
        (jump done))
    )
 )

 (test-case "patch 2"
   (check-equal?
   (patch-instructions
   ' (begin 
   (set! fv0 0) 
   (set! fv1 1) 
   (compare fv0 fv1) 
   (jump-if > L.foo.1) 
   (halt 0)
   (with-label L.foo.1 (halt 1))))
      '(begin
  (set! fv0 0)
  (set! fv1 1)
  (set! r11 fv1)
  (set! r10 fv0)
  (compare r10 r11)
  (jump-if > L.foo.1)
  (set! rax 0)
  (jump done)
  (with-label L.foo.1 (set! rax 1))
  (jump done))
    )
 )

  (test-case "patch 3"
   (check-equal?
   (patch-instructions
   '(begin 
   (with-label L.main.51 (set! r14 1)) 
   (set! r15 5) 
   (with-label L.fact_loop.50 (compare r15 0)) 
   (jump-if = L.nested.54) 
   (set! r13 r15) 
   (set! r15 (+ r15 -1)) 
   (set! r14 (* r14 r13)) 
   (jump L.fact_loop.50) 
   (with-label L.nested.54 (halt r14))))
      '(begin
  (with-label L.main.51 (set! r14 1))
  (set! r15 5)
  (with-label L.fact_loop.50 (compare r15 0))
  (jump-if = L.nested.54)
  (set! r13 r15)
  (set! r15 (+ r15 -1))
  (set! r14 (* r14 r13))
  (jump L.fact_loop.50)
  (with-label L.nested.54 (set! rax r14))
  (jump done))
    )
 )


 (test-case "patch 4"
   (check-equal?
   (patch-instructions
      '(begin
      (jump-if < rax)))
      '(begin (jump-if >= L.tmp.1) (jump rax) (with-label L.tmp.1 (set! r10 r10)))
    )
 )
