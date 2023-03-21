#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt"
)

 (test-case "patch 1 - simple addresses"
   (check-equal?
   (patch-instructions
    '(begin 
      (set! rax 10)
      (set! rax L.tmp.1)
      (set! (rbp - 8) L.tmp.2)
      (set! (rbp - 8) rcx)
     )
    ) 
    '(begin
        (set! rax 10)
        (set! rax L.tmp.1)
        (set! r10 L.tmp.2)
        (set! (rbp - 8) r10)
        (set! (rbp - 8) rcx))
    )
 )

 (test-case "patch 2 - set! with address replacement cases"
   (check-equal?
   (patch-instructions
    '(begin 
        (set! (rbp - 8) L.tmp.2)
        (set! (rbp - 8) (rbp - 16))
        (set! (rbp - 8) 10)
        (set! (rbp - 8) rcx) ; not a case that needs replacing
        )
    )
    '(begin
        (set! r10 L.tmp.2)
        (set! (rbp - 8) r10)
        (set! r10 (rbp - 16))
        (set! (rbp - 8) r10)
        (set! (rbp - 8) 10)
        (set! (rbp - 8) rcx))
    )
 )

 (test-case "patch 3 - jump with address replacement cases"
   (check-equal?
    (patch-instructions
    '(begin 
        (jump L.tmp.1)      ; not a case taht needs replacing
        (jump rax)          ; not a case that needs replacing
        (jump (rbp - 8))
        )
    )
      
    '(begin 
        (jump L.tmp.1) 
        (jump rax) 
        (set! r10 (rbp - 8)) 
        (jump r10))

    )
 )

 (test-case "patch 4 - complex jump and single block"
   (check-equal?
   (patch-instructions
   ' (begin 
       (set! (rbp - 8) 0) 
       (set! (rbp - 16) 1) 
       (compare (rbp - 8) (rbp - 16)) 
       (jump-if > L.foo.1) 
       (with-label L.foo.1 (jump rax))
     )
   )

      '(begin
        (set! (rbp - 8) 0)
        (set! (rbp - 16) 1)
        (set! r11 (rbp - 16))
        (set! r10 (rbp - 8))
        (compare r10 r11)
        (jump-if > L.foo.1)
        (with-label L.foo.1 (jump rax)))
    )
 )

  (test-case "patch 5 - complex jump and multiple blocks"
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
   (set! r14 (- r14 r13)) 
   (jump L.fact_loop.50) 
   (with-label L.nested.54 (jump rax))))

      '(begin
  (with-label L.main.51 (set! r14 1))
  (set! r15 5)
  (with-label L.fact_loop.50 (compare r15 0))
  (jump-if = L.nested.54)
  (set! r13 r15)
  (set! r15 (+ r15 -1))
  (set! r14 (* r14 r13))
  (set! r14 (- r14 r13))
  (jump L.fact_loop.50)
  (with-label L.nested.54 (jump rax)))
    )
 )

 (test-case "patch 6 - simple jump-if"
   (check-equal?
   (patch-instructions
      '(begin
      (jump-if < rax)))
      '(begin (jump-if >= L.tmp.1) (jump rax) (with-label L.tmp.1 (set! r10 r10)))
    )
 )