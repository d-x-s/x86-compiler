#lang racket

(require
 cpsc411/compiler-lib
 rackunit "compiler.rkt"
)

(test-case "allocate fvars"
    (check-equal?
     (allocate-fvars 3)
     '(fv0 fv1 fv2)
    )
)

(test-case "construct registers 1"
    (check-equal?
     (construct-registers '((x.1 ())))
    '(r15 r14 r13 r9 r8 rdi rsi rdx rcx rbx rsp fv0)
    )
)

(test-case "construct registers 2"
    (check-equal?
     (construct-registers    
      '((x.3 (z.5 p.1 y.4 v.1 w.2))
        (w.2 (z.5 p.1 y.4 v.1 x.3))
        (v.1 (w.2 x.3))
        (y.4 (t.6 z.5 p.1 w.2 x.3))
        (p.1 (t.6 z.5 y.4 w.2 x.3))
        (z.5 (t.6 p.1 y.4 w.2 x.3))
        (t.6 (z.5 p.1 y.4))))
    '(r15 r14 r13 r9 r8 rdi rsi rdx rcx rbx rsp fv0 fv1 fv2 fv3 fv4 fv5 fv6)
    )
)

(test-case "assign-registers 1"
    (check-equal? 
        (assign-registers
   '(module ((locals (x.1))
             (conflicts ((x.1 ()))))
     (begin
       (set! x.1 42)
       (halt x.1))))
        '(module

   ((locals (x.1)) (conflicts ((x.1 ()))) (assignment ((x.1 r15))))

   (begin (set! x.1 42) (halt x.1)))))

(test-case "assign-registers 2"
    (check-equal?
     (assign-registers
      '(module 
        ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1))
         (conflicts
            ((x.3 (z.5 p.1 y.4 v.1 w.2))
            (w.2 (z.5 p.1 y.4 v.1 x.3))
            (v.1 (w.2 x.3))
            (y.4 (t.6 z.5 p.1 w.2 x.3))
            (p.1 (t.6 z.5 y.4 w.2 x.3))
            (z.5 (t.6 p.1 y.4 w.2 x.3))
            (t.6 (z.5 p.1 y.4)))))
       (begin 
            (set! v.1 1) 
            (set! w.2 46) 
            (set! x.3 v.1) 
            (set! p.1 7) 
            (set! x.3 (+ x.3 p.1)) 
            (set! y.4 x.3) 
            (set! p.1 4) 
            (set! y.4 (+ y.4 p.1)) 
            (set! z.5 x.3) 
            (set! z.5 (+ z.5 w.2)) 
            (set! t.6 y.4) 
            (set! p.1 -1) 
            (set! t.6 (* t.6 p.1)) 
            (set! z.5 (+ z.5 t.6)) 
            (halt z.5))))
     '(module
        ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1))
         (conflicts
            ((x.3 (z.5 p.1 y.4 v.1 w.2))
            (w.2 (z.5 p.1 y.4 v.1 x.3))
            (v.1 (w.2 x.3))
            (y.4 (t.6 z.5 p.1 w.2 x.3))
            (p.1 (t.6 z.5 y.4 w.2 x.3))
            (z.5 (t.6 p.1 y.4 w.2 x.3))
            (t.6 (z.5 p.1 y.4))))
         (assignment
            ((z.5 r15)
            (p.1 r14)
            (y.4 r13)
            (w.2 r9)
            (x.3 r8)
            (t.6 r9)
            (v.1 r15))))
        (begin
            (set! v.1 1)
            (set! w.2 46)
            (set! x.3 v.1)
            (set! p.1 7)
            (set! x.3 (+ x.3 p.1))
            (set! y.4 x.3)
            (set! p.1 4)
            (set! y.4 (+ y.4 p.1))
            (set! z.5 x.3)
            (set! z.5 (+ z.5 w.2))
            (set! t.6 y.4)
            (set! p.1 -1)
            (set! t.6 (* t.6 p.1))
            (set! z.5 (+ z.5 t.6))
            (halt z.5)))
))