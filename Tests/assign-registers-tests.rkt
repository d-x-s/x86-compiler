#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt"
)

; M3/M4 Tests
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
            ((z.5 r15) (p.1 r14) (y.4 r13) (w.2 r9) (x.3 r8) (t.6 r9) (v.1 r15))))
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
            (halt z.5)))))

; M5 Tests
(test-case "assign-registers 3"
    (check-equal? 
        (assign-registers 
        '(module
            ((locals (x.1))
            (conflicts
                ((x.1 ())
                (x.4 (rsp x.3 z.3 c.4 b.3))
                (b.3 (x.4))
                (c.4 (x.4))
                (z.3 (x.4))
                (x.3 (x.4))
                (rsp (z.5 x.2 x.4))
                (x.2 (rsp))
                (z.5 (rsp)))))
            (begin
                (set! x.4 x.5)
                (if (= c.4 b.3) (set! x.2 x.3) (set! z.5 z.3))
                (jump L.start.1 rsp))))

        '(module
            ((locals (x.1))
            (conflicts
                ((x.1 ())
                (x.4 (rsp x.3 z.3 c.4 b.3))
                (b.3 (x.4))
                (c.4 (x.4))
                (z.3 (x.4))
                (x.3 (x.4))
                (rsp (z.5 x.2 x.4))
                (x.2 (rsp))
                (z.5 (rsp))))
            (assignment ((x.1 r15))))
            (begin
                (set! x.4 x.5)
                (if (= c.4 b.3) (set! x.2 x.3) (set! z.5 z.3))
                (jump L.start.1 rsp)))))

(test-case "assign-registers 4"
    (check-equal? 
        (assign-registers 
        '(module
            ((locals (x.1 y.2 x.4 x.5))
            (conflicts
                ((x.5 ())
                (x.4 (y.1 fv1 rsp x.1))
                (y.2 ())
                (x.1 (x.4))
                (rsp (x.4))
                (fv1 (x.4))
                (y.1 (x.4)))))
            (begin (set! x.4 x.5) (jump x.1 rsp fv1 y.1))))

         '(module
            ((locals (x.1 y.2 x.4 x.5))
                (conflicts
                ((x.5 ())
                (x.4 (y.1 fv1 rsp x.1))
                (y.2 ())
                (x.1 (x.4))
                (rsp (x.4))
                (fv1 (x.4))
                (y.1 (x.4))))
                (assignment ((x.4 r15) (x.1 r14) (y.2 r15) (x.5 r15))))
            (begin (set! x.4 x.5) (jump x.1 rsp fv1 y.1)))))

(test-case "assign-registers 5"
    (check-equal? 
        (assign-registers 
            '(module
            ((locals (x.1 y.2 x.4 x.5))
            (conflicts
                ((x.5 ())
                (x.4 (rsp fv1 y.1 x.1))
                (y.2 ())
                (x.1 (x.4))
                (y.1 (x.4))
                (fv1 (x.4))
                (rsp (x.4)))))
            (define L.start.1
                ((locals (x.1 x.2))
                (conflicts ((x.2 (x.1)) (x.1 (x.2)))))
                (begin (set! x.1 5) (set! x.2 10) (set! x.1 (+ x.1 x.2)) (halt x.2)))
            (define L.start.2
                ((locals (x.1 y.2 b.3 c.4))
                (conflicts ((c.4 (y.2 x.1)) (b.3 ()) (y.2 (x.1 c.4)) (x.1 (y.2 c.4)))))
                (if (begin (set! x.1 2) (set! y.2 3) (> y.2 x.1)) (halt x.1) (halt c.4)))
            (begin (set! x.4 x.5) (jump x.1 rsp fv1 y.1))))

        '(module
            ((locals (x.1 y.2 x.4 x.5))
                (conflicts
                ((x.5 ())
                (x.4 (rsp fv1 y.1 x.1))
                (y.2 ())
                (x.1 (x.4))
                (y.1 (x.4))
                (fv1 (x.4))
                (rsp (x.4))))
                (assignment ((x.4 r15) (x.1 r14) (y.2 r15) (x.5 r15))))
            (define L.start.1
                ((locals (x.1 x.2))
                (conflicts ((x.2 (x.1)) (x.1 (x.2))))
                (assignment ((x.1 r15) (x.2 r14))))
                (begin (set! x.1 5) (set! x.2 10) (set! x.1 (+ x.1 x.2)) (halt x.2)))
            (define L.start.2
                ((locals (x.1 y.2 b.3 c.4))
                (conflicts ((c.4 (y.2 x.1)) (b.3 ()) (y.2 (x.1 c.4)) (x.1 (y.2 c.4))))
                (assignment ((x.1 r15) (y.2 r14) (c.4 r13) (b.3 r15))))
                (if (begin (set! x.1 2) (set! y.2 3) (> y.2 x.1)) (halt x.1) (halt c.4)))
            (begin (set! x.4 x.5) (jump x.1 rsp fv1 y.1)))))

