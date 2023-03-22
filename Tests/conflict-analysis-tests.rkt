#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt")

(test-case "conflict 1 - simple halt"
   (check-equal?
        (conflict-analysis
            `(module ((locals (v.1)) (undead-out ())) (halt v.1)))
        
        `(module ((locals (v.1)) (conflicts ((v.1 ())))) (halt v.1))))

(test-case "conflict 2 - simple begin"
   (check-equal?
        (conflict-analysis
            `(module ((locals (x.1))
                    (undead-out ((x.1) ())))
                    (begin
                        (set! x.1 42)
                        (halt x.1))))
        
        `(module
            ((locals (x.1)) (conflicts ((x.1 ()))))
            (begin (set! x.1 42) (halt x.1)))))

(test-case "conflict 3 - long begin"
   (check-equal?
        (conflict-analysis
            `(module ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1))
                        (undead-out
                            ((v.1)
                            (v.1 w.2)
                            (w.2 x.3)
                            (p.1 w.2 x.3)
                            (w.2 x.3)
                            (y.4 w.2 x.3)
                            (p.1 y.4 w.2 x.3)
                            (y.4 w.2 x.3)
                            (z.5 y.4 w.2)
                            (z.5 y.4)
                            (t.6 z.5)
                            (t.6 z.5 p.1)
                            (t.6 z.5)
                            (z.5)
                            ())))
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
        
        `(module
            ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1))
                (conflicts
                ((p.1 (z.5 t.6 y.4 x.3 w.2))
                (t.6 (p.1 z.5))
                (z.5 (p.1 t.6 w.2 y.4))
                (y.4 (z.5 x.3 p.1 w.2))
                (x.3 (y.4 p.1 w.2))
                (w.2 (z.5 y.4 p.1 x.3 v.1))
                (v.1 (w.2)))))
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

(test-case "conflict 4 - nested begin"
   (check-equal?
        (conflict-analysis
            `(module
                ((locals (x.1 w.2 p.1 t.6))
                    (undead-out ((t.6) (w.2 t.6) ((w.2 t.6) (t.6) ()))))
                (begin
                    (set! x.1 42)
                    (set! w.2 46)
                    (begin (set! p.1 -1) (set! t.6 (* t.6 w.2)) (halt t.6)))))
        
        `(module
            ((locals (x.1 w.2 p.1 t.6))
            (conflicts
                ((t.6 (p.1 w.2 x.1)) (p.1 (t.6 w.2)) (w.2 (p.1 t.6)) (x.1 (t.6)))))
            (begin
                (set! x.1 42)
                (set! w.2 46)
                (begin (set! p.1 -1) (set! t.6 (* t.6 w.2)) (halt t.6))))))

(test-case "conflict 5 - effect is begin"
   (check-equal?
        (conflict-analysis
            `(module
                ((locals (x.1 y.3 w.2 p.1 t.6))
                    (undead-out
                        ((t.6 y.3)
                        (w.2 t.6 y.3)
                        ((w.2 t.6 y.3) (y.3 t.6) ((y.3 t.6) (y.3 t.6) (t.6)) ()))))
                        (begin
                            (set! x.1 42)
                            (set! w.2 46)
                            (begin
                                (set! p.1 -1)
                                (set! t.6 (* t.6 w.2))
                                (begin (set! x.1 42) (set! w.2 46) (set! y.3 (+ y.3 2)))
                            (halt t.6)))))
        
        `(module
            ((locals (x.1 y.3 w.2 p.1 t.6))
             (conflicts
                ((t.6 (y.3 p.1 w.2 x.1))
                (p.1 (y.3 t.6 w.2))
                (w.2 (p.1 y.3 t.6))
                (y.3 (t.6 p.1 w.2 x.1))
                (x.1 (y.3 t.6)))))
            (begin
                (set! x.1 42)
                (set! w.2 46)
                (begin
                (set! p.1 -1)
                (set! t.6 (* t.6 w.2))
                (begin (set! x.1 42) (set! w.2 46) (set! y.3 (+ y.3 2)))
                (halt t.6))))))


; M4 tests

(test-case "conflict 6 - tail is if"
   (check-equal?
        (conflict-analysis
            `(module ((locals (x.1 y.2)) 
                      (undead-out ((x.1) (x.1 y.2) ((x.1 y.2) () ())))) 
                     (begin 
                        (set! x.1 3) 
                        (set! y.2 x.1) 
                        (if (> y.2 x.1) (halt x.1) (halt y.2)))))
        
        `(module
            ((locals (x.1 y.2)) 
             (conflicts (
                (y.2 ()) 
                (x.1 ()))))
            (begin (set! x.1 3) (set! y.2 x.1) (if (> y.2 x.1) (halt x.1) (halt y.2))))))

(test-case "conflict 7 - if branch is begin"
   (check-equal?
        (conflict-analysis
            `(module ((locals (x.1 y.2 b.3 c.4)) 
                      (undead-out ((x.1) (x.1 y.2) ((y.2 b.3) (b.3) (b.3 c.4) ((c.4) () ((c.4) ())))))) 
                     (begin 
                        (set! x.1 5) 
                        (set! y.2 x.1) 
                        (begin 
                            (set! b.3 x.1) 
                            (set! b.3 (+ b.3 y.2)) 
                            (set! c.4 b.3) 
                            (if (= c.4 b.3) (halt c.4) (begin (set! x.1 c.4) (halt c.4)))))))
        
        `(module
            ((locals (x.1 y.2 b.3 c.4))
             (conflicts (
                (c.4 ()) 
                (b.3 (y.2)) 
                (y.2 (b.3)) 
                (x.1 ()))))
            (begin
                (set! x.1 5)
                (set! y.2 x.1)
                (begin
                (set! b.3 x.1)
                (set! b.3 (+ b.3 y.2))
                (set! c.4 b.3)
                (if (= c.4 b.3) (halt c.4) (begin (set! x.1 c.4) (halt c.4))))))))

(test-case "conflict 8 - nested if"
   (check-equal?
        (conflict-analysis
            `(module 
                ((locals (tmp.83 z.39 tmp.82 y.38 x.37 tmp.84)) 
                 (undead-out 
                    ((x.37) 
                     (x.37 y.38) 
                     (((tmp.84 x.37 y.38) (x.37 y.38)) 
                      ((((y.38 z.39) ((z.39 tmp.82) ())) () ()) () ()) 
                      ((y.38 tmp.83) (tmp.83) ()))))) 
                (begin 
                    (set! x.37 20) 
                    (set! y.38 21) 
                    (if (not (begin (set! tmp.84 x.37) (> tmp.84 12))) 
                        (if (if (begin (set! z.39 x.37) (begin (set! tmp.82 y.38) (< tmp.82 z.39))) 
                                (true) 
                                (false)) 
                            (halt 10) 
                            (halt 12)) 
                        (begin (set! tmp.83 x.37) (set! tmp.83 (+ tmp.83 y.38)) (halt tmp.83)))))
        )
        `(module
            ((locals (tmp.83 z.39 tmp.82 y.38 x.37 tmp.84))
             (conflicts
                ((tmp.84 (y.38))
                (x.37 (y.38))
                (y.38 (tmp.83 z.39 tmp.84 x.37))
                (tmp.82 (z.39))
                (z.39 (tmp.82 y.38))
                (tmp.83 (y.38)))))
            (begin
                (set! x.37 20)
                (set! y.38 21)
                (if (not (begin (set! tmp.84 x.37) (> tmp.84 12)))
                (if (if (begin
                            (set! z.39 x.37)
                            (begin (set! tmp.82 y.38) (< tmp.82 z.39)))
                        (true)
                        (false))
                    (halt 10)
                    (halt 12))
                (begin (set! tmp.83 x.37) (set! tmp.83 (+ tmp.83 y.38)) (halt tmp.83)))))))

; M5 tests

(test-case "conflict 9 - tail is jump"
   (check-equal?
        (conflict-analysis
            `(module
                ((locals (x.1))
                (undead-out ((b.3 c.4 z.3 x.3 rsp) ((z.3 x.3 rsp) (rsp) (rsp)) (rsp))))
                (begin
                    (set! x.4 x.5)
                    (if (= c.4 b.3) (set! x.2 x.3) (set! z.5 z.3))
                    (jump L.start.1 rsp))))
        `(module
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
                (jump L.start.1 rsp)))))

(test-case "conflict 10 - jump with multiple params"
   (check-equal?
        (conflict-analysis
            `(module
                ((locals (x.1 y.2 x.4 x.5)) (undead-out ((x.1 rsp fv1 y.1) (rsp fv1 y.1))))
                (begin (set! x.4 x.5) (jump x.1 rsp fv1 y.1))))
        `(module
            ((locals (x.1 y.2 x.4 x.5))
            (conflicts
                ((x.5 ())
                (x.4 (y.1 fv1 rsp x.1))
                (y.2 ())
                (x.1 (x.4))
                (rsp (x.4))
                (fv1 (x.4))
                (y.1 (x.4)))))
            (begin (set! x.4 x.5) (jump x.1 rsp fv1 y.1)))))

(test-case "conflict 11 - define functions"
   (check-equal?
        (conflict-analysis
            `(module
                ((locals (x.1 y.2 x.4 x.5)) (undead-out ((x.1 y.1 fv1 rsp) (y.1 fv1 rsp))))
                (define L.start.1
                    ((locals (x.1 x.2)) (undead-out ((x.1) (x.1 x.2) (x.2) ())))
                    (begin (set! x.1 5) (set! x.2 10) (set! x.1 (+ x.1 x.2)) (halt x.2)))
                (define L.start.2
                    ((locals (x.1 y.2 b.3 c.4))
                    (undead-out (((c.4 x.1) (y.2 c.4 x.1) (c.4 x.1)) () ())))
                    (if (begin (set! x.1 2) (set! y.2 3) (> y.2 x.1)) (halt x.1) (halt c.4)))
                (begin (set! x.4 x.5) (jump x.1 rsp fv1 y.1))))
        `(module
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
            (begin (set! x.4 x.5) (jump x.1 rsp fv1 y.1)))))
