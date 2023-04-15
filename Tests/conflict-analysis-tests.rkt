#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt")

; M6 Tests

(test-case "conflict 1 - long begin"
   (check-equal?
        (conflict-analysis
            `(module ((new-frames (())) 
                      (locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1))
                      (call-undead ()) 
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
                            (jump z.5))))
        
        `(module
            ((new-frames (())) (locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1))
             (call-undead ())
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
                ())) 
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
                (jump z.5)))))

(test-case "conflict 2 - nested begin"
   (check-equal?
        (conflict-analysis
            `(module
                ((new-frames (())) (locals (x.1 w.2 p.1 t.6))
                 (call-undead ()) 
                 (undead-out ((t.6) (w.2 t.6) ((w.2 t.6) (t.6) ()))))
                (begin
                    (set! x.1 42)
                    (set! w.2 46)
                    (begin (set! p.1 -1) (set! t.6 (* t.6 w.2)) (jump t.6)))))
        
        `(module
            ((new-frames (())) (locals (x.1 w.2 p.1 t.6))
             (call-undead ())
             (undead-out ((t.6) (w.2 t.6) ((w.2 t.6) (t.6) ())))
             (conflicts
                ((t.6 (p.1 w.2 x.1)) 
                 (p.1 (t.6 w.2)) 
                 (w.2 (p.1 t.6)) 
                 (x.1 (t.6)))))
            (begin
                (set! x.1 42)
                (set! w.2 46)
                (begin (set! p.1 -1) (set! t.6 (* t.6 w.2)) (jump t.6))))))

(test-case "conflict 3 - effect is begin"
   (check-equal?
        (conflict-analysis
            `(module
                ((new-frames (())) (locals (x.1 y.3 w.2 p.1 t.6))
                    (call-undead ()) 
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
                            (jump t.6)))))
        
        `(module
            ((new-frames (())) (locals (x.1 y.3 w.2 p.1 t.6))
             (call-undead ())
             (undead-out
                ((t.6 y.3)
                (w.2 t.6 y.3)
                ((w.2 t.6 y.3) (y.3 t.6) ((y.3 t.6) (y.3 t.6) (t.6)) ())))
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
                (jump t.6))))))

(test-case "conflict 4 - tail is if"
   (check-equal?
        (conflict-analysis
            `(module ((new-frames (())) (locals (x.1 y.2)) 
                      (call-undead ()) 
                      (undead-out ((x.1) (x.1 y.2) ((x.1 y.2) () ())))) 
                     (begin 
                        (set! x.1 3) 
                        (set! y.2 x.1) 
                        (if (> y.2 x.1) (jump x.1) (jump y.2)))))
        
        `(module
            ((new-frames (())) (locals (x.1 y.2)) 
             (call-undead ())
             (undead-out ((x.1) (x.1 y.2) ((x.1 y.2) () ())))
             (conflicts (
                (y.2 ()) 
                (x.1 ()))))
            (begin (set! x.1 3) (set! y.2 x.1) (if (> y.2 x.1) (jump x.1) (jump y.2))))))

(test-case "conflict 5 - if branch is begin"
   (check-equal?
        (conflict-analysis
            `(module ((new-frames (())) (locals (x.1 y.2 b.3 c.4)) 
                      (call-undead ()) 
                      (undead-out ((x.1) (x.1 y.2) ((y.2 b.3) (b.3) (b.3 c.4) ((c.4) () ((c.4) ())))))) 
                     (begin 
                        (set! x.1 5) 
                        (set! y.2 x.1) 
                        (begin 
                            (set! b.3 x.1) 
                            (set! b.3 (+ b.3 y.2)) 
                            (set! c.4 b.3) 
                            (if (= c.4 b.3) (jump c.4) (begin (set! x.1 c.4) (jump c.4)))))))
        
        `(module
            ((new-frames (())) (locals (x.1 y.2 b.3 c.4))
             (call-undead ())
             (undead-out ((x.1) (x.1 y.2) ((y.2 b.3) (b.3) (b.3 c.4) ((c.4) () ((c.4) ())))))
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
                (if (= c.4 b.3) (jump c.4) (begin (set! x.1 c.4) (jump c.4))))))))

(test-case "conflict 6 - nested if"
   (check-equal?
        (conflict-analysis
            `(module 
                ((new-frames (())) (locals (tmp.83 z.39 tmp.82 y.38 x.37 tmp.84)) 
                 (call-undead ()) 
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
                            (jump L.tmp.1) 
                            (jump L.tmp.2)) 
                        (begin (set! tmp.83 x.37) (set! tmp.83 (+ tmp.83 y.38)) (jump tmp.83))))))

        `(module
            ((new-frames (())) (locals (tmp.83 z.39 tmp.82 y.38 x.37 tmp.84))
             (call-undead ())
             (undead-out 
                ((x.37) 
                    (x.37 y.38) 
                    (((tmp.84 x.37 y.38) (x.37 y.38)) 
                    ((((y.38 z.39) ((z.39 tmp.82) ())) () ()) () ()) 
                    ((y.38 tmp.83) (tmp.83) ()))))
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
                    (jump L.tmp.1)
                    (jump L.tmp.2))
                (begin (set! tmp.83 x.37) (set! tmp.83 (+ tmp.83 y.38)) (jump tmp.83)))))))

(test-case "conflict 7 - tail is jump"
   (check-equal?
        (conflict-analysis
            `(module
                ((new-frames (())) (locals (x.1))
                 (call-undead ()) 
                 (undead-out ((b.3 c.4 z.3 x.3 rsp) ((z.3 x.3 rsp) (rsp) (rsp)) (rsp))))
                (begin
                    (set! x.4 x.5)
                    (if (= c.4 b.3) (set! x.2 x.3) (set! z.5 z.3))
                    (jump L.start.1 rsp))))
        `(module
            ((new-frames (())) (locals (x.1))
             (call-undead ())
             (undead-out ((b.3 c.4 z.3 x.3 rsp) ((z.3 x.3 rsp) (rsp) (rsp)) (rsp)))
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

(test-case "conflict 8 - jump with multiple params"
   (check-equal?
        (conflict-analysis
            `(module
                ((new-frames (())) (locals (x.1 y.2 x.4 x.5)) (call-undead ()) 
                 (undead-out ((x.1 rsp fv1 y.1) (rsp fv1 y.1))))
                (begin (set! x.4 x.5) (jump x.1 rsp fv1 y.1))))
        `(module
            ((new-frames (())) (locals (x.1 y.2 x.4 x.5))
             (call-undead ())
             (undead-out ((x.1 rsp fv1 y.1) (rsp fv1 y.1)))
             (conflicts
                ((x.5 ())
                (x.4 (y.1 fv1 rsp x.1))
                (y.2 ())
                (x.1 (x.4))
                (rsp (x.4))
                (fv1 (x.4))
                (y.1 (x.4)))))
            (begin (set! x.4 x.5) (jump x.1 rsp fv1 y.1)))))

(test-case "conflict 9 - define functions"
   (check-equal?
        (conflict-analysis
            `(module
                ((new-frames (())) 
                 (locals (x.1 y.2 x.4 x.5)) 
                 (call-undead ()) 
                 (undead-out ((x.1 y.1 fv1 rsp) (y.1 fv1 rsp))))
                (define L.start.1
                    ((new-frames (())) (locals (x.1 x.2)) (call-undead ()) 
                     (undead-out ((x.1) (x.1 x.2) (x.2) ())))
                    (begin (set! x.1 5) (set! x.2 10) (set! x.1 (+ x.1 x.2)) (jump x.2)))
                (define L.start.2
                    ((new-frames (())) (locals (x.1 y.2 b.3 c.4)) (call-undead ()) 
                     (undead-out (((c.4 x.1) (y.2 c.4 x.1) (c.4 x.1)) () ())))
                    (if (begin (set! x.1 2) (set! y.2 3) (> y.2 x.1)) (jump x.1) (jump c.4)))
                (begin (set! x.4 x.5) (jump x.1 rsp fv1 y.1))))
        `(module
            ((new-frames (())) (locals (x.1 y.2 x.4 x.5))
             (call-undead ())
             (undead-out ((x.1 y.1 fv1 rsp) (y.1 fv1 rsp)))
             (conflicts
                ((x.5 ())
                (x.4 (rsp fv1 y.1 x.1))
                (y.2 ())
                (x.1 (x.4))
                (y.1 (x.4))
                (fv1 (x.4))
                (rsp (x.4)))))
            (define L.start.1
                ((new-frames (())) (locals (x.1 x.2))
                 (call-undead ())
                 (undead-out ((x.1) (x.1 x.2) (x.2) ()))
                 (conflicts ((x.2 (x.1)) (x.1 (x.2)))))
                (begin (set! x.1 5) (set! x.2 10) (set! x.1 (+ x.1 x.2)) (jump x.2)))
            (define L.start.2
                ((new-frames (())) (locals (x.1 y.2 b.3 c.4))
                (call-undead ())
                (undead-out (((c.4 x.1) (y.2 c.4 x.1) (c.4 x.1)) () ()))
                (conflicts ((c.4 (y.2 x.1)) (b.3 ()) (y.2 (x.1 c.4)) (x.1 (y.2 c.4)))))
                (if (begin (set! x.1 2) (set! y.2 3) (> y.2 x.1)) (jump x.1) (jump c.4)))
            (begin (set! x.4 x.5) (jump x.1 rsp fv1 y.1)))))

(test-case "conflict 10 - binop is subtraction"
   (check-equal?
        (conflict-analysis
            `(module
                ((new-frames (())) (locals (x.1 w.2 p.1 t.6))
                 (call-undead ()) 
                 (undead-out ((t.6) (w.2 t.6) ((w.2 t.6) (t.6) ()))))
                (begin
                    (set! x.1 42)
                    (set! w.2 46)
                    (begin (set! p.1 -1) (set! t.6 (- t.6 w.2)) (jump t.6)))))
        
        `(module
            ((new-frames (())) (locals (x.1 w.2 p.1 t.6))
             (call-undead ())
             (undead-out ((t.6) (w.2 t.6) ((w.2 t.6) (t.6) ())))
             (conflicts
                ((t.6 (p.1 w.2 x.1)) 
                 (p.1 (t.6 w.2)) 
                 (w.2 (p.1 t.6)) 
                 (x.1 (t.6)))))
            (begin
                (set! x.1 42)
                (set! w.2 46)
                (begin (set! p.1 -1) (set! t.6 (- t.6 w.2)) (jump t.6))))))

(test-case "conflict 11 - simple return-points"
   (check-equal?
        (conflict-analysis
            `(module
                ((new-frames (()))
                (locals (x.1 x.3 p.1 y.1 y.2 y.3 y.4 y.5 y.6))
                (call-undead (y.4 y.6 x.1 y.3 y.5))
                (undead-out
                    ((x.3 p.1 y.4 y.3 x.1 y.6 y.5 y.1)
                    (y.4 y.3 x.1 y.6 y.5 y.1)
                    ((y.5 y.6 x.1 y.3 y.4) ())
                    ((x.1 y.6 y.5) ((y.4) ()))
                    ((x.1) ((y.6 y.5) () ()))
                    ())))
                (begin
                    (set! x.1 42)
                    (set! x.3 (- x.3 p.1))
                    (return-point L.one.1 (jump y.1))
                    (return-point L.two.2 (begin (set! y.2 y.3) (jump y.4)))
                    (return-point L.three.3 (if (false) (jump y.5) (jump y.6)))
                    (jump x.1))))
        
        `(module
            ((new-frames (()))
            (locals (x.1 x.3 p.1 y.1 y.2 y.3 y.4 y.5 y.6))
            (call-undead (y.4 y.6 x.1 y.3 y.5))
            (undead-out
                ((x.3 p.1 y.4 y.3 x.1 y.6 y.5 y.1)
                (y.4 y.3 x.1 y.6 y.5 y.1)
                ((y.5 y.6 x.1 y.3 y.4) ())
                ((x.1 y.6 y.5) ((y.4) ()))
                ((x.1) ((y.6 y.5) () ()))
                ()))
            (conflicts
                ((y.6 (x.3 x.1))
                (y.5 (x.3 x.1))
                (y.4 (y.2 x.3 x.1))
                (y.3 (x.3 x.1))
                (y.2 (y.4))
                (y.1 (x.3 x.1))
                (p.1 (x.1))
                (x.3 (y.1 y.5 y.6 y.3 y.4 x.1))
                (x.1 (y.1 y.5 y.6 y.3 y.4 p.1 x.3)))))
            (begin
                (set! x.1 42)
                (set! x.3 (- x.3 p.1))
                (return-point L.one.1 (jump y.1))
                (return-point L.two.2 (begin (set! y.2 y.3) (jump y.4)))
                (return-point L.three.3 (if (false) (jump y.5) (jump y.6)))
                (jump x.1)))))

(test-case "conflict 12 - nested tail in return point"
   (check-equal?
        (conflict-analysis
            `(module
                ((new-frames (()))
                (locals (x.1 x.3 p.1 y.1 y.2 y.3 z.5 y.5 y.6))
                (call-undead (x.1))
                (undead-out
                    ((x.3 p.1 x.1 y.3 z.5 y.6 y.5)
                    (x.1 y.3 z.5 y.6 y.5)
                    ((x.1)
                    ((y.3 z.5 y.6 y.5)
                    (z.5 y.6 y.5)
                    ((z.5 p.1 y.6 y.5) (y.6 y.5))
                    ((y.6 y.5) () ())))
                    ())))
                (begin
                    (set! x.1 42)
                    (set! x.3 (- x.3 p.1))
                    (return-point
                    L.two.2
                    (begin
                    (set! y.1 2)
                    (set! y.2 y.3)
                    (begin (set! p.1 7) (set! z.5 (+ z.5 p.1)))
                    (if (false) (jump y.5) (jump y.6))))
                    (jump x.1))))
        
        `(module
            ((new-frames (()))
            (locals (x.1 x.3 p.1 y.1 y.2 y.3 z.5 y.5 y.6))
            (call-undead (x.1))
            (undead-out
                ((x.3 p.1 x.1 y.3 z.5 y.6 y.5)
                (x.1 y.3 z.5 y.6 y.5)
                ((x.1)
                ((y.3 z.5 y.6 y.5)
                (z.5 y.6 y.5)
                ((z.5 p.1 y.6 y.5) (y.6 y.5))
                ((y.6 y.5) () ())))
                ()))
            (conflicts
                ((y.6 (z.5 p.1 y.2 y.1 x.3 x.1))
                (y.5 (z.5 p.1 y.2 y.1 x.3 x.1))
                (z.5 (y.5 y.6 p.1 y.2 y.1 x.3 x.1))
                (y.3 (y.1 x.3 x.1))
                (y.2 (y.5 y.6 z.5))
                (y.1 (y.5 y.6 z.5 y.3))
                (p.1 (y.5 y.6 z.5 x.1))
                (x.3 (y.5 y.6 z.5 y.3 x.1))
                (x.1 (y.5 y.6 z.5 y.3 p.1 x.3)))))
            (begin
                (set! x.1 42)
                (set! x.3 (- x.3 p.1))
                (return-point
                L.two.2
                (begin
                (set! y.1 2)
                (set! y.2 y.3)
                (begin (set! p.1 7) (set! z.5 (+ z.5 p.1)))
                (if (false) (jump y.5) (jump y.6))))
                (jump x.1)))))

(test-case "conflict 13 - functions with return points"
   (check-equal?
        (conflict-analysis
            `(module
                ((new-frames (()))
                (locals (x.1 y.2 x.4 x.5 z.3))
                (call-undead (fv1 y.1 x.1))
                (undead-out ((y.1 fv1 rsp x.1 z.3) ((x.1 rsp fv1 y.1) ()) (rsp fv1 y.1))))
                (define L.start.1
                    ((new-frames (()))
                    (locals (x.1 x.2 z.1))
                    (undead-out ((x.1 z.1) (x.1 x.2 z.1) (x.2 z.1) ((x.2) ()) ()))
                    (call-undead (x.2)))
                    (begin
                    (set! x.1 5)
                    (set! x.2 10)
                    (set! x.1 (+ x.1 x.2))
                    (return-point L.one.1 (jump z.1))
                    (jump x.2)))
                (define L.start.2
                    ((new-frames (()))
                    (locals (x.1 y.2 b.3 c.4 z.2))
                    (undead-out
                    (((x.1 c.4 z.2) (x.1 c.4 y.2 z.2) ((y.2 c.4 x.1) ()) (c.4 x.1)) () ()))
                    (call-undead (y.2 c.4 x.1)))
                    (if (begin
                        (set! x.1 2)
                        (set! y.2 3)
                        (return-point L.two.2 (jump z.2))
                        (> y.2 x.1))
                    (jump x.1)
                    (jump c.4)))
                (begin
                    (set! x.4 x.5)
                    (return-point L.three.3 (jump z.3))
                    (jump x.1 rsp fv1 y.1))))
        
        `(module
            ((new-frames (()))
            (locals (x.1 y.2 x.4 x.5 z.3))
            (call-undead (fv1 y.1 x.1))
            (undead-out ((y.1 fv1 rsp x.1 z.3) ((x.1 rsp fv1 y.1) ()) (rsp fv1 y.1)))
            (conflicts
                ((z.3 (x.4))
                (x.5 ())
                (x.4 (z.3 x.1 rsp fv1 y.1))
                (y.2 ())
                (x.1 (x.4))
                (y.1 (x.4))
                (fv1 (x.4))
                (rsp (x.4)))))
            (define L.start.1
                ((new-frames (()))
                (locals (x.1 x.2 z.1))
                (undead-out ((x.1 z.1) (x.1 x.2 z.1) (x.2 z.1) ((x.2) ()) ()))
                (call-undead (x.2))
                (conflicts ((z.1 (x.2 x.1)) (x.2 (z.1 x.1)) (x.1 (x.2 z.1)))))
                (begin
                (set! x.1 5)
                (set! x.2 10)
                (set! x.1 (+ x.1 x.2))
                (return-point L.one.1 (jump z.1))
                (jump x.2)))
            (define L.start.2
                ((new-frames (()))
                (locals (x.1 y.2 b.3 c.4 z.2))
                (undead-out
                (((x.1 c.4 z.2) (x.1 c.4 y.2 z.2) ((y.2 c.4 x.1) ()) (c.4 x.1)) () ()))
                (call-undead (y.2 c.4 x.1))
                (conflicts
                ((z.2 (y.2 x.1))
                (c.4 (y.2 x.1))
                (b.3 ())
                (y.2 (z.2 c.4 x.1))
                (x.1 (y.2 z.2 c.4)))))
                (if (begin
                    (set! x.1 2)
                    (set! y.2 3)
                    (return-point L.two.2 (jump z.2))
                    (> y.2 x.1))
                (jump x.1)
                (jump c.4)))
            (begin
                (set! x.4 x.5)
                (return-point L.three.3 (jump z.3))
                (jump x.1 rsp fv1 y.1)))))

(test-case "conflict 14 - complex test"
   (check-equal?
        (conflict-analysis
            `(module
                ((new-frames ())
                (locals (ra.12))
                (call-undead ())
                (undead-out ((ra.12 rbp) (ra.12 rbp fv0) (rbp r15 fv0) (rbp r15 fv0))))
                (define L.fact.4
                    ((new-frames ((nfv.16)))
                    (locals (ra.13 x.9 tmp.14 tmp.15 new-n.10 nfv.16 factn-1.11 tmp.17))
                    (undead-out
                    ((r15 x.9 rbp)
                    (x.9 ra.13 rbp)
                    ((x.9 ra.13 rbp)
                        ((ra.13 rbp rax) (rbp rax))
                        ((tmp.14 ra.13 x.9 rbp)
                        (tmp.14 tmp.15 ra.13 x.9 rbp)
                        (tmp.15 ra.13 x.9 rbp)
                        (ra.13 x.9 new-n.10 rbp)
                        ((rax x.9 ra.13 rbp) ((rbp nfv.16) (rbp r15 nfv.16) (rbp r15 nfv.16)))
                        (x.9 factn-1.11 ra.13 rbp)
                        (factn-1.11 tmp.17 ra.13 rbp)
                        (tmp.17 ra.13 rbp)
                        (ra.13 rbp rax)
                        (rbp rax)))))
                    (call-undead (x.9 ra.13)))
                    (begin
                    (set! x.9 fv0)
                    (set! ra.13 r15)
                    (if (= x.9 0)
                        (begin (set! rax 1) (jump ra.13 rbp rax))
                        (begin
                        (set! tmp.14 -1)
                        (set! tmp.15 x.9)
                        (set! tmp.15 (+ tmp.15 tmp.14))
                        (set! new-n.10 tmp.15)
                        (return-point
                            L.rp.6
                            (begin
                                (set! nfv.16 new-n.10)
                                (set! r15 L.rp.6)
                                (jump L.fact.4 rbp r15 nfv.16)))
                        (set! factn-1.11 rax)
                        (set! tmp.17 x.9)
                        (set! tmp.17 (* tmp.17 factn-1.11))
                        (set! rax tmp.17)
                        (jump ra.13 rbp rax)))))
                (begin
                    (set! ra.12 r15)
                    (set! fv0 5)
                    (set! r15 ra.12)
                    (jump L.fact.4 rbp r15 fv0))))
        
        `(module
            ((new-frames ())
            (locals (ra.12))
            (call-undead ())
            (undead-out ((ra.12 rbp) (ra.12 rbp fv0) (rbp r15 fv0) (rbp r15 fv0)))
            (conflicts
                ((ra.12 (fv0 rbp))
                (rbp (r15 fv0 ra.12))
                (fv0 (r15 rbp ra.12))
                (r15 (fv0 rbp)))))
            (define L.fact.4
                ((new-frames ((nfv.16)))
                (locals (ra.13 x.9 tmp.14 tmp.15 new-n.10 nfv.16 factn-1.11 tmp.17))
                (undead-out
                ((r15 x.9 rbp)
                (x.9 ra.13 rbp)
                ((x.9 ra.13 rbp)
                    ((ra.13 rbp rax) (rbp rax))
                    ((tmp.14 ra.13 x.9 rbp)
                    (tmp.14 tmp.15 ra.13 x.9 rbp)
                    (tmp.15 ra.13 x.9 rbp)
                    (ra.13 x.9 new-n.10 rbp)
                    ((rax x.9 ra.13 rbp) ((rbp nfv.16) (rbp r15 nfv.16) (rbp r15 nfv.16)))
                    (x.9 factn-1.11 ra.13 rbp)
                    (factn-1.11 tmp.17 ra.13 rbp)
                    (tmp.17 ra.13 rbp)
                    (ra.13 rbp rax)
                    (rbp rax)))))
                (call-undead (x.9 ra.13))
                (conflicts
                ((tmp.17 (rbp ra.13 factn-1.11))
                (factn-1.11 (tmp.17 rbp ra.13 x.9))
                (nfv.16 (r15 rbp))
                (new-n.10 (rbp x.9 ra.13))
                (tmp.15 (x.9 rbp ra.13 tmp.14))
                (tmp.14 (tmp.15 rbp x.9 ra.13))
                (x.9 (factn-1.11 new-n.10 tmp.15 tmp.14 ra.13 rbp r15))
                (ra.13 (tmp.17 factn-1.11 new-n.10 tmp.15 tmp.14 rax rbp x.9))
                (r15 (nfv.16 rbp x.9))
                (rbp
                    (tmp.17 factn-1.11 r15 nfv.16 new-n.10 tmp.15 tmp.14 rax ra.13 x.9))
                (rax (rbp ra.13)))))
                (begin
                (set! x.9 fv0)
                (set! ra.13 r15)
                (if (= x.9 0)
                    (begin (set! rax 1) (jump ra.13 rbp rax))
                    (begin
                    (set! tmp.14 -1)
                    (set! tmp.15 x.9)
                    (set! tmp.15 (+ tmp.15 tmp.14))
                    (set! new-n.10 tmp.15)
                    (return-point
                    L.rp.6
                    (begin
                        (set! nfv.16 new-n.10)
                        (set! r15 L.rp.6)
                        (jump L.fact.4 rbp r15 nfv.16)))
                    (set! factn-1.11 rax)
                    (set! tmp.17 x.9)
                    (set! tmp.17 (* tmp.17 factn-1.11))
                    (set! rax tmp.17)
                    (jump ra.13 rbp rax)))))
            (begin
                (set! ra.12 r15)
                (set! fv0 5)
                (set! r15 ra.12)
                (jump L.fact.4 rbp r15 fv0)))))

; M7 Tests

(test-case "conflict 15 - extend binops"
   (check-equal?
        (conflict-analysis
            `(module
                ((new-frames (())) (locals (x.1 w.2 p.1 t.6))
                 (call-undead ()) 
                 (undead-out ((t.6) (w.2 t.6) ((w.2 t.6) (t.6) ()))))
                (begin
                    (set! x.1 42)
                    (set! w.2 46)
                    (begin (set! p.1 -1) (set! t.6 (bitwise-xor t.6 w.2)) (jump t.6)))))
        
        `(module
            ((new-frames (()))
            (locals (x.1 w.2 p.1 t.6))
            (call-undead ())
            (undead-out ((t.6) (w.2 t.6) ((w.2 t.6) (t.6) ())))
            (conflicts
                ((t.6 (p.1 w.2 x.1)) (p.1 (t.6 w.2)) (w.2 (p.1 t.6)) (x.1 (t.6)))))
            (begin
                (set! x.1 42)
                (set! w.2 46)
                (begin (set! p.1 -1) (set! t.6 (bitwise-xor t.6 w.2)) (jump t.6))))))

; M8 Tests

(test-case "conflict 16 - effect is mref"
   (check-equal?
        (conflict-analysis
            `(module
                ((new-frames (()))
                (locals (y.1 x.1 x.2 x.3))
                (call-undead ())
                (undead-out
                    ((fv2 rbx fv4 x.2 y.1 rax)
                    (fv4 x.2 y.1 rax)
                    (x.2 y.1 rax)
                    (x.3 y.1 rax)
                    (rax)
                    ())))
                (begin
                    (set! x.1 (mref fv1 5))
                    (set! r14 (mref fv2 rbx))
                    (set! fv3 (mref x.2 fv4))
                    (set! x.3 (mref x.2 y.1))
                    (set! x.1 (mref x.3 y.1))
                    (jump rax))))
        
        `(module
            ((new-frames (()))
            (locals (y.1 x.1 x.2 x.3))
            (call-undead ())
            (undead-out
                ((fv2 rbx fv4 x.2 y.1 rax)
                (fv4 x.2 y.1 rax)
                (x.2 y.1 rax)
                (x.3 y.1 rax)
                (rax)
                ()))
            (conflicts
                ((x.3 (rax y.1))
                (x.2 (fv3 r14 x.1))
                (x.1 (rax y.1 x.2 fv4 rbx fv2))
                (y.1 (x.3 fv3 r14 x.1))
                (fv2 (x.1))
                (rbx (x.1))
                (fv4 (r14 x.1))
                (rax (x.3 fv3 r14 x.1))
                (r14 (rax y.1 x.2 fv4))
                (fv3 (rax y.1 x.2)))))
            (begin
                (set! x.1 (mref fv1 5))
                (set! r14 (mref fv2 rbx))
                (set! fv3 (mref x.2 fv4))
                (set! x.3 (mref x.2 y.1))
                (set! x.1 (mref x.3 y.1))
                (jump rax)))))

(test-case "conflict 17 - effect is mset"
   (check-equal?
        (conflict-analysis
            `(module
                ((new-frames (()))
                (locals (y.1 x.1 x.3 y.3 y.2))
                (call-undead ())
                (undead-out
                    ((fv4 y.2 r14 fv1 r15 x.1 y.1 fv3 fv2 rsp x.3 y.3 rax)
                    (r14 fv1 r15 x.1 y.1 fv3 fv2 rsp x.3 y.3 rax)
                    (r15 x.1 y.1 fv3 fv2 rsp x.3 y.3 rax)
                    (fv3 fv2 rsp x.3 y.3 rax)
                    (x.3 y.3 rax)
                    (rax)
                    ())))
                (begin
                    (set! r14 L.s.1)
                    (mset! y.2 fv4 L.s.1)
                    (mset! fv1 r14 6)
                    (mset! y.1 x.1 r15)
                    (mset! rsp fv2 fv3)
                    (mset! y.3 5 x.3)
                    (jump rax))))
        
        `(module
            ((new-frames (()))
            (locals (y.1 x.1 x.3 y.3 y.2))
            (call-undead ())
            (undead-out
                ((fv4 y.2 r14 fv1 r15 x.1 y.1 fv3 fv2 rsp x.3 y.3 rax)
                (r14 fv1 r15 x.1 y.1 fv3 fv2 rsp x.3 y.3 rax)
                (r15 x.1 y.1 fv3 fv2 rsp x.3 y.3 rax)
                (fv3 fv2 rsp x.3 y.3 rax)
                (x.3 y.3 rax)
                (rax)
                ()))
            (conflicts
                ((y.2 (r14))
                (y.3 (r14))
                (x.3 (r14))
                (x.1 (r14))
                (y.1 (r14))
                (r14 (rax y.3 x.3 rsp fv2 fv3 y.1 x.1 r15 fv1 y.2 fv4))
                (fv4 (r14))
                (fv1 (r14))
                (r15 (r14))
                (fv3 (r14))
                (fv2 (r14))
                (rsp (r14))
                (rax (r14)))))
            (begin
                (set! r14 L.s.1)
                (mset! y.2 fv4 L.s.1)
                (mset! fv1 r14 6)
                (mset! y.1 x.1 r15)
                (mset! rsp fv2 fv3)
                (mset! y.3 5 x.3)
                (jump rax)))))
