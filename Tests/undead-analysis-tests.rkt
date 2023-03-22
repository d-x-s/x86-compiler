#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt")

; M6 Tests

(test-case "undead 1 - tail is begin"
   (check-equal?
        (undead-analysis
            `(module ((new-frames (())) (locals (x.1)))
                (begin
                    (set! x.1 42)
                    (jump x.1))))
        
        `(module
            ((new-frames (())) (locals (x.1)) (call-undead ()) (undead-out ((x.1) ())))
            (begin (set! x.1 42) (jump x.1)))))

(test-case "undead 2 - long begin"
   (check-equal?
        (undead-analysis
            `(module ((new-frames (())) (locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1)))
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
            ((new-frames (()))
            (locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1))
            (call-undead ())
            (undead-out
                ((v.1)
                (v.1 w.2)
                (x.3 w.2)
                (p.1 x.3 w.2)
                (x.3 w.2)
                (x.3 w.2 y.4)
                (p.1 x.3 w.2 y.4)
                (x.3 w.2 y.4)
                (w.2 y.4 z.5)
                (y.4 z.5)
                (t.6 z.5)
                (p.1 t.6 z.5)
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
                (jump z.5)))))

(test-case "undead 3 - unused variable"
   (check-equal?
        (undead-analysis
            `(module ((new-frames (())) (locals (x.1 y.1)))
                (begin
                    (set! y.1 42)
                    (set! x.1 5)
                    (jump x.1))))
        
        `(module
            ((new-frames (()))
            (locals (x.1 y.1))
            (call-undead ())
            (undead-out (() (x.1) ())))
            (begin (set! y.1 42) (set! x.1 5) (jump x.1)))))

(test-case "undead 4 - unused variable"
   (check-equal?
        (undead-analysis
            `(module ((new-frames (())) (locals (x.1 y.1)))
                (begin
                    (set! x.1 5)
                    (set! y.1 42)
                    (jump x.1))))
        
        `(module
            ((new-frames (()))
            (locals (x.1 y.1))
            (call-undead ())
            (undead-out ((x.1) (x.1) ())))
            (begin (set! x.1 5) (set! y.1 42) (jump x.1)))))

(test-case "undead 5 - simple tail-nested begin"
   (check-equal?
        (undead-analysis
            `(module ((new-frames (())) (locals (x.1 w.2 p.1 t.6)))
                (begin
                    (set! x.1 42)
                    (set! w.2 46)
                    (begin (set! p.1 -1)
                        (set! t.6 (* t.6 w.2))
                        (jump t.6)))))
        
        `(module
            ((new-frames (()))
            (locals (x.1 w.2 p.1 t.6))
            (call-undead ())
            (undead-out ((t.6) (w.2 t.6) ((w.2 t.6) (t.6) ()))))
            (begin
                (set! x.1 42)
                (set! w.2 46)
                (begin (set! p.1 -1) (set! t.6 (* t.6 w.2)) (jump t.6))))))

(test-case "undead 6 - effect is begin"
   (check-equal?
        (undead-analysis
            `(module ((new-frames (())) (locals (x.1 y.3 w.2 p.1 t.6)))
                (begin
                    (set! x.1 42)
                    (set! w.2 46)
                    (begin (set! p.1 -1)
                        (set! t.6 (* t.6 w.2))
                        (begin (set! x.1 42)
                                (set! w.2 46)
                                (set! y.3 (+ y.3 2)))
                        (jump t.6)))))

        `(module
            ((new-frames (()))
            (locals (x.1 y.3 w.2 p.1 t.6))
            (call-undead ())
            (undead-out
                ((y.3 t.6)
                (w.2 y.3 t.6)
                ((w.2 y.3 t.6) (y.3 t.6) ((y.3 t.6) (y.3 t.6) (t.6)) ()))))
            (begin
                (set! x.1 42)
                (set! w.2 46)
                (begin
                (set! p.1 -1)
                (set! t.6 (* t.6 w.2))
                (begin (set! x.1 42) (set! w.2 46) (set! y.3 (+ y.3 2)))
                (jump t.6))))))

(test-case "undead 7 - tail-nested begin"
   (check-equal?
        (undead-analysis
            `(module ((new-frames (())) (locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1)))
                (begin
                    (set! v.1 1)
                    (set! w.2 46)
                    (set! x.3 v.1)
                    (set! p.1 7)
                    (set! z.5 (+ z.5 p.1))
                    (begin
                        (set! v.1 1)
                        (set! w.2 46)
                        (set! x.3 v.1)
                        (set! p.1 7)
                        (set! z.5 (+ z.5 p.1))
                        (jump z.5)))))
        
        `(module
            ((new-frames (()))
            (locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1))
            (call-undead ())
            (undead-out
                ((v.1 z.5)
                (v.1 z.5)
                (z.5)
                (p.1 z.5)
                (z.5)
                ((v.1 z.5) (v.1 z.5) (z.5) (p.1 z.5) (z.5) ()))))
            (begin
                (set! v.1 1)
                (set! w.2 46)
                (set! x.3 v.1)
                (set! p.1 7)
                (set! z.5 (+ z.5 p.1))
                (begin
                (set! v.1 1)
                (set! w.2 46)
                (set! x.3 v.1)
                (set! p.1 7)
                (set! z.5 (+ z.5 p.1))
                (jump z.5))))))

(test-case "undead 8 - multiple effects are begins"
   (check-equal?
        (undead-analysis
            `(module ((new-frames (())) (locals (x.1 y.3 w.2 p.1 t.6)))
                (begin
                    (set! x.1 42)
                    (set! w.2 46)
                    (begin (begin (set! x.1 42)
                                  (set! w.2 46)
                                  (set! y.3 (+ y.3 2)))
                           (set! t.6 (* t.6 w.2))
                           (begin (set! x.1 42)
                                  (set! w.2 46)
                                  (set! y.3 (+ y.3 2)))
                           (jump t.6)))))

        `(module
            ((new-frames (()))
            (locals (x.1 y.3 w.2 p.1 t.6))
            (call-undead ())
            (undead-out
                ((y.3 t.6)
                (y.3 t.6)
                (((y.3 t.6) (w.2 y.3 t.6) (w.2 y.3 t.6))
                (y.3 t.6)
                ((y.3 t.6) (y.3 t.6) (t.6))
                ()))))
            (begin
                (set! x.1 42)
                (set! w.2 46)
                (begin
                (begin (set! x.1 42) (set! w.2 46) (set! y.3 (+ y.3 2)))
                (set! t.6 (* t.6 w.2))
                (begin (set! x.1 42) (set! w.2 46) (set! y.3 (+ y.3 2)))
                (jump t.6))))))

(test-case "undead 9 - tail is if, pred is not"
   (check-equal?
        (undead-analysis
            `(module ((new-frames (())) (locals (x.1 y.2 b.3 c.4))) 
                (if (not (true)) (jump x.1) (jump c.4))))
        
        `(module
            ((new-frames (()))
            (locals (x.1 y.2 b.3 c.4))
            (call-undead ())
            (undead-out ((c.4 x.1) () ())))
            (if (not (true)) (jump x.1) (jump c.4)))))

(test-case "undead 10 - pred is begin, relop"
   (check-equal?
        (undead-analysis
            `(module ((new-frames (())) (locals (x.1 y.2 b.3 c.4))) 
                (if (begin (set! x.1 2)
                            (set! y.2 3)
                            (> y.2 x.1)) 
                    (jump x.1) (jump c.4))))
        
        `(module
            ((new-frames (()))
            (locals (x.1 y.2 b.3 c.4))
            (call-undead ())
            (undead-out (((c.4 x.1) (y.2 c.4 x.1) (c.4 x.1)) () ())))
            (if (begin (set! x.1 2) (set! y.2 3) (> y.2 x.1)) (jump x.1) (jump c.4)))))

(test-case "undead 11 - pred is equality"
   (check-equal?
        (undead-analysis
            `(module ((new-frames (())) (locals (x.1 y.2 b.3 c.4))) 
                (begin (set! x.1 5) 
                        (set! y.2 x.1) 
                        (begin (set! b.3 x.1) 
                                (set! b.3 (+ b.3 y.2)) 
                                (set! c.4 b.3) 
                                (if (= c.4 b.3) (jump c.4) (begin (set! x.1 c.4) 
                                                                (jump c.4)))))))
        
        `(module
            ((new-frames (()))
            (locals (x.1 y.2 b.3 c.4))
            (call-undead ())
            (undead-out
                ((x.1) (x.1 y.2) ((y.2 b.3) (b.3) (b.3 c.4) ((c.4) () ((c.4) ()))))))
            (begin
                (set! x.1 5)
                (set! y.2 x.1)
                (begin
                (set! b.3 x.1)
                (set! b.3 (+ b.3 y.2))
                (set! c.4 b.3)
                (if (= c.4 b.3) (jump c.4) (begin (set! x.1 c.4) (jump c.4))))))))
    
(test-case "undead 12 - pred is if"
   (check-equal?
        (undead-analysis
            `(module ((new-frames (())) (locals (x.1 y.2 b.3 c.4 x.2 x.3))) 
                (if (if (< x.2 x.3) (= b.3 c.4) (> x.1 y.2)) (jump x.1) (jump c.4))))
        
        `(module
            ((new-frames (()))
            (locals (x.1 y.2 b.3 c.4 x.2 x.3))
            (call-undead ())
            (undead-out (((y.2 b.3 c.4 x.1) (c.4 x.1) (c.4 x.1)) () ())))
            (if (if (< x.2 x.3) (= b.3 c.4) (> x.1 y.2)) (jump x.1) (jump c.4)))))

(test-case "undead 13 - effect is if"
   (check-equal?
        (undead-analysis
            `(module ((new-frames (())) (locals (x.1)))
                (begin
                    (set! x.4 x.5)
                    (if (= c.4 b.3) (set! x.2 x.3) (set! z.5 z.3))
                    (jump x.1))))
        
        `(module
            ((new-frames (()))
            (locals (x.1))
            (call-undead ())
            (undead-out ((b.3 c.4 z.3 x.3 x.1) ((z.3 x.3 x.1) (x.1) (x.1)) ())))
            (begin
                (set! x.4 x.5)
                (if (= c.4 b.3) (set! x.2 x.3) (set! z.5 z.3))
                (jump x.1)))))

(test-case "undead 14 - jump on register"
   (check-equal?
        (undead-analysis
            `(module ((new-frames (())) (locals ())) (jump rsp)))
        
        `(module
            ((new-frames (())) (locals ()) (call-undead ()) (undead-out ()))
            (jump rsp))))

(test-case "undead 15 - jump to label, register as param"
   (check-equal?
        (undead-analysis
            `(module ((new-frames (())) (locals (x.1)))
                (begin
                    (set! x.4 x.5)
                    (if (= c.4 b.3) (set! x.2 x.3) (set! z.5 z.3))
                    (jump L.start.1 rsp))))
        
        `(module
            ((new-frames (()))
            (locals (x.1))
            (call-undead ())
            (undead-out ((b.3 c.4 z.3 x.3 rsp) ((z.3 x.3 rsp) (rsp) (rsp)) (rsp))))
            (begin
                (set! x.4 x.5)
                (if (= c.4 b.3) (set! x.2 x.3) (set! z.5 z.3))
                (jump L.start.1 rsp)))))

(test-case "undead 16 - jump to aloc, multiple params"
   (check-equal?
        (undead-analysis
            `(module ((new-frames (())) (locals (x.1 y.2 x.4 x.5)))
                (begin
                    (set! x.4 x.5)
                    (jump x.1 rsp fv1 y.1))))

        `(module
            ((new-frames (()))
            (locals (x.1 y.2 x.4 x.5))
            (call-undead ())
            (undead-out ((x.1 rsp fv1 y.1) (rsp fv1 y.1))))
            (begin (set! x.4 x.5) (jump x.1 rsp fv1 y.1)))))

(test-case "undead 17 - define functions"
   (check-equal?
        (undead-analysis
            `(module ((new-frames (())) (locals (x.1 y.2 x.4 x.5)))
                     (define L.start.1 ((new-frames (())) (locals (x.1 x.2)))
                                       (begin (set! x.1 5) 
                                              (set! x.2 10) 
                                              (set! x.1 (+ x.1 x.2)) 
                                              (jump x.2)))
                     (define L.start.2 ((new-frames (())) (locals (x.1 y.2 b.3 c.4)))
                                       (if (begin (set! x.1 2)
                                                  (set! y.2 3)
                                                  (> y.2 x.1)) 
                                           (jump x.1) 
                                           (jump c.4)))
                     (begin
                        (set! x.4 x.5)
                        (jump x.1 rsp fv1 y.1))))

        `(module
            ((new-frames (()))
            (locals (x.1 y.2 x.4 x.5))
            (call-undead ())
            (undead-out ((x.1 rsp fv1 y.1) (rsp fv1 y.1))))
            (define L.start.1
                ((new-frames (()))
                (locals (x.1 x.2))
                (undead-out ((x.1) (x.1 x.2) (x.2) ()))
                (call-undead ()))
                (begin (set! x.1 5) (set! x.2 10) (set! x.1 (+ x.1 x.2)) (jump x.2)))
            (define L.start.2
                ((new-frames (()))
                (locals (x.1 y.2 b.3 c.4))
                (undead-out (((c.4 x.1) (y.2 c.4 x.1) (c.4 x.1)) () ()))
                (call-undead ()))
                (if (begin (set! x.1 2) (set! y.2 3) (> y.2 x.1)) (jump x.1) (jump c.4)))
            (begin (set! x.4 x.5) (jump x.1 rsp fv1 y.1)))))

(test-case "undead 18 - set loc to label"
   (check-equal?
        (undead-analysis
            `(module ((new-frames (())) (locals (y.12))) 
                     (define L.id1.2 ((new-frames (())) (locals (x.10))) (begin (set! x.10 rdi) (jump x.10))) 
                     (define L.id2.3 ((new-frames (())) (locals (x.11))) (begin (set! x.11 rdi) (jump x.11))) 
                     (begin 
                        (if (true) (set! y.12 L.id1.2) (set! y.12 L.id2.3)) 
                        (set! rdi 5) 
                        (jump y.12 rbp rdi))))

        `(module
            ((new-frames (()))
            (locals (y.12))
            (call-undead ())
            (undead-out (((rbp) (y.12 rbp) (y.12 rbp)) (y.12 rbp rdi) (rbp rdi))))
            (define L.id1.2
                ((new-frames (()))
                (locals (x.10))
                (undead-out ((x.10) ()))
                (call-undead ()))
                (begin (set! x.10 rdi) (jump x.10)))
            (define L.id2.3
                ((new-frames (()))
                (locals (x.11))
                (undead-out ((x.11) ()))
                (call-undead ()))
                (begin (set! x.11 rdi) (jump x.11)))
            (begin
                (if (true) (set! y.12 L.id1.2) (set! y.12 L.id2.3))
                (set! rdi 5)
                (jump y.12 rbp rdi)))))

(test-case "undead 19 - binop is subtraction"
   (check-equal?
        (undead-analysis
            `(module ((new-frames (())) (locals (x.1 x.3 p.1)))
                (begin
                    (set! x.1 42)
                    (set! x.3 (- x.3 p.1))
                    (jump x.1))))

        `(module
            ((new-frames (()))
            (locals (x.1 x.3 p.1))
            (call-undead ())
            (undead-out ((x.3 p.1 x.1) (x.1) ())))
            (begin (set! x.1 42) (set! x.3 (- x.3 p.1)) (jump x.1)))))

(test-case "undead 20 - simple return points"
   (check-equal?
        (undead-analysis
            `(module ((new-frames (())) (locals (x.1 x.3 p.1 y.1 y.2 y.3 y.4 y.5 y.6)))
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
                ())))
            (begin
                (set! x.1 42)
                (set! x.3 (- x.3 p.1))
                (return-point L.one.1 (jump y.1))
                (return-point L.two.2 (begin (set! y.2 y.3) (jump y.4)))
                (return-point L.three.3 (if (false) (jump y.5) (jump y.6)))
                (jump x.1)))))

(test-case "undead 21 - nested tail in return point"
   (check-equal?
        (undead-analysis
            `(module ((new-frames (())) (locals (x.1 x.3 p.1 y.1 y.2 y.3 z.5 y.5 y.6)))
                (begin
                    (set! x.1 42)
                    (set! x.3 (- x.3 p.1))
                    (return-point L.two.2 (begin 
                                            (set! y.1 2)
                                            (set! y.2 y.3)
                                            (begin 
                                                (set! p.1 7)
                                                (set! z.5 (+ z.5 p.1))) 
                                            (if (false) 
                                                (jump y.5) 
                                                (jump y.6))))
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
                (jump x.1)))))

(test-case "undead 22 - return point in function"
   (check-equal?
        (undead-analysis
            `(module ((new-frames (())) (locals (x.1 y.2 x.4 x.5 z.3)))
                     (define L.start.1 ((new-frames (())) (locals (x.1 x.2 z.1)))
                                       (begin (set! x.1 5) 
                                              (set! x.2 10) 
                                              (set! x.1 (+ x.1 x.2))
                                              (return-point L.one.1 (jump z.1)) 
                                              (jump x.2)))
                     (define L.start.2 ((new-frames (())) (locals (x.1 y.2 b.3 c.4 z.2)))
                                       (if (begin (set! x.1 2)
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
                (jump x.1 rsp fv1 y.1)))))