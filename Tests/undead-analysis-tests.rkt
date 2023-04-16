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

(test-case "undead 22 - return point in function, registers in return-point undead-out"
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

(test-case "undead 23 - complex test, rax in undead-out"
   (check-equal?
        (undead-analysis
            `(module
                ((new-frames ()) (locals (ra.12)))
                (define L.fact.4
                    ((new-frames ((nfv.16)))
                    (locals (ra.13 x.9 tmp.14 tmp.15 new-n.10 nfv.16 factn-1.11 tmp.17)))
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
                (jump L.fact.4 rbp r15 fv0)))))

; M6 Tests

(test-case "undead 24 - extend binops"
   (check-equal?
        (undead-analysis
            `(module ((new-frames (())) (locals (x.1 x.3 p.1)))
                (begin
                    (set! x.1 42)
                    (set! x.3 (bitwise-ior x.3 p.1))
                    (jump x.1))))

        `(module
            ((new-frames (()))
            (locals (x.1 x.3 p.1))
            (call-undead ())
            (undead-out ((x.3 p.1 x.1) (x.1) ())))
            (begin (set! x.1 42) (set! x.3 (bitwise-ior x.3 p.1)) (jump x.1)))))

; M8 Tests

(test-case "undead 25 - effect is mref"
   (check-equal?
        (undead-analysis
            `(module
                ((new-frames (())) (locals (y.1 x.1 x.2 x.3)))
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
                ())))
            (begin
                (set! x.1 (mref fv1 5))
                (set! r14 (mref fv2 rbx))
                (set! fv3 (mref x.2 fv4))
                (set! x.3 (mref x.2 y.1))
                (set! x.1 (mref x.3 y.1))
                (jump rax)))))

(test-case "undead 26 - effect is mset"
   (check-equal?
        (undead-analysis
            `(module
                ((new-frames (())) (locals (y.1 x.1 x.3 y.3 y.2)))
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
                ())))
            (begin
                (set! r14 L.s.1)
                (mset! y.2 fv4 L.s.1)
                (mset! fv1 r14 6)
                (mset! y.1 x.1 r15)
                (mset! rsp fv2 fv3)
                (mset! y.3 5 x.3)
                (jump rax)))))

(test-case "undead 0 - stack smash"
   (check-equal?
(undead-analysis
'(module
  ((new-frames ()) (locals (tmp-ra.7)))
  (define L.+.5
    ((new-frames ()) (locals (tmp.3 tmp.16 tmp.4 tmp-ra.1 tmp.2 tmp.17 tmp.1)))
    (begin
      (set! tmp-ra.1 r15)
      (set! tmp.16 rdi)
      (set! tmp.17 rsi)
      (if (begin
            (if (begin
                  (begin
                    (set! tmp.2 tmp.17)
                    (set! tmp.2 (bitwise-and tmp.2 7)))
                  (= tmp.2 0))
              (set! tmp.1 14)
              (set! tmp.1 6))
            (!= tmp.1 6))
        (if (begin
              (if (begin
                    (begin
                      (set! tmp.4 tmp.16)
                      (set! tmp.4 (bitwise-and tmp.4 7)))
                    (= tmp.4 0))
                (set! tmp.3 14)
                (set! tmp.3 6))
              (!= tmp.3 6))
          (begin
            (set! rax tmp.16)
            (set! rax (+ rax tmp.17))
            (jump tmp-ra.1 rbp rax))
          (begin (set! rax 574) (jump tmp-ra.1 rbp rax)))
        (begin (set! rax 574) (jump tmp-ra.1 rbp rax)))))
  (define L.F.4
    ((new-frames ((nfv.3 nfv.4)))
     (locals (tmp.5 nfv.4 nfv.3 g.1 f.2 e.3 d.4 c.5 b.6 a.7 tmp-ra.2)))
    (begin
      (set! tmp-ra.2 r15)
      (set! a.7 rdi)
      (set! b.6 rsi)
      (set! c.5 rdx)
      (set! d.4 rcx)
      (set! e.3 r8)
      (set! f.2 r9)
      (set! g.1 fv0)
      (return-point
       L.rp.4
       (begin
         (set! nfv.4 64)
         (set! nfv.3 g.1)
         (set! r9 f.2)
         (set! r8 e.3)
         (set! rcx d.4)
         (set! rdx c.5)
         (set! rsi b.6)
         (set! rdi a.7)
         (set! r15 L.rp.4)
         (jump L.G.5 rbp r15 rdi rsi rdx rcx r8 r9 nfv.3 nfv.4)))
      (set! tmp.5 rax)
      (set! rsi tmp.5)
      (set! rdi 80)
      (set! r15 tmp-ra.2)
      (jump L.+.5 rbp r15 rdi rsi)))
  (define L.G.5
    ((new-frames ()) (locals (h.8 g.9 f.10 e.11 d.12 c.13 b.14 a.15 tmp-ra.5)))
    (begin
      (set! tmp-ra.5 r15)
      (set! a.15 rdi)
      (set! b.14 rsi)
      (set! c.13 rdx)
      (set! d.12 rcx)
      (set! e.11 r8)
      (set! f.10 r9)
      (set! g.9 fv0)
      (set! h.8 fv1)
      (set! fv2 72)
      (set! fv1 h.8)
      (set! fv0 g.9)
      (set! r9 f.10)
      (set! r8 e.11)
      (set! rcx d.12)
      (set! rdx c.13)
      (set! rsi b.14)
      (set! rdi a.15)
      (set! r15 tmp-ra.5)
      (jump L.H.6 rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2)))
  (define L.H.6
    ((new-frames (() () () () () () ()))
     (locals
      (r7.31
       r6.30
       r5.29
       r4.28
       r3.27
       r2.26
       r1.25
       j.16
       h.17
       g.18
       f.19
       e.20
       d.21
       c.22
       b.23
       a.24
       tmp-ra.6)))
    (begin
      (set! tmp-ra.6 r15)
      (set! a.24 rdi)
      (set! b.23 rsi)
      (set! c.22 rdx)
      (set! d.21 rcx)
      (set! e.20 r8)
      (set! f.19 r9)
      (set! g.18 fv0)
      (set! h.17 fv1)
      (set! j.16 fv2)
      (return-point
       L.rp.5
       (begin
         (set! rsi b.23)
         (set! rdi a.24)
         (set! r15 L.rp.5)
         (jump L.+.5 rbp r15 rdi rsi)))
      (set! r1.25 rax)
      (return-point
       L.rp.6
       (begin
         (set! rsi c.22)
         (set! rdi r1.25)
         (set! r15 L.rp.6)
         (jump L.+.5 rbp r15 rdi rsi)))
      (set! r2.26 rax)
      (return-point
       L.rp.7
       (begin
         (set! rsi d.21)
         (set! rdi r2.26)
         (set! r15 L.rp.7)
         (jump L.+.5 rbp r15 rdi rsi)))
      (set! r3.27 rax)
      (return-point
       L.rp.8
       (begin
         (set! rsi e.20)
         (set! rdi r3.27)
         (set! r15 L.rp.8)
         (jump L.+.5 rbp r15 rdi rsi)))
      (set! r4.28 rax)
      (return-point
       L.rp.9
       (begin
         (set! rsi f.19)
         (set! rdi r4.28)
         (set! r15 L.rp.9)
         (jump L.+.5 rbp r15 rdi rsi)))
      (set! r5.29 rax)
      (return-point
       L.rp.10
       (begin
         (set! rsi g.18)
         (set! rdi r5.29)
         (set! r15 L.rp.10)
         (jump L.+.5 rbp r15 rdi rsi)))
      (set! r6.30 rax)
      (return-point
       L.rp.11
       (begin
         (set! rsi h.17)
         (set! rdi r6.30)
         (set! r15 L.rp.11)
         (jump L.+.5 rbp r15 rdi rsi)))
      (set! r7.31 rax)
      (set! rsi j.16)
      (set! rdi r7.31)
      (set! r15 tmp-ra.6)
      (jump L.+.5 rbp r15 rdi rsi)))
  (begin
    (set! tmp-ra.7 r15)
    (set! fv0 56)
    (set! r9 48)
    (set! r8 40)
    (set! rcx 32)
    (set! rdx 24)
    (set! rsi 16)
    (set! rdi 8)
    (set! r15 tmp-ra.7)
    (jump L.F.4 rbp r15 rdi rsi rdx rcx r8 r9 fv0)))
)
        
        `(module
            ((new-frames (())) (locals (x.1)) (call-undead ()) (undead-out ((x.1) ())))
            (begin (set! x.1 42) (jump x.1)))))
