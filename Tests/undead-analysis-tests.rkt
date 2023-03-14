#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt"
)

(test-case "undead 1"
   (check-equal?
        (undead-analysis
            `(module ((locals (v.1)))
                (halt v.1)))
        
        `(module ((locals (v.1)) (undead-out ())) (halt v.1))))

(test-case "undead 2"
   (check-equal?
        (undead-analysis
            `(module ((locals (x.1)))
                (begin
                    (set! x.1 42)
                    (halt x.1))))
        
        `(module ((locals (x.1)) (undead-out ((x.1) ()))) (begin (set! x.1 42) (halt x.1)))))

(test-case "undead 3"
   (check-equal?
        (undead-analysis
            `(module ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1)))
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
                (halt z.5)))))

(test-case "undead 4"
   (check-equal?
        (undead-analysis
            `(module ((locals (x.1 y.1)))
                (begin
                    (set! y.1 42)
                    (set! x.1 5)
                    (halt x.1))))
        
        `(module
            ((locals (x.1 y.1)) (undead-out (() (x.1) ())))
            (begin (set! y.1 42) (set! x.1 5) (halt x.1)))))

(test-case "undead 5"
   (check-equal?
        (undead-analysis
            `(module ((locals (x.1 y.1)))
                (begin
                    (set! x.1 5)
                    (set! y.1 42)
                    (halt x.1))))
        
        `(module
            ((locals (x.1 y.1)) (undead-out ((x.1) (x.1) ())))
            (begin (set! x.1 5) (set! y.1 42) (halt x.1)))))

(test-case "undead 6"
   (check-equal?
        (undead-analysis
            `(module ((locals (x.1 w.2 p.1 t.6)))
                (begin
                    (set! x.1 42)
                    (set! w.2 46)
                    (begin (set! p.1 -1)
                        (set! t.6 (* t.6 w.2))
                        (halt t.6)))))
        
        `(module
            ((locals (x.1 w.2 p.1 t.6))
            (undead-out 
                ((t.6) (w.2 t.6) 
                    ((w.2 t.6) (t.6) ()))))
            (begin
                (set! x.1 42)
                (set! w.2 46)
                (begin (set! p.1 -1) (set! t.6 (* t.6 w.2)) (halt t.6))))))

(test-case "undead 7"
   (check-equal?
        (undead-analysis
            `(module ((locals (x.1 y.3 w.2 p.1 t.6)))
                (begin
                    (set! x.1 42)
                    (set! w.2 46)
                    (begin (set! p.1 -1)
                        (set! t.6 (* t.6 w.2))
                        (begin (set! x.1 42)
                                (set! w.2 46)
                                (set! y.3 (+ y.3 2)))
                        (halt t.6)))))
        
        `(module
            ((locals (x.1 y.3 w.2 p.1 t.6))
            (undead-out
                ((y.3 t.6)
                (w.2 y.3 t.6)
                ((w.2 y.3 t.6) 
                (y.3 t.6) 
                ((y.3 t.6) 
                (y.3 t.6) 
                (t.6)) 
                ()))))
            (begin
                (set! x.1 42)
                (set! w.2 46)
                (begin
                (set! p.1 -1)
                (set! t.6 (* t.6 w.2))
                (begin (set! x.1 42) (set! w.2 46) (set! y.3 (+ y.3 2)))
                (halt t.6))))))

(test-case "undead 8"
   (check-equal?
        (undead-analysis
            `(module ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1)))
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
                    (halt z.5)))))
        
        `(module
            ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1))
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
                (halt z.5))))))

(test-case "undead 9"
   (check-equal?
        (undead-analysis
            `(module ((locals (x.1 y.3 w.2 p.1 t.6)))
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
                        (halt t.6)))))
        
        `(module
            ((locals (x.1 y.3 w.2 p.1 t.6))
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
                (halt t.6))))))

(test-case "undead 10"
   (check-equal?
        (undead-analysis
            `(module
                ((locals (x.1 x.2)) )
                (begin (set! x.1 5) 
                    (set! x.2 10) 
                    (set! x.1 (+ x.1 x.2)) 
                    (halt x.2))))
        
        `(module
            ((locals (x.1 x.2)) (undead-out ((x.1) (x.1 x.2) (x.2) ())))
            (begin (set! x.1 5) (set! x.2 10) (set! x.1 (+ x.1 x.2)) (halt x.2)))))


; M4 Tests

(test-case "undead 11"
   (check-equal?
        (undead-analysis
            `(module ((locals (x.1 y.2 b.3 c.4))) 
                (if (not (true)) (halt x.1) (halt c.4))))
        
        `(module
            ((locals (x.1 y.2 b.3 c.4)) (undead-out ((c.4 x.1) () ())))
            (if (not (true)) (halt x.1) (halt c.4)))))

(test-case "undead 12"
   (check-equal?
        (undead-analysis
            `(module ((locals (x.1 y.2 b.3 c.4))) 
                (if (begin (set! x.1 2)
                            (set! y.2 3)
                            (> y.2 x.1)) 
                    (halt x.1) (halt c.4))))
        
        `(module
            ((locals (x.1 y.2 b.3 c.4))
            (undead-out (((c.4 x.1) (y.2 c.4 x.1) (c.4 x.1)) () ())))
            (if (begin (set! x.1 2) (set! y.2 3) (> y.2 x.1)) (halt x.1) (halt c.4)))))

(test-case "undead 13"
   (check-equal?
        (undead-analysis
            `(module ((locals (x.1 y.2 b.3 c.4))) 
                (begin (set! x.1 5) 
                        (set! y.2 x.1) 
                        (begin (set! b.3 x.1) 
                                (set! b.3 (+ b.3 y.2)) 
                                (set! c.4 b.3) 
                                (if (= c.4 b.3) (halt c.4) (begin (set! x.1 c.4) 
                                                                (halt c.4)))))))
        
        `(module
            ((locals (x.1 y.2 b.3 c.4))
            (undead-out
                ((x.1) (x.1 y.2) ((y.2 b.3) (b.3) (b.3 c.4) ((c.4) () ((c.4) ()))))))
            (begin
                (set! x.1 5)
                (set! y.2 x.1)
                (begin
                (set! b.3 x.1)
                (set! b.3 (+ b.3 y.2))
                (set! c.4 b.3)
                (if (= c.4 b.3) (halt c.4) (begin (set! x.1 c.4) (halt c.4))))))))
    
(test-case "undead 14"
   (check-equal?
        (undead-analysis
            `(module ((locals (x.1 y.2 b.3 c.4 x.2 x.3))) 
                (if (if (< x.2 x.3) (= b.3 c.4) (> x.1 y.2)) (halt x.1) (halt c.4))))
        
        `(module
            ((locals (x.1 y.2 b.3 c.4 x.2 x.3))
            (undead-out (((y.2 b.3 c.4 x.1) (c.4 x.1) (c.4 x.1)) () ())))
            (if (if (< x.2 x.3) (= b.3 c.4) (> x.1 y.2)) (halt x.1) (halt c.4)))))

(test-case "undead 15"
   (check-equal?
        (undead-analysis
            `(module ((locals (x.1)))
                (begin
                    (set! x.4 x.5)
                    (if (= c.4 b.3) (set! x.2 x.3) (set! z.5 z.3))
                    (halt x.1))))
        
        `(module
            ((locals (x.1))
            (undead-out ((b.3 c.4 z.3 x.3 x.1) ((z.3 x.3 x.1) (x.1) (x.1)) ())))
            (begin
                (set! x.4 x.5)
                (if (= c.4 b.3) (set! x.2 x.3) (set! z.5 z.3))
                (halt x.1)))))

; M5 Tests

(test-case "undead 16"
   (check-equal?
        (undead-analysis
            `(module ((locals ())) (halt rsp)))
        
        `(module ((locals ()) (undead-out ())) (halt rsp))))

(test-case "undead 17"
   (check-equal?
        (undead-analysis
            `(module ((locals (x.1)))
                (begin
                    (set! x.4 x.5)
                    (if (= c.4 b.3) (set! x.2 x.3) (set! z.5 z.3))
                    (jump L.start.1 rsp))))
        
        `(module
            ((locals (x.1))
            (undead-out ((b.3 c.4 z.3 x.3 rsp) ((z.3 x.3 rsp) (rsp) (rsp)) (rsp))))
            (begin
                (set! x.4 x.5)
                (if (= c.4 b.3) (set! x.2 x.3) (set! z.5 z.3))
                (jump L.start.1 rsp)))))

(test-case "undead 18"
   (check-equal?
        (undead-analysis
            `(module ((locals (x.1 y.2 x.4 x.5)))
                (begin
                    (set! x.4 x.5)
                    (jump x.1 rsp fv1 y.1))))
        
        `(module
            ((locals (x.1 y.2 x.4 x.5)) (undead-out ((x.1 rsp fv1 y.1) (rsp fv1 y.1))))
            (begin (set! x.4 x.5) (jump x.1 rsp fv1 y.1)))))

(test-case "undead 19"
   (check-equal?
        (undead-analysis
            `(module ((locals (x.1 y.2 x.4 x.5)))
                     (define L.start.1 ((locals (x.1 x.2)))
                                       (begin (set! x.1 5) 
                                              (set! x.2 10) 
                                              (set! x.1 (+ x.1 x.2)) 
                                              (halt x.2)))
                     (define L.start.2 ((locals (x.1 y.2 b.3 c.4)))
                                       (if (begin (set! x.1 2)
                                                  (set! y.2 3)
                                                  (> y.2 x.1)) 
                                           (halt x.1) 
                                           (halt c.4)))
                     (begin
                        (set! x.4 x.5)
                        (jump x.1 rsp fv1 y.1))))
        
        `(module
            ((locals (x.1 y.2 x.4 x.5)) (undead-out ((x.1 y.1 fv1 rsp) (y.1 fv1 rsp))))
            (define L.start.1
                ((locals (x.1 x.2)) (undead-out ((x.1) (x.1 x.2) (x.2) ())))
                (begin (set! x.1 5) (set! x.2 10) (set! x.1 (+ x.1 x.2)) (halt x.2)))
            (define L.start.2
                ((locals (x.1 y.2 b.3 c.4))
                (undead-out (((c.4 x.1) (y.2 c.4 x.1) (c.4 x.1)) () ())))
                (if (begin (set! x.1 2) (set! y.2 3) (> y.2 x.1)) (halt x.1) (halt c.4)))
            (begin (set! x.4 x.5) (jump x.1 rsp fv1 y.1)))))
