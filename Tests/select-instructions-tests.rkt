#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt")

; M6 Tests
(test-case "select 1 - tail is jump"
   (check-equal?
        (select-instructions
            `(module ((new-frames ())) 
                     (jump fv1)))
        
        `(module ((new-frames ())) (jump fv1))))

(test-case "select 2 - nested begins, tail is jump"
   (check-equal?
        (select-instructions
            `(module ((new-frames ())) 
                     (begin 
                        (set! x.1 2)
                        (set! x.2 x.3)
                        (begin 
                            (set! z.1 2)
                            (set! z.1 z.4))
                        (set! x.3 (+ y.1 y.2))
                        (jump L.start.1 r9))))
        
        `(module
            ((new-frames ()))
            (begin
                (set! x.1 2)
                (set! x.2 x.3)
                (set! z.1 2)
                (set! z.1 z.4)
                (set! x.3 y.1)
                (set! x.3 (+ x.3 y.2))
                (jump L.start.1 r9)))))

(test-case "select 3 - nested begins, tail is begin"
   (check-equal?
        (select-instructions
            `(module ((new-frames ())) 
                     (begin 
                        (set! x.1 2)
                        (set! x.2 x.3)
                        (begin 
                            (set! z.1 2)
                            (set! z.1 z.4))
                        (set! x.3 (+ y.1 y.2))
                        (begin 
                            (set! z.1 2)
                            (jump r9 r9 fv1 x.1)))))
        
        `(module
            ((new-frames ()))
            (begin
                (set! x.1 2)
                (set! x.2 x.3)
                (set! z.1 2)
                (set! z.1 z.4)
                (set! x.3 y.1)
                (set! x.3 (+ x.3 y.2))
                (set! z.1 2)
                (jump r9 r9 fv1 x.1)))))

(test-case "select 4 - tail is if"
   (check-equal?
        (select-instructions
            `(module ((new-frames ())) 
                     (if (true)
                         (jump fv9 fv1)
                         (jump x.7 x.1))))
        
        `(module ((new-frames ())) (if (true) (jump fv9 fv1) (jump x.7 x.1)))))

(test-case "select 5 - pred is number comparison"
   (check-equal?
        (select-instructions
            `(module ((new-frames ())) 
                     (if (> 0 1) 
                         (jump fv9 fv1)
                         (jump x.7 x.1))))
        
        `(module
            ((new-frames ()))
            (if (begin (set! tmp.2 0) (> tmp.2 1)) 
                (jump fv9 fv1) 
                (jump x.7 x.1)))))

(test-case "select 6 - pred is not"
   (check-equal?
        (select-instructions
            `(module ((new-frames ()))
                     (if (not (> 0 1)) 
                         (jump fv9)
                         (jump x.7))))
        
        `(module
            ((new-frames ()))
            (if (not (begin (set! tmp.4 0) (> tmp.4 1))) 
                (jump fv9) 
                (jump x.7)))))

; Note: this test differs from interrogator result in that it flattens begins in (begin effects ... pred).
(test-case "select 7 - pred is begin"
   (check-equal?
        (select-instructions
            `(module ((new-frames ())) 
                     (if (begin 
                            (set! x.1 2) 
                            (set! x.2 (+ 1 2)) 
                            (begin (set! y.2 2)) 
                            (true)) 
                         (jump fv9)
                         (jump x.7))))
        
        `(module
            ((new-frames ()))
            (if (begin
                    (set! x.1 2)
                    (set! x.2 1) 
                    (set! x.2 (+ x.2 2))
                    (set! y.2 2)
                    (true))
                (jump fv9)
                (jump x.7)))))

(test-case "select 8 - pred is if"
   (check-equal?
        (select-instructions
            `(module ((new-frames ())) 
                     (if (if (> 1 2) (true) (false)) 
                         (jump fv9)
                         (jump x.7))))
        
        `(module
            ((new-frames ()))
            (if (if (begin (set! tmp.6 1) (> tmp.6 2)) (true) (false))
                (jump fv9)
                (jump x.7)))))

(test-case "select 9 - effect is if"
   (check-equal?
        (select-instructions
            `(module ((new-frames ())) 
                     (begin 
                        (set! x.1 2)
                        (if (true) (set! x.2 (+ 1 2)) (set! x.3 3))
                        (jump fv9))))
        
        `(module
            ((new-frames ()))
            (begin
                (set! x.1 2)
                (if (true) (begin (set! x.2 1) (set! x.2 (+ x.2 2))) (set! x.3 3))
                (jump fv9)))))

(test-case "select 10 - effect if has begins as branches"
   (check-equal?
        (select-instructions
            `(module 
                ((new-frames ()))
                (begin 
                    (set! x.1 5) 
                    (if (true) 
                        (begin (set! y.2 (+ x.1 17)) (set! x.5 12)) 
                        (begin (set! x.5 15))) 
                    (jump x.5))))
        
        `(module
            ((new-frames ()))
            (begin
                (set! x.1 5)
                (if (true)
                (begin (set! y.2 x.1) (set! y.2 (+ y.2 17)) (set! x.5 12))
                (begin (set! x.5 15)))
                (jump x.5)))))

(test-case "select 11 - define functions"
   (check-equal?
        (select-instructions
            `(module 
                ((new-frames ()))
                (define L.id.5 ((new-frames ())) (begin (set! x.32 rdi) (jump x.32))) 
                (begin (set! y.33 L.id.5) (begin (set! rdi 5) (jump y.33 rbp rdi)))))
        
        `(module
            ((new-frames ()))
            (define L.id.5 ((new-frames ())) (begin (set! x.32 rdi) (jump x.32)))
            (begin (set! y.33 L.id.5) (set! rdi 5) (jump y.33 rbp rdi)))))

(test-case "select 12 - complex test"
   (check-equal?
        (select-instructions
            `(module 
                ((new-frames ()))
                (define L.odd?.6 ((new-frames ())) 
                                 (begin 
                                    (set! x.58 rdi) 
                                    (if (= x.58 0) 
                                        (jump x.0) 
                                        (begin 
                                            (set! y.59 (+ x.58 -1)) 
                                            (begin (set! rdi y.59) (jump L.even?.7 rbp rdi)))))) 
                (define L.even?.7 ((new-frames ())) 
                                  (begin 
                                    (set! x.60 rdi) 
                                    (if (= x.60 0) 
                                        (jump x.1)
                                        (begin (set! y.61 (+ x.60 -1)) (begin (set! rdi y.61) (jump L.odd?.6 rbp rdi)))))) 
                (begin (set! rdi 5) (jump L.even?.7 rbp rdi))))
        
        `(module
            ((new-frames ()))
            (define L.odd?.6
                ((new-frames ()))
                (begin
                (set! x.58 rdi)
                (if (= x.58 0)
                    (jump x.0)
                    (begin
                    (set! y.59 x.58)
                    (set! y.59 (+ y.59 -1))
                    (set! rdi y.59)
                    (jump L.even?.7 rbp rdi)))))
            (define L.even?.7
                ((new-frames ()))
                (begin
                (set! x.60 rdi)
                (if (= x.60 0)
                    (jump x.1)
                    (begin
                    (set! y.61 x.60)
                    (set! y.61 (+ y.61 -1))
                    (set! rdi y.61)
                    (jump L.odd?.6 rbp rdi)))))
            (begin (set! rdi 5) (jump L.even?.7 rbp rdi)))))

(test-case "select 13 - effect is return-point, return-point tail is jump, begin, if"
   (check-equal?
        (select-instructions
            `(module ((new-frames ())) 
                     (begin 
                        (set! x.1 2)
                        (return-point L.one.1 (jump fv9))
                        (return-point L.two.2 (begin (set! z.1 2) (jump r9 r9 fv1 x.1)))
                        (return-point L.three.3 (if (true) (jump fv9 fv1) (jump x.7 x.1)))
                        (jump fv9))))
        
        `(module
            ((new-frames ()))
            (begin
                (set! x.1 2)
                (return-point L.one.1 (jump fv9))
                (return-point L.two.2 (begin (set! z.1 2) (jump r9 r9 fv1 x.1)))
                (return-point L.three.3 (if (true) (jump fv9 fv1) (jump x.7 x.1)))
                (jump fv9)))))