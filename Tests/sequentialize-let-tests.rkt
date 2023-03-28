#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt")

(test-case "sequentialize 1 - tail is number"
   (check-equal?
        (sequentialize-let
            `(module 6))
        
        `(module 6)))

(test-case "sequentialize 2 - tail is binop"
   (check-equal?
        (sequentialize-let
            `(module (* 2 3)))
        
        `(module (* 2 3))))

(test-case "sequentialize 3 - empty let"
   (check-equal?
        (sequentialize-let
            `(module (let () 2)))
        
        `(module (begin 2))))

(test-case "sequentialize 4 - simple let"
   (check-equal?
        (sequentialize-let
            `(module (let ([x.1 1] [x.2 2]) (+ x.1 x.2))))
        
        `(module (begin (set! x.1 1) (set! x.2 2) (+ x.1 x.2)))))

(test-case "sequentialize 5 - nested let"
   (check-equal?
        (sequentialize-let
            `(module (let ([x.1 1] [x.2 2]) 
                          (let ([x.1 0] [x.2 1]) 2))))
        
        `(module (begin (set! x.1 1) (set! x.2 2) (begin (set! x.1 0) (set! x.2 1) 2)))))

(test-case "sequentialize 6 - binop, aloc as let value"
   (check-equal?
        (sequentialize-let
            `(module (let ([x.1 (+ 0 2)] [x.2 x.1]) 
                          (let ([x.1 0] [x.2 1]) 2))))
        
        `(module (begin (set! x.1 (+ 0 2)) (set! x.2 x.1) (begin (set! x.1 0) (set! x.2 1) 2)))))

(test-case "sequentialize 7 - let as let value"
   (check-equal?
        (sequentialize-let
            `(module (let ([x.3 (let () 2)]) 
                          (let ([x.1 (let ([y.1 1] [y.2 2]) y.2)] 
                                [x.2 1]) 
                                2))))
        
        `(module (begin (set! x.3 (begin 2)) (begin (set! x.1 (begin (set! y.1 1) (set! y.2 2) y.2)) (set! x.2 1) 2)))))


; M4 Tests

(test-case "sequentialize 8 - tail is if"
   (check-equal?
        (sequentialize-let
            `(module (if (= 0 0) 0 1)))
        
        `(module (if (= 0 0) 0 1))))

(test-case "sequentialize 9 - simple pred"
   (check-equal?
        (sequentialize-let
            `(module (let ([y.1 200]) 
                          (if (< 3 y.1) 1 0))))
        
        `(module (begin (set! y.1 200) 
                        (if (< 3 y.1) 1 0)))))

(test-case "sequentialize 10 - not, if, and let as pred, nested if"
   (check-equal?
        (sequentialize-let
            `(module (let ([x.37 20] [y.38 21]) 
                          (if (not (> x.37 12)) 
                              (if (if (let ([z.39 x.37]) (< y.38 z.39)) 
                                      (true) 
                                      (false))
                                  10 
                                  12) 
                              (+ x.37 y.38)))))
        
        `(module
            (begin
                (set! x.37 20)
                (set! y.38 21)
                (if (not (> x.37 12))
                    (if (if (begin (set! z.39 x.37) (< y.38 z.39)) 
                            (true) 
                            (false)) 
                        10 
                        12)
                    (+ x.37 y.38))))))

(test-case "sequentialize 11 - multi-nested let"
   (check-equal?
        (sequentialize-let
            `(module (let ((x.48 1)) 
                          (let ((y.49 (let ((z.50 3)) z.50))) 
                               (let ((z.51 (let ((y.52 2)) (+ y.52 y.52)))) 
                                    (if (let ((x.53 6)) (> x.53 7)) 
                                        9 
                                        10))))))
        
        `(module
            (begin
                (set! x.48 1)
                (begin
                (set! y.49 (begin (set! z.50 3) z.50))
                (begin
                    (set! z.51 (begin (set! y.52 2) (+ y.52 y.52)))
                    (if (begin (set! x.53 6) (> x.53 7)) 9 10)))))))

(test-case "sequentialize 12 - let and if as if branches"
   (check-equal?
        (sequentialize-let
            `(module (if (true) 
                         (if (let ((y.54 11) (z.55 15)) (> y.54 z.55)) 
                             14 
                             15) 
                         (let ((z.56 12)) 
                              (let ((z.57 15) (y.58 1)) (+ z.57 y.58))))))
        
        `(module
            (if (true)
                (if (begin 
                        (set! y.54 11) 
                        (set! z.55 15) 
                        (> y.54 z.55)) 
                    14 
                    15)
                (begin 
                    (set! z.56 12) 
                    (begin (set! z.57 15) (set! y.58 1) (+ z.57 y.58)))))))

; M5 tests

(test-case "sequentialize 13 - tail is call"
   (check-equal?
        (sequentialize-let
            `(module (let ((x.5 10)) (call x.5 2 3 4))))
        
        `(module (begin (set! x.5 10) (call x.5 2 3 4)))))

(test-case "sequentialize 14 - define lambda functions"
   (check-equal?
        (sequentialize-let
            `(module 
                (define L.id1.2 (lambda (x.10) x.10)) 
                (define L.id2.3 (lambda (x.11) x.11)) 
                (let ((y.12 (if (true) L.id1.2 L.id2.3))) (call y.12 5))))
        
        `(module
            (define L.id1.2 (lambda (x.10) x.10))
            (define L.id2.3 (lambda (x.11) x.11))
            (begin (set! y.12 (if (true) L.id1.2 L.id2.3)) (call y.12 5)))))

(test-case "sequentialize 15 - complex test"
   (check-equal?
        (sequentialize-let
            `(module 
                (define L.id1.2 (lambda (x.10) x.10)) 
                (define L.id2.3 (lambda (x.11) (let ((x.48 1)) 
                                                    (let ((y.49 (let ((z.50 3)) z.50))) 
                                                        (let ((z.51 (let ((y.52 2)) (+ y.52 y.52)))) 
                                                                (if (let ((x.53 6)) (> x.53 7)) 
                                                                    9 
                                                                    10)))))) 
                (let ((y.12 (if (true) L.id1.2 L.id2.3))) (call y.12 5))))
        
        `(module
            (define L.id1.2 (lambda (x.10) x.10))
            (define L.id2.3
                (lambda (x.11)
                (begin
                    (set! x.48 1)
                    (begin
                    (set! y.49 (begin (set! z.50 3) z.50))
                    (begin
                        (set! z.51 (begin (set! y.52 2) (+ y.52 y.52)))
                        (if (begin (set! x.53 6) (> x.53 7)) 9 10))))))
            (begin (set! y.12 (if (true) L.id1.2 L.id2.3)) (call y.12 5)))))

; M6 Tests

(test-case "sequentialize 16 - tail is subtraction binop"
   (check-equal?
        (sequentialize-let
            `(module (- 2 3)))
        
        `(module (- 2 3))))

(test-case "sequentialize 17 - tail let value is call"
   (check-equal?
        (sequentialize-let
            `(module (let ((x.5 (call x.2 1 x.1))) (call x.5 2 3 4))))
        
        `(module (begin (set! x.5 (call x.2 1 x.1)) (call x.5 2 3 4)))))

(test-case "sequentialize 18 - value let value is call"
   (check-equal?
        (sequentialize-let
            `(module (let ((x.5 (let ((x.5 (call x.3))) (- 2 1)))) 
                          (call x.5 2 3 4))))
        
        `(module
            (begin (set! x.5 (begin (set! x.5 (call x.3)) (- 2 1))) (call x.5 2 3 4)))))

(test-case "sequentialize 19 - value if value is call"
   (check-equal?
        (sequentialize-let
            `(module (let ((x.5 (if (> 1 2) (call x.8) (call x.9 1 2 3)))) 
                          (call x.5 2 3 4))))
        
        `(module
            (begin (set! x.5 (if (> 1 2) (call x.8) (call x.9 1 2 3))) (call x.5 2 3 4)))))