#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt"
)

(test-case "normalize 1"
   (check-equal?
        (normalize-bind
            `(module x.1))
        
        `(module x.1)))

(test-case "normalize 2"
   (check-equal?
        (normalize-bind
            `(module (+ 1 2)))
        
        `(module (+ 1 2))))

(test-case "normalize 3"
   (check-equal?
        (normalize-bind
            `(module (begin (set! x.1 0) 2)))
        
        `(module (begin (set! x.1 0) 2))))

(test-case "normalize 4"
   (check-equal?
        (normalize-bind
            `(module (begin (set! x.1 0) (begin (set! x.1 0)) 2)))
        
        `(module (begin (set! x.1 0) (begin (set! x.1 0)) 2))))

(test-case "normalize 5"
   (check-equal?
        (normalize-bind
            `(module (begin 
                        (set! x.1 (begin 2))
                        x.1)))
        
        `(module (begin (begin (set! x.1 2)) x.1))))

(test-case "normalize 6"
   (check-equal?
        (normalize-bind
            `(module (begin 
                        (set! x.1 (begin (+ 1 2)))
                        (set! x.2 5)
                        x.1)))
        
        `(module (begin (begin (set! x.1 (+ 1 2))) (set! x.2 5) x.1))))

(test-case "normalize 7"
   (check-equal?
        (normalize-bind
            `(module (begin 
                        (set! x.1 (begin (set! x.2 3) x.2))
                        x.1)))
        
        `(module (begin (begin (set! x.2 3) (set! x.1 x.2)) x.1))))

(test-case "normalize 8"
   (check-equal?
        (normalize-bind
            `(module (begin 
                        (set! x.1 (begin (set! x.2 (begin (set! x.3 7) x.3)) x.2))
                        x.1)))
        
        `(module (begin (begin (begin (set! x.3 7) (set! x.2 x.3)) (set! x.1 x.2)) x.1))))

; M4 Tests

(test-case "normalize 9"
   (check-equal?
        (normalize-bind
            `(module (if (true)
                         (begin (set! x.1 1) 2)
                         (if (> 1 2) 3 4))))
        
        `(module (if (true) (begin (set! x.1 1) 2) (if (> 1 2) 3 4)))))

(test-case "normalize 10"
   (check-equal?
        (normalize-bind
            `(module (begin 
                        (set! x.5 (if (true) 
                                      (begin (set! y.2 14) 12) 
                                      (begin 15))) 
                        x.5)))
        
        `(module
            (begin
                (if (true) (begin (set! y.2 14) (set! x.5 12)) (begin (set! x.5 15)))
                x.5))))

(test-case "normalize 11"
   (check-equal?
        (normalize-bind
            `(module (begin 
                        (set! x.37 1) 
                        (begin 
                            (set! x.37 x.37) 
                            (set! y.39 (+ x.37 x.37)) 
                            (set! z.38 (begin (set! x.37 8) x.37)) 
                            (if (true) 
                                (begin (set! z.38 (+ x.37 7)) 8) 
                                9)))))
        
        `(module
            (begin
                (set! x.37 1)
                (begin
                (set! x.37 x.37)
                (set! y.39 (+ x.37 x.37))
                (begin (set! x.37 8) (set! z.38 x.37))
                (if (true) (begin (set! z.38 (+ x.37 7)) 8) 9))))))

(test-case "normalize 12"
   (check-equal?
        (normalize-bind
            `(module (begin 
                        (if (true) 
                            (set! x.3 (begin 
                                        (set! y.4 (begin (set! z.4 (+ 4 5)) z.4)) 
                                        y.4)) 
                            (set! x.3 y.7)) x.3)))
        
        `(module
            (begin
                (if (true)
                (begin (begin (set! z.4 (+ 4 5)) (set! y.4 z.4)) (set! x.3 y.4))
                (set! x.3 y.7))
                x.3))))

(test-case "normalize 13"
   (check-equal?
        (normalize-bind
            `(module (begin 
                        (set! x.1 0) 
                        (set! x.5 (if (true) 
                                      (begin (set! y.2 (+ x.1 17)) 12) 
                                      (begin 15))) 
                        x.5)))
        
        `(module
            (begin
                (set! x.1 0)
                (if (true)
                (begin (set! y.2 (+ x.1 17)) (set! x.5 12))
                (begin (set! x.5 15)))
                x.5))))

(test-case "normalize 13"
   (check-equal?
        (normalize-bind
            `(module (begin 
                        (set! x.37 20) 
                        (set! y.38 21) 
                        (if (not (> x.37 12)) 
                            (if (if (begin (set! z.39 x.37) (< y.38 z.39)) 
                                    (true) (false)) 
                                10 12) 
                            (+ x.37 y.38)))))
        
        `(module
            (begin
                (set! x.37 20)
                (set! y.38 21)
                (if (not (> x.37 12))
                (if (if (begin (set! z.39 x.37) (< y.38 z.39)) (true) (false)) 10 12)
                (+ x.37 y.38))))))

; M5 Tests

(test-case "normalize 14"
   (check-equal?
        (normalize-bind
            `(module (call x.5 1 2 3)))
        
        `(module (call x.5 1 2 3))))