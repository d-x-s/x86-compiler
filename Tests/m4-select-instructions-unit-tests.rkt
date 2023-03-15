#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt"
)

(test-case "select 1"
   (check-equal?
        (select-instructions
            `(module 2))
        
        `(module () (halt 2))))

(test-case "select 2"
   (check-equal?
        (select-instructions
            `(module (+ 1 x.1)))
        
        `(module () (begin (set! tmp.1 1) (set! tmp.1 (+ tmp.1 x.1)) (halt tmp.1)))))

(test-case "select 3"
   (check-equal?
        (select-instructions
            `(module (begin 
                        (set! x.1 2)
                        (set! x.2 x.3)
                        (begin 
                            (set! z.1 2)
                            (set! z.1 z.4))
                        (set! x.3 (+ y.1 y.2))
                        2)))
        
        `(module
            ()
            (begin
                (set! x.1 2)
                (set! x.2 x.3)
                (set! z.1 2)
                (set! z.1 z.4)
                (set! x.3 y.1)
                (set! x.3 (+ x.3 y.2))
                (halt 2)))))

(test-case "select 4"
   (check-equal?
        (select-instructions
            `(module (begin 
                        (set! x.1 2)
                        (set! x.2 x.3)
                        (begin 
                            (set! z.1 2)
                            (set! z.1 z.4))
                        (set! x.3 (+ y.1 y.2))
                        (begin 
                            (set! z.1 2)
                            x.2))))
        
        `(module
            ()
            (begin
                (set! x.1 2)
                (set! x.2 x.3)
                (set! z.1 2)
                (set! z.1 z.4)
                (set! x.3 y.1)
                (set! x.3 (+ x.3 y.2))
                (set! z.1 2)
                (halt x.2)))))

; M4 Tests

(test-case "select 5"
   (check-equal?
        (select-instructions
            `(module (if (true) 0 (+ 0 1))))
        
        `(module ()
            (if (true)
                (halt 0)
                (begin (set! tmp.2 0) (set! tmp.2 (+ tmp.2 1)) (halt tmp.2))))))

(test-case "select 6"
   (check-equal?
        (select-instructions
            `(module (if (> 0 1) 0 (+ 0 1))))
        
        `(module
            ()
            (if (begin (set! tmp.4 0) (> tmp.4 1))
                (halt 0)
                (begin (set! tmp.3 0) (set! tmp.3 (+ tmp.3 1)) (halt tmp.3))))))

(test-case "select 7"
   (check-equal?
        (select-instructions
            `(module (if (not (> 0 1)) 0 (+ 0 1))))
        
        `(module ()
            (if (not (begin (set! tmp.6 0) (> tmp.6 1)))
                (halt 0)
                (begin (set! tmp.5 0) (set! tmp.5 (+ tmp.5 1)) (halt tmp.5))))))

(test-case "select 8"
   (check-equal?
        (select-instructions
            `(module (if (begin (set! x.1 2) (set! x.2 (+ 1 2)) (true)) 
                         0 
                         (+ 0 1))))
        
        `(module ()
            (if (begin (set! x.1 2) (begin (set! x.2 1) (set! x.2 (+ x.2 2))) (true))
                (halt 0)
                (begin (set! tmp.7 0) (set! tmp.7 (+ tmp.7 1)) (halt tmp.7))))))

(test-case "select 9"
   (check-equal?
        (select-instructions
            `(module (if (if (> 1 2) (true) (false)) 
                         0 
                         (+ 0 1))))
        
        `(module ()
            (if (if (begin (set! tmp.9 1) (> tmp.9 2)) (true) (false))
                (halt 0)
                (begin (set! tmp.8 0) (set! tmp.8 (+ tmp.8 1)) (halt tmp.8))))))

(test-case "select 10"
   (check-equal?
        (select-instructions
            `(module (begin 
                        (set! x.1 2)
                        (if (true) (set! x.2 (+ 1 2)) (set! x.3 3))
                        2)))
        
        `(module ()
            (begin
                (set! x.1 2)
                (if (true) (begin (set! x.2 1) (set! x.2 (+ x.2 2))) (set! x.3 3))
                (halt 2)))))

(test-case "select 11"
   (check-equal?
        (select-instructions
            `(module 
                (begin 
                    (set! x.1 5) 
                    (if (true) 
                        (begin (set! y.2 (+ x.1 17)) (set! x.5 12)) 
                        (begin (set! x.5 15))) 
                    x.5)))
        
        `(module
            ()
            (begin
                (set! x.1 5)
                (if (true)
                    (begin (set! y.2 x.1) (set! y.2 (+ y.2 17)) (set! x.5 12))
                    (begin (set! x.5 15)))
                (halt x.5)))))

