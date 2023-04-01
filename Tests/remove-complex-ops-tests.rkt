#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt")

; M7 Tests

(test-case "remop 1 - simple binop"
    (check-match
        (remove-complex-opera*
            `(module (+ 1 2)))

     `(module (+ 1 2))))

(test-case "remop 2 - binop val1 is atomic"
    (check-match
        (remove-complex-opera*
            `(module (+ 2 (- 4 1))))

     `(module (let ([tmp.1 (- 4 1)]) (+ 2 tmp.1)))))

(test-case "remop 3 - binop val2 is atomic"
    (check-match
        (remove-complex-opera*
            `(module (+ (- 4 1) 2)))

     `(module (let ([tmp.2 (- 4 1)]) (+ tmp.2 2)))))

(test-case "remop 4 - binop val1 and val2 are not atomic"
    (check-match
        (remove-complex-opera*
            `(module (+ (call x.1) (call x.2))))

     `(module (let ([tmp.3 (call x.1)]) (let ([tmp.4 (call x.2)]) (+ tmp.3 tmp.4))))))     

(test-case "remop 5 - complex nested binops"
    (check-match
        (remove-complex-opera*
            `(module (+ (+ 3 (* y.1 y.3)) (+ (- x.3 x.4) (+ 1 2)))))

     `(module
        (let ([tmp.5 (let ([tmp.6 (* y.1 y.3)]) (+ 3 tmp.6))])
            (let ([tmp.7
                (let ([tmp.8 (- x.3 x.4)])
                    (let ([tmp.9 (+ 1 2)]) (+ tmp.8 tmp.9)))])
            (+ tmp.5 tmp.7))))))

(test-case "remop 6 - value is if"
    (check-match
        (remove-complex-opera*
            `(module (if (true) 
                         (if (false) 
                             (+ 1 (call x.1)) 
                             L.start.1) 
                         3)))

     `(module
        (if (true) (if (false) (let ((tmp.10 (call x.1))) (+ 1 tmp.10)) L.start.1) 3))))

(test-case "remop 7 - value is let"
    (check-match
        (remove-complex-opera*
            `(module (let ([x.1 1] 
                           [x.2 (call x.1 (+ 1 2))]) 
                          x.1)))

     `(module (let ((x.1 1) (x.2 (let ((tmp.11 (+ 1 2))) (call x.1 tmp.11)))) x.1))))

(test-case "remop 8 - value is call with non-atomic params"
    (check-match
        (remove-complex-opera*
            `(module (call L.start.1 1 2 (+ 3 (- 3 4)) (+ 4 5) y.1 (call x.1))))

     `(module
        (let ([tmp.12 (let ((tmp.13 (- 3 4))) (+ 3 tmp.13))])
            (let ([tmp.14 (+ 4 5)])
            (let ([tmp.15 (call x.1)]) (call L.start.1 1 2 tmp.12 tmp.14 y.1 tmp.15)))))))

(test-case "remop 9 - value is if, pred is if, pred is let"
    (check-match
        (remove-complex-opera*
            `(module (if (> 1 2) 
                         (call x.1 1 (+ 1 2)) 
                         (if (not (true))
                             (let ([x.1 (+ 1 (* 2 3))]) x.1)
                             (if (if (let ([x.3 5] [x.4 6]) (= x.3 x.4)) (true) (false))
                                 L.start.1
                                 L.start.2)))))

     `(module
        (if (> 1 2)
            (let ((tmp.16 (+ 1 2))) (call x.1 1 tmp.16))
            (if (not (true))
            (let ((x.1 (let ((tmp.17 (* 2 3))) (+ 1 tmp.17)))) x.1)
            (if (if (let ((x.3 5) (x.4 6)) (= x.3 x.4)) (true) (false))
                L.start.1
                L.start.2))))))

(test-case "remop 10 - define functions"
    (check-match
        (remove-complex-opera*
            `(module
                (define L.fn.1 (lambda (x.1 x.2) (call L.fn.2 3 (call L.fn.2 4 5))))
                (define L.fn.2 (lambda (x.3 x.4) (if (let ([x.1 1] [x.2 2]) (<= x.1 x.2)) 
                                   (* 24 (* 11 5))
                                   (* (+ 55 2) (if (>= 3 4) 3 4)))))
                (call L.fn.1 1 (+ 1 2) 3)))

     `(module
        (define L.fn.1
            (lambda (x.1 x.2) (let ((tmp.18 (call L.fn.2 4 5))) (call L.fn.2 3 tmp.18))))
        (define L.fn.2
            (lambda (x.3 x.4)
            (if (let ((x.1 1) (x.2 2)) (<= x.1 x.2))
                (let ((tmp.19 (* 11 5))) (* 24 tmp.19))
                (let ((tmp.20 (+ 55 2)))
                (let ((tmp.21 (if (>= 3 4) 3 4))) (* tmp.20 tmp.21))))))
        (let ((tmp.22 (+ 1 2))) (call L.fn.1 1 tmp.22 3)))))

