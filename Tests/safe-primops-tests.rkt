#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt")

; M6 Tests

(test-case "primop 1 - simple binop"
    (check-match
        (implement-safe-primops
            `(module *))

     `(module
        (define L.*.1
            (lambda (tmp.1 tmp.2)
            (if (fixnum? tmp.2)
                (if (fixnum? tmp.1) (unsafe-fx* tmp.1 tmp.2) (error 1))
                (error 1))))
        L.*.1)))

(test-case "primop 2 - every binop"
    (check-match
        (implement-safe-primops
            `(module (call * + - < eq? <= >= >)))

     `(module
        (define L.*.8
            (lambda (tmp.1 tmp.2)
            (if (fixnum? tmp.2)
                (if (fixnum? tmp.1) (unsafe-fx* tmp.1 tmp.2) (error 1))
                (error 1))))
        (define L.>.7
            (lambda (tmp.11 tmp.12)
            (if (fixnum? tmp.12)
                (if (fixnum? tmp.11) (unsafe-fx> tmp.11 tmp.12) (error 6))
                (error 6))))
        (define L.>=.6
            (lambda (tmp.13 tmp.14)
            (if (fixnum? tmp.14)
                (if (fixnum? tmp.13) (unsafe-fx>= tmp.13 tmp.14) (error 7))
                (error 7))))
        (define L.<=.5
            (lambda (tmp.9 tmp.10)
            (if (fixnum? tmp.10)
                (if (fixnum? tmp.9) (unsafe-fx<= tmp.9 tmp.10) (error 5))
                (error 5))))
        (define L.eq?.4 (lambda (tmp.15 tmp.16) (eq? tmp.15 tmp.16)))
        (define L.<.3
            (lambda (tmp.7 tmp.8)
            (if (fixnum? tmp.8)
                (if (fixnum? tmp.7) (unsafe-fx< tmp.7 tmp.8) (error 4))
                (error 4))))
        (define L.-.2
            (lambda (tmp.5 tmp.6)
            (if (fixnum? tmp.6)
                (if (fixnum? tmp.5) (unsafe-fx- tmp.5 tmp.6) (error 3))
                (error 3))))
        (define L.+.1
            (lambda (tmp.3 tmp.4)
            (if (fixnum? tmp.4)
                (if (fixnum? tmp.3) (unsafe-fx+ tmp.3 tmp.4) (error 2))
                (error 2))))
        (call L.*.8 L.+.1 L.-.2 L.<.3 L.eq?.4 L.<=.5 L.>=.6 L.>.7))))

(test-case "primop 3 - every unop"
    (check-match
        (implement-safe-primops
            `(module (call L.s.1 fixnum? boolean? empty? void? ascii-char? error? not)))

     `(module
        (define L.not.7 (lambda (tmp.23) (not tmp.23)))
        (define L.error?.6 (lambda (tmp.22) (error? tmp.22)))
        (define L.ascii-char?.5 (lambda (tmp.21) (ascii-char? tmp.21)))
        (define L.void?.4 (lambda (tmp.20) (void? tmp.20)))
        (define L.empty?.3 (lambda (tmp.19) (empty? tmp.19)))
        (define L.boolean?.2 (lambda (tmp.18) (boolean? tmp.18)))
        (define L.fixnum?.1 (lambda (tmp.17) (fixnum? tmp.17)))
        (call
        L.s.1
        L.fixnum?.1
        L.boolean?.2
        L.empty?.3
        L.void?.4
        L.ascii-char?.5
        L.error?.6
        L.not.7))))

(test-case "primop 4 - define functions"
    (check-match
        (implement-safe-primops
            `(module 
                (define L.s.1 (lambda (x.1) (call x.1 void? #f 2 (void) #\a)))
                (define L.s.2 (lambda (y.1) (let ([x.1 L.w.1] [x.2 empty] [x.3 (error 0)] [x.4 +]) (call x.2 x.3 void? *))))
                (define L.s.3 (lambda (z.1 z.2) (if * #\a eq?)))
                (call L.s.1 fixnum? boolean? empty?)))

     `(module
        (define L.empty?.7 (lambda (tmp.19) (empty? tmp.19)))
        (define L.boolean?.6 (lambda (tmp.18) (boolean? tmp.18)))
        (define L.fixnum?.5 (lambda (tmp.17) (fixnum? tmp.17)))
        (define L.eq?.4 (lambda (tmp.15 tmp.16) (eq? tmp.15 tmp.16)))
        (define L.*.3
            (lambda (tmp.1 tmp.2)
            (if (fixnum? tmp.2)
                (if (fixnum? tmp.1) (unsafe-fx* tmp.1 tmp.2) (error 1))
                (error 1))))
        (define L.+.2
            (lambda (tmp.3 tmp.4)
            (if (fixnum? tmp.4)
                (if (fixnum? tmp.3) (unsafe-fx+ tmp.3 tmp.4) (error 2))
                (error 2))))
        (define L.void?.1 (lambda (tmp.20) (void? tmp.20)))
        (define L.s.1 (lambda (x.1) (call x.1 L.void?.1 #f 2 (void) #\a)))
        (define L.s.2
            (lambda (y.1)
            (let ((x.1 L.w.1) (x.2 empty) (x.3 (error 0)) (x.4 L.+.2))
                (call x.2 x.3 L.void?.1 L.*.3))))
        (define L.s.3 (lambda (z.1 z.2) (if L.*.3 #\a L.eq?.4)))
        (call L.s.1 L.fixnum?.5 L.boolean?.6 L.empty?.7))))

