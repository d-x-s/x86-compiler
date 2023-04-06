#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt")

; M8 Tests

(test-case "primop 1 - every triv; simple prim-f"
    (check-match
        (implement-safe-primops
            `(module (call L.start.1
                           L.tmp.2 
                           z.5
                           vector?
                           55
                           #t 
                           #f 
                           empty 
                           (void) 
                           (error 3) 
                           #\a)))

        `(module
            (define L.vector?.5 (lambda (tmp.44) (vector? tmp.44)))
            (call L.start.1 L.tmp.2 z.5 L.vector?.5 55 #t #f empty (void) (error 3) #\a))))

(test-case "primop 2 - binops"
    (check-match
        (implement-safe-primops
            `(module (call * + - < eq? <= >= > cons)))

     `(module
        (define L.*.13
            (lambda (tmp.14 tmp.15)
            (if (fixnum? tmp.15)
                (if (fixnum? tmp.14) (unsafe-fx* tmp.14 tmp.15) (error 1))
                (error 1))))
        (define L.cons.12 (lambda (tmp.46 tmp.47) (cons tmp.46 tmp.47)))
        (define L.>.11
            (lambda (tmp.24 tmp.25)
            (if (fixnum? tmp.25)
                (if (fixnum? tmp.24) (unsafe-fx> tmp.24 tmp.25) (error 6))
                (error 6))))
        (define L.>=.10
            (lambda (tmp.26 tmp.27)
            (if (fixnum? tmp.27)
                (if (fixnum? tmp.26) (unsafe-fx>= tmp.26 tmp.27) (error 7))
                (error 7))))
        (define L.<=.9
            (lambda (tmp.22 tmp.23)
            (if (fixnum? tmp.23)
                (if (fixnum? tmp.22) (unsafe-fx<= tmp.22 tmp.23) (error 5))
                (error 5))))
        (define L.eq?.8 (lambda (tmp.48 tmp.49) (eq? tmp.48 tmp.49)))
        (define L.<.7
            (lambda (tmp.20 tmp.21)
            (if (fixnum? tmp.21)
                (if (fixnum? tmp.20) (unsafe-fx< tmp.20 tmp.21) (error 4))
                (error 4))))
        (define L.-.6
            (lambda (tmp.18 tmp.19)
            (if (fixnum? tmp.19)
                (if (fixnum? tmp.18) (unsafe-fx- tmp.18 tmp.19) (error 3))
                (error 3))))
        (define L.+.5
            (lambda (tmp.16 tmp.17)
            (if (fixnum? tmp.17)
                (if (fixnum? tmp.16) (unsafe-fx+ tmp.16 tmp.17) (error 2))
                (error 2))))
        (call L.*.13 L.+.5 L.-.6 L.<.7 L.eq?.8 L.<=.9 L.>=.10 L.>.11 L.cons.12))))

(test-case "primop 3 - unops"
    (check-match
        (implement-safe-primops
            `(module (call L.s.1 fixnum? boolean? empty? void? ascii-char? error? not pair? vector?)))

     `(module
        (define L.vector?.13 (lambda (tmp.44) (vector? tmp.44)))
        (define L.pair?.12 (lambda (tmp.43) (pair? tmp.43)))
        (define L.not.11 (lambda (tmp.45) (not tmp.45)))
        (define L.error?.10 (lambda (tmp.42) (error? tmp.42)))
        (define L.ascii-char?.9 (lambda (tmp.41) (ascii-char? tmp.41)))
        (define L.void?.8 (lambda (tmp.40) (void? tmp.40)))
        (define L.empty?.7 (lambda (tmp.39) (empty? tmp.39)))
        (define L.boolean?.6 (lambda (tmp.38) (boolean? tmp.38)))
        (define L.fixnum?.5 (lambda (tmp.37) (fixnum? tmp.37)))
        (call
        L.s.1
        L.fixnum?.5
        L.boolean?.6
        L.empty?.7
        L.void?.8
        L.ascii-char?.9
        L.error?.10
        L.not.11
        L.pair?.12
        L.vector?.13))))

(test-case "primop 4 - vector ops"
    (check-match
        (implement-safe-primops
            `(module (call L.s.1 cons car cdr make-vector vector-length vector-set! vector-ref)))

     `(module
        (define L.unsafe-vector-ref.3
            (lambda (tmp.11 tmp.12)
            (if (unsafe-fx< tmp.12 (unsafe-vector-length tmp.11))
                (if (unsafe-fx>= tmp.12 0)
                (unsafe-vector-ref tmp.11 tmp.12)
                (error 11))
                (error 11))))
        (define L.vector-ref.11
            (lambda (tmp.33 tmp.34)
            (if (fixnum? tmp.34)
                (if (vector? tmp.33)
                (call L.unsafe-vector-ref.3 tmp.33 tmp.34)
                (error 11))
                (error 11))))
        (define L.unsafe-vector-set!.2
            (lambda (tmp.6 tmp.7 tmp.8)
            (if (unsafe-fx< tmp.7 (unsafe-vector-length tmp.6))
                (if (unsafe-fx>= tmp.7 0)
                (begin (unsafe-vector-set! tmp.6 tmp.7 tmp.8) (void))
                (error 10))
                (error 10))))
        (define L.vector-set!.10
            (lambda (tmp.30 tmp.31 tmp.32)
            (if (fixnum? tmp.31)
                (if (vector? tmp.30)
                (call L.unsafe-vector-set!.2 tmp.30 tmp.31 tmp.32)
                (error 10))
                (error 10))))
        (define L.vector-length.9
            (lambda (tmp.29)
            (if (vector? tmp.29) (unsafe-vector-length tmp.29) (error 9))))
        (define L.vector-init-loop.4
            (lambda (len.3 i.5 vec.4)
            (if (eq? len.3 i.5)
                vec.4
                (begin
                (unsafe-vector-set! vec.4 i.5 0)
                (call L.vector-init-loop.4 len.3 (unsafe-fx+ i.5 1) vec.4)))))
        (define L.make-init-vector.1
            (lambda (tmp.1)
            (if (unsafe-fx>= tmp.1 0)
                (let ((tmp.2 (unsafe-make-vector tmp.1)))
                (call L.vector-init-loop.4 tmp.1 0 tmp.2))
                (error 12))))
        (define L.make-vector.8
            (lambda (tmp.28)
            (if (fixnum? tmp.28) (call L.make-init-vector.1 tmp.28) (error 8))))
        (define L.cdr.7
            (lambda (tmp.36) (if (pair? tmp.36) (unsafe-cdr tmp.36) (error 13))))
        (define L.car.6
            (lambda (tmp.35) (if (pair? tmp.35) (unsafe-car tmp.35) (error 12))))
        (define L.cons.5 (lambda (tmp.46 tmp.47) (cons tmp.46 tmp.47)))
        (call
        L.s.1
        L.cons.5
        L.car.6
        L.cdr.7
        L.make-vector.8
        L.vector-length.9
        L.vector-set!.10
        L.vector-ref.11))))

(test-case "primop 5 - define functions"
    (check-match
        (implement-safe-primops
            `(module 
                (define L.s.1 (lambda (x.1) (call x.1 void? #f 2 (void) #\a)))
                (define L.s.2 (lambda (y.1) (let ([x.1 L.w.1] [x.2 empty] [x.3 (error 0)] [x.4 +]) (call x.2 x.3 void? *))))
                (define L.s.3 (lambda (z.1 z.2) (if * #\a eq?)))
                (call L.s.1 fixnum? boolean? empty?)))

     `(module
        (define L.empty?.11 (lambda (tmp.39) (empty? tmp.39)))
        (define L.boolean?.10 (lambda (tmp.38) (boolean? tmp.38)))
        (define L.fixnum?.9 (lambda (tmp.37) (fixnum? tmp.37)))
        (define L.eq?.8 (lambda (tmp.48 tmp.49) (eq? tmp.48 tmp.49)))
        (define L.*.7
            (lambda (tmp.14 tmp.15)
            (if (fixnum? tmp.15)
                (if (fixnum? tmp.14) (unsafe-fx* tmp.14 tmp.15) (error 1))
                (error 1))))
        (define L.+.6
            (lambda (tmp.16 tmp.17)
            (if (fixnum? tmp.17)
                (if (fixnum? tmp.16) (unsafe-fx+ tmp.16 tmp.17) (error 2))
                (error 2))))
        (define L.void?.5 (lambda (tmp.40) (void? tmp.40)))
        (define L.s.1 (lambda (x.1) (call x.1 L.void?.5 #f 2 (void) #\a)))
        (define L.s.2
            (lambda (y.1)
            (let ((x.1 L.w.1) (x.2 empty) (x.3 (error 0)) (x.4 L.+.6))
                (call x.2 x.3 L.void?.5 L.*.7))))
        (define L.s.3 (lambda (z.1 z.2) (if L.*.7 #\a L.eq?.8)))
        (call L.s.1 L.fixnum?.9 L.boolean?.10 L.empty?.11))))

(test-case "primop 6 - nested let"
    (check-match
        (implement-safe-primops
            `(module 
                (let ((x.1.61 (call make-vector 3)))
                     (let ((x.2.62 (call vector-set! x.1.61 0 1)))
                          (let ((x.3.63 (call vector-set! x.1.61 1 2)))
                               (let ((x.4.64 (call vector-set! x.1.61 2 3)))
                                    (call vector-ref x.1.61 2)))))))

     `(module
        (define L.unsafe-vector-ref.3
            (lambda (tmp.11 tmp.12)
            (if (unsafe-fx< tmp.12 (unsafe-vector-length tmp.11))
                (if (unsafe-fx>= tmp.12 0)
                (unsafe-vector-ref tmp.11 tmp.12)
                (error 11))
                (error 11))))
        (define L.vector-ref.7
            (lambda (tmp.33 tmp.34)
            (if (fixnum? tmp.34)
                (if (vector? tmp.33)
                (call L.unsafe-vector-ref.3 tmp.33 tmp.34)
                (error 11))
                (error 11))))
        (define L.unsafe-vector-set!.2
            (lambda (tmp.6 tmp.7 tmp.8)
            (if (unsafe-fx< tmp.7 (unsafe-vector-length tmp.6))
                (if (unsafe-fx>= tmp.7 0)
                (begin (unsafe-vector-set! tmp.6 tmp.7 tmp.8) (void))
                (error 10))
                (error 10))))
        (define L.vector-set!.6
            (lambda (tmp.30 tmp.31 tmp.32)
            (if (fixnum? tmp.31)
                (if (vector? tmp.30)
                (call L.unsafe-vector-set!.2 tmp.30 tmp.31 tmp.32)
                (error 10))
                (error 10))))
        (define L.vector-init-loop.4
            (lambda (len.3 i.5 vec.4)
            (if (eq? len.3 i.5)
                vec.4
                (begin
                (unsafe-vector-set! vec.4 i.5 0)
                (call L.vector-init-loop.4 len.3 (unsafe-fx+ i.5 1) vec.4)))))
        (define L.make-init-vector.1
            (lambda (tmp.1)
            (if (unsafe-fx>= tmp.1 0)
                (let ((tmp.2 (unsafe-make-vector tmp.1)))
                (call L.vector-init-loop.4 tmp.1 0 tmp.2))
                (error 12))))
        (define L.make-vector.5
            (lambda (tmp.28)
            (if (fixnum? tmp.28) (call L.make-init-vector.1 tmp.28) (error 8))))
        (let ((x.1.61 (call L.make-vector.5 3)))
            (let ((x.2.62 (call L.vector-set!.6 x.1.61 0 1)))
            (let ((x.3.63 (call L.vector-set!.6 x.1.61 1 2)))
                (let ((x.4.64 (call L.vector-set!.6 x.1.61 2 3)))
                (call L.vector-ref.7 x.1.61 2))))))))

