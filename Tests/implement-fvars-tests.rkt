#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt")

(test-case "fvars 1 - tail is jump"
    (check-equal?
        (implement-fvars
            '(module (jump L.start.1)))

      '(module (jump L.start.1))))

(test-case "fvars 2 - tail is jump fvar"
    (check-equal?
        (implement-fvars
            '(module (jump fv3)))

        '(module (jump (rbp - 24)))))

(test-case "fvars 3 - tail is if, pred is relop"
    (check-equal?
        (implement-fvars
            '(module (if (!= fv3 fv1) (jump fv2) (jump fv3))))

        '(module (if (!= (rbp - 24) (rbp - 8)) 
                     (jump (rbp - 16)) 
                     (jump (rbp - 24))))))

(test-case "fvars 4 - pred is begin"
    (check-equal?
        (implement-fvars
            '(module 
                (if (begin (set! fv1 L.s.1) (set! fv2 (+ fv2 rsp)) (false)) 
                    (jump fv1) 
                    (jump fv2))))

        '(module
            (if (begin
                    (set! (rbp - 8) L.s.1)
                    (set! (rbp - 16) (+ (rbp - 16) rsp))
                    (false))
                (jump (rbp - 8))
                (jump (rbp - 16))))))

(test-case "fvars 5 - pred is if"
    (check-equal?
        (implement-fvars
            '(module (if (if (true) (> r13 fv5) (<= fv4 fv5)) 
                         (jump L.s.1) 
                         (jump L.s.2))))

        '(module
            (if (if (true) (> r13 (rbp - 40)) (<= (rbp - 32) (rbp - 40)))
                (jump L.s.1)
                (jump L.s.2)))))

(test-case "fvars 6 - tail is begin, simple fbp acc"
    (check-equal?
        (implement-fvars
            '(module 
                (begin
                    (set! rbp (- rbp 8))
                    (return-point L.rp.8
                        (begin
                        (set! rdi fv3)
                        (jump L.f.1)))
                    (set! rbp (+ rbp 8))
                    (jump fv0))))

        '(module
            (begin
                (set! rbp (- rbp 8))
                (return-point L.rp.8 
                    (begin 
                        (set! rdi (rbp - 16)) ; translate frame variables relative to frame allocations introduced around return points
                        (jump L.f.1)))
                (set! rbp (+ rbp 8))
                (jump (rbp - 0))))))

(test-case "fvars 7 - set triv, set binop"
    (check-equal?
        (implement-fvars
            '(module
                (begin
                    (set! rbp (+ rbp 8))
                    (set! rbp (* rbp 2))
                    (set! rbp 0)
                    (set! rbp (- rbp 8))
                    (set! r8 (bitwise-and r8 8))
                    (set! fv1 fv2)
                    (set! fv1 (+ fv1 1))
                    (jump fv0))))

        '(module
            (begin
                (set! rbp (+ rbp 8))
                (set! rbp (* rbp 2))
                (set! rbp 0)
                (set! rbp (- rbp 8))
                (set! r8 (bitwise-and r8 8))
                (set! (rbp - 16) (rbp - 24))
                (set! (rbp - 16) (+ (rbp - 16) 1))
                (jump (rbp - 8))))))

(test-case "fvars 8 - mset, set mref"
    (check-equal?
        (implement-fvars
            '(module
                (begin
                    (mset! rbp 5 8)
                    (mset! fv0 rbp rbp)
                    (set! rbp (mref rbp 8))
                    (set! rbp (mref fv1 rbp))
                    (jump fv0))))

        '(module
            (begin
                (mset! rbp 5 8)
                (mset! (rbp - 0) rbp rbp)
                (set! rbp (mref rbp 8))
                (set! rbp (mref (rbp - 8) rbp))
                (jump (rbp - 0))))))

(test-case "fvars 9 - if begin tail1 tail2"
    (check-equal?
        (implement-fvars
            '(module 
                (if (begin (set! rbp (+ rbp 8)) (set! fv0 fv1) (true))
                    (begin 
                        (set! rbp (+ rbp 8))
                        (jump fv0))
                    (begin
                        (set! rbp (- rbp 8))
                        (jump fv0)))))

        '(module
            (if (begin (set! rbp (+ rbp 8)) (set! (rbp - 8) (rbp - 16)) (true))
                (begin (set! rbp (+ rbp 8)) (jump (rbp - 8)))
                (begin (set! rbp (- rbp 8)) (jump (rbp - -8)))))))

(test-case "fvars 10 - define function, nested begin, return-point begin"
   (check-equal?
        (implement-fvars
            '(module 
                (define L.fact.1 
                    (begin 
                        (set! fv0 r15) 
                        (set! rbp (+ rbp 8))
                        (if (= fv1 0) 
                            (begin (set! rax 1) (set! rbp (- rbp 32)) (jump fv0)) 
                            (begin 
                                (set! r15 fv1) 
                                (set! r15 (+ r15 -1)) 
                                (begin 
                                    (set! rbp (- rbp 16)) 
                                    (return-point L.rp.21 (begin (set! rbp (* rbp 8)) (set! r15 L.rp.21) (jump L.fact.1))) 
                                    (set! rbp (+ rbp 16))) 
                                (set! r15 rax) 
                                (set! rax fv1) 
                                (set! rax (* rax r15)) 
                                (jump fv0))))) 
                (begin 
                    (set! r15 r15) 
                    (set! rdi 5) 
                    (set! r15 r15) 
                    (jump fv0))))

        
        `(module
            (define L.fact.1
                (begin
                    (set! (rbp - 0) r15)
                    (set! rbp (+ rbp 8))  ; offset 8
                    (if (= (rbp - 16) 0)
                        (begin (set! rax 1) (set! rbp (- rbp 32)) (jump (rbp - -24))) ; offset 24
                        (begin ; offset 8
                            (set! r15 (rbp - 16))
                            (set! r15 (+ r15 -1))
                            (begin
                                (set! rbp (- rbp 16)) ; offset -8
                                (return-point
                                    L.rp.21
                                    (begin (set! rbp (* rbp 8)) (set! r15 L.rp.21) (jump L.fact.1))) ; offset -64
                                (set! rbp (+ rbp 16))) ; offset 8
                            (set! r15 rax)
                            (set! rax (rbp - 16))
                            (set! rax (* rax r15))
                            (jump (rbp - 8))))))
            (begin (set! r15 r15) (set! rdi 5) (set! r15 r15) (jump (rbp - 0)))))) ; offset 0

(test-case "fvars 11 - nested effect, effect is if"
   (check-equal?
        (implement-fvars 
            `(module 
                (begin 
                    (set! rbp (- rbp 8)) 
                    (set! fv1 10) 
                    (begin 
                        (return-point L.rp.1
                            (begin
                                (set! rbp (- rbp 16)) 
                                (set! fv5 30)
                                (jump L.start.1)))
                        (if (true)
                            (set! rbp (+ rbp 16))
                            (set! rbp (+ rbp 16))))
                    (jump fv0))))

        `(module
            (begin
                (set! rbp (- rbp 8))
                (set! (rbp - 0) 10)
                (begin
                (return-point
                    L.rp.1
                    (begin (set! rbp (- rbp 16)) (set! (rbp - 16) 30) (jump L.start.1)))
                (if (true) 
                    (set! rbp (+ rbp 16)) 
                    (set! rbp (+ rbp 16))))
                (jump (rbp - 8))))))


; (test-case "fvars  - "
;     (check-equal?
;         (implement-fvars
;             '())

;         '()))

; (test-case "fvars  - "
;     (check-equal?
;         (implement-fvars
;             '())

;         '()))

; (test-case "fvars  - "
;     (check-equal?
;         (implement-fvars
;             '())

;         '()))
