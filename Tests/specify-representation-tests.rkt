#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt")

; M7 Tests

(test-case "specrep 1 - fixnums"
    (check-match
        (specify-representation
            `(module (call L.start.1 L.s.1 x.1 1 2 55 26 -2)))

     `(module (call L.start.1 L.s.1 x.1 8 16 440 208 -16))))

(test-case "specrep 2 - booleans"
    (check-match
        (specify-representation
            `(module (call L.start.1 #t #f)))

     `(module (call L.start.1 14 6))))

(test-case "specrep 3 - empty, void, errors"
    (check-match
        (specify-representation
            `(module (call L.start.1 empty (void) (error 55) (error 1) (error 0) (error 71))))

     `(module (call L.start.1 22 30 14142 318 62 18238))))

(test-case "specrep 4 - ascii chars"
    (check-match
        (specify-representation
            `(module (call L.start.1 #\a #\6 #\? #\space #\.)))

     `(module (call L.start.1 24878 13870 16174 8238 11822))))

(test-case "specrep 5 - binops"
    (check-match
        (specify-representation
            `(module (call x.1 
                        (unsafe-fx* 1 2) 
                        (unsafe-fx+ y.1 2) 
                        (unsafe-fx- 1 2) 
                        (eq? 1 #t) 
                        (unsafe-fx< 1 2) 
                        (unsafe-fx<= 1 2) 
                        (unsafe-fx> 1 x.2) 
                        (unsafe-fx>= 1 2))))

     `(module
        (call
        x.1
        (* 1 16)
        (+ y.1 16)
        (- 8 16)
        (if (= 8 14) 14 6)
        (if (< 8 16) 14 6)
        (if (<= 8 16) 14 6)
        (if (> 8 x.2) 14 6)
        (if (>= 8 16) 14 6)))))

(test-case "specrep 6 - unops"
    (check-match
        (specify-representation
            `(module (call x.1 
                        (fixnum? 1) 
                        (boolean? #t) 
                        (empty? empty) 
                        (void? (void)) 
                        (ascii-char? #\a) 
                        (error? (error 123)) 
                        (not #f) 
                        (not L.start.1) 
                        (not (if 45 46 47)))))

     `(module
        (call
        x.1
        (if (= (bitwise-and 8 7) 0) 14 6)
        (if (= (bitwise-and 14 247) 6) 14 6)
        (if (= (bitwise-and 22 255) 22) 14 6)
        (if (= (bitwise-and 30 255) 30) 14 6)
        (if (= (bitwise-and 24878 255) 46) 14 6)
        (if (= (bitwise-and 31550 255) 62) 14 6)
        (if (!= 6 6) 6 14)
        (if (!= L.start.1 6) 6 14)
        (if (!= (if (!= 360 6) 368 376) 6) 6 14)))))

(test-case "specrep 7 - nested binops"
    (check-match
        (specify-representation
            `(module (unsafe-fx* 
                        (unsafe-fx< 1 2) 
                        (unsafe-fx+ 
                            (unsafe-fx<= 3 4) 
                            (unsafe-fx- 
                                (eq? 1 (unsafe-fx> 5 6)) 
                                (unsafe-fx>= 7 8))))))

     `(module
        (* (if (< 8 16) 14 6)
           (arithmetic-shift-right
                (+ (if (<= 24 32) 14 6)
                   (- (if (= 8 (if (> 40 48) 14 6)) 14 6) (if (>= 56 64) 14 6)))
                3)))))

(test-case "specrep 8 - * combos"
    ; arithmetic shift is added to (* x y):
    ; - x and y are not constants: prefer shifting y
    ; - either x or y is a constant: shift the constant
    ; - both x and y are constants: prefer shifting x
    ; constants include: fixnum, boolean, empty, void, error, ascii
    ; https://www.students.cs.ubc.ca/~cs-411/2022w2/chp-immediates_book_top.html#%28def._%28%28lib._cpsc411%2Freference%2Fa7-solution..rkt%29._remove-complex-opera%2A%29%29:~:text=Only%20*%20poses,)).
    (check-match
        (specify-representation
            `(module (call x.1
                            (unsafe-fx* (unsafe-fx+ 1 2) (unsafe-fx>= 1 2))
                            (unsafe-fx* y.2 (unsafe-fx+ 1 2))
                            (unsafe-fx* (unsafe-fx>= 1 2) 2)
                            (unsafe-fx* #t (unsafe-fx< 1 2))
                            (unsafe-fx* (void) (unsafe-fx+ 1 2))
                            (unsafe-fx* (fixnum? 4) 6)
                            (unsafe-fx* x.2 L.start.1)
                            (unsafe-fx* (error 1) (error 1))
                            (unsafe-fx* #\a #\a))))

     `(module
        (call
        x.1
        (* (+ 8 16) (arithmetic-shift-right (if (>= 8 16) 14 6) 3))
        (* y.2 (arithmetic-shift-right (+ 8 16) 3))
        (* (if (>= 8 16) 14 6) 2)
        (* 1 (if (< 8 16) 14 6))
        (* 3 (+ 8 16))
        (* (if (= (bitwise-and 32 7) 0) 14 6) 6)
        (* x.2 (arithmetic-shift-right L.start.1 3))
        (* 39 318)
        (* 3109 24878)))))

(test-case "specrep 9 - let"
    (check-match
        (specify-representation
            `(module (let ([x.1 (unsafe-fx+ 1 2)] [x.2 L.start.1] [x.3 #t]) (call x.1 x.2 x.3 4))))

     `(module (let ((x.1 (+ 8 16)) (x.2 L.start.1) (x.3 14)) (call x.1 x.2 x.3 32)))))

(test-case "specrep 10 - ifs"
    (check-match
        (specify-representation
            `(module (call x.1 
                        (if #t 1 2) 
                        (if #f 1 2) 
                        (if x.1 1 2) 
                        (if (not x.1) 1 2)
                        (if (void) 1 2)
                        (if (error? x.5) 1 2)
                        (if (unsafe-fx* 3 5) 1 2)
                        (if (let ([x.2 #t]) x.2) 1 2)
                        (if (let ([x.2 #t]) (let ([x.3 #f]) x.3)) 1 2)
                        (if (let ([x.2 #t]) (let ([x.3 #f]) (eq? 1 2))) 1 2)
                        (if (if #t 3 4) 1 2)
                        (if (if #t (if #f y.1 y.2) 4) 1 (if #t 55 56))
                        (if (unsafe-fx> 1 2) 1 2)
                        (if (unsafe-fx* (if 1 2 3) 2) 1 2)
                        (if (fixnum? (void? x.1)) 1 2)
                        (if (call 5 6 7) 1 2)
                        (if (begin (cons x.1 x.2)) 1 2))))

     `(module
        (call x.1
        (if (!= 14 6) 8 16)
        (if (!= 6 6) 8 16)
        (if (!= x.1 6) 8 16)
        (if (not (!= x.1 6)) 8 16)
        (if (!= 30 6) 8 16)
        (if (!= (if (= (bitwise-and x.5 255) 62) 14 6) 6) 8 16)
        (if (!= (* 3 40) 6) 8 16)
        (if (let ((x.2 14)) (!= x.2 6)) 8 16)
        (if (let ((x.2 14)) (let ((x.3 6)) (!= x.3 6))) 8 16)
        (if (let ((x.2 14)) (let ((x.3 6)) (!= (if (= 8 16) 14 6) 6))) 8 16)
        (if (if (!= 14 6) (!= 24 6) (!= 32 6)) 8 16)
        (if (if (!= 14 6) (if (!= 6 6) (!= y.1 6) (!= y.2 6)) (!= 32 6))
            8
            (if (!= 14 6) 440 448))
        (if (!= (if (> 8 16) 14 6) 6) 8 16)
        (if (!= (* (if (!= 8 6) 16 24) 2) 6) 8 16)
        (if (!=
                (if (= (bitwise-and (if (= (bitwise-and x.1 255) 30) 14 6) 7) 0) 14 6)
                6)
            8
            16)
        (if (!= (call 40 48 56) 6) 8 16)
        (if (!=
                (begin
                (let ((tmp.1 (+ (alloc 16) 1)))
                    (begin (mset! tmp.1 -1 x.1) (mset! tmp.1 7 x.2) tmp.1)))
                6)
            8
            16)))))

(test-case "specrep 11 - define functions"
    (check-match
        (specify-representation
            `(module
                (define L.s.1 (lambda (x.1) (call L.start.1 L.s.1 x.1 5)))
                (define L.s.2 (lambda (x.1) (unsafe-fx- 
                                                (eq? 1 (unsafe-fx> 5 6)) 
                                                (unsafe-fx>= 7 8))))
                (let ([x.1 (unsafe-fx+ 1 2)] [x.2 L.start.1] [x.3 #t]) (call x.1 x.2 x.3 4))))

     `(module
        (define L.s.1 (lambda (x.1) (call L.start.1 L.s.1 x.1 40)))
        (define L.s.2
            (lambda (x.1)
            (- (if (= 8 (if (> 40 48) 14 6)) 14 6) (if (>= 56 64) 14 6))))
        (let ((x.1 (+ 8 16)) (x.2 L.start.1) (x.3 14)) (call x.1 x.2 x.3 32)))))

(test-case "specrep 12 - binops2"
    (check-match
        (specify-representation
            `(module (call x.1 
                        (unsafe-fx+ y.1 2) 
                        (unsafe-fx- 1 2) 
                        (eq? 1 #t) 
                        (unsafe-fx< 1 2) 
                        (unsafe-fx<= 1 2) 
                        (unsafe-fx> 1 x.2) 
                        (unsafe-fx>= 1 2))))

     `(module
        (call
        x.1
        (+ y.1 16)
        (- 8 16)
        (if (= 8 14) 14 6)
        (if (< 8 16) 14 6)
        (if (<= 8 16) 14 6)
        (if (> 8 x.2) 14 6)
        (if (>= 8 16) 14 6)))))

;  M8 Tests

(test-case "specrep 13 - pair, vector, cons, car, cdr"
    (check-match
        (specify-representation
            `(module (call x.1 
                        (pair? x.1) 
                        (vector? x.1) 
                        (cons x.1 (call (unsafe-fx+ 1 2))) 
                        (cons 5 6) 
                        (unsafe-car x.1) 
                        (unsafe-car (cons 5 6)) 
                        (unsafe-cdr x.3))))

     `(module
        (call x.1
        (if (= (bitwise-and x.1 7) 1) 14 6)
        (if (= (bitwise-and x.1 7) 3) 14 6)
        (let ((tmp.2 (+ (alloc 16) 1)))
            (begin (mset! tmp.2 -1 x.1) (mset! tmp.2 7 (call (+ 8 16))) tmp.2))
        (let ((tmp.3 (+ (alloc 16) 1)))
            (begin (mset! tmp.3 -1 40) (mset! tmp.3 7 48) tmp.3))
        (mref x.1 -1)
        (mref
            (let ((tmp.4 (+ (alloc 16) 1)))
            (begin (mset! tmp.4 -1 40) (mset! tmp.4 7 48) tmp.4))
            -1)
        (mref x.3 7)))))

(test-case "specrep 14 - unsafe-make-vector"
    (check-match
        (specify-representation
            `(module 
                (call x.1
                    (unsafe-make-vector L.start.1)
                    (unsafe-make-vector x.1) 
                    (unsafe-make-vector 55)
                    (unsafe-make-vector #t)
                    (unsafe-make-vector empty)
                    (unsafe-make-vector (void))
                    (unsafe-make-vector (error 1))
                    (unsafe-make-vector (unsafe-fx+ 1 2)) 
                    (unsafe-make-vector (unsafe-fx* x.2 x.1)) 
                    (unsafe-make-vector (fixnum? 1)) 
                    (unsafe-make-vector (not 1)) 
                    (unsafe-make-vector (unsafe-make-vector (unsafe-car (cons 1 2)))))))

     `(module
        (call x.1
        (let ((tmp.5 (+ (alloc (* (+ 1 (arithmetic-shift-right L.start.1 3)) 8)) 3)))
            (begin (mset! tmp.5 -3 L.start.1) tmp.5))
        (let ((tmp.6 (+ (alloc (* (+ 1 (arithmetic-shift-right x.1 3)) 8)) 3)))
            (begin (mset! tmp.6 -3 x.1) tmp.6))
        (let ((tmp.7 (+ (alloc 448) 3))) (begin (mset! tmp.7 -3 440) tmp.7))
        (let ((tmp.8 (+ (alloc 16) 3))) (begin (mset! tmp.8 -3 14) tmp.8))
        (let ((tmp.9 (+ (alloc 24) 3))) (begin (mset! tmp.9 -3 22) tmp.9))
        (let ((tmp.10 (+ (alloc 32) 3))) (begin (mset! tmp.10 -3 30) tmp.10))
        (let ((tmp.11 (+ (alloc 320) 3))) (begin (mset! tmp.11 -3 318) tmp.11))
        (let ((tmp.12 (+ (alloc (* (+ 1 (arithmetic-shift-right (+ 8 16) 3)) 8)) 3)))
            (begin (mset! tmp.12 -3 (+ 8 16)) tmp.12))
        (let ((tmp.13 (+ (alloc (* (+ 1 (arithmetic-shift-right (* x.2 (arithmetic-shift-right x.1 3)) 3)) 8)) 3)))
            (begin (mset! tmp.13 -3 (* x.2 (arithmetic-shift-right x.1 3))) tmp.13))
        (let ((tmp.14 (+ (alloc (* (+ 1 (arithmetic-shift-right (if (= (bitwise-and 8 7) 0) 14 6) 3)) 8)) 3)))
            (begin (mset! tmp.14 -3 (if (= (bitwise-and 8 7) 0) 14 6)) tmp.14))
        (let ((tmp.15 (+ (alloc (* (+ 1 (arithmetic-shift-right (if (!= 8 6) 6 14) 3)) 8)) 3)))
            (begin (mset! tmp.15 -3 (if (!= 8 6) 6 14)) tmp.15))
        (let ((tmp.16 (+ (alloc (* (+ 1 (arithmetic-shift-right
                    (let ((tmp.17 (+ (alloc (* (+ 1 (arithmetic-shift-right (mref
                                    (let ((tmp.18 (+ (alloc 16) 1)))
                                    (begin (mset! tmp.18 -1 8) (mset! tmp.18 7 16) tmp.18)) -1) 3))  8)) 3)))
                        (begin  (mset! tmp.17 -3 (mref (let ((tmp.18 (+ (alloc 16) 1)))
                            (begin (mset! tmp.18 -1 8) (mset! tmp.18 7 16) tmp.18)) -1)) tmp.17)) 3)) 8)) 3)))
            (begin (mset! tmp.16 -3 (let ((tmp.17 (+ (alloc
                        (* (+ 1 (arithmetic-shift-right
                            (mref (let ((tmp.18 (+ (alloc 16) 1))) (begin (mset! tmp.18 -1 8) (mset! tmp.18 7 16) tmp.18))
                            -1) 3)) 8)) 3)))
                (begin (mset! tmp.17 -3
                    (mref (let ((tmp.18 (+ (alloc 16) 1)))  (begin (mset! tmp.18 -1 8) (mset! tmp.18 7 16) tmp.18))
                    -1)) tmp.17))) tmp.16))))))

(test-case "specrep 15 - unsafe-vector-length"
    (check-match
        (specify-representation
            `(module 
                (call x.1
                    (unsafe-vector-length L.start.1)
                    (unsafe-vector-length x.1) 
                    (unsafe-vector-length 55)
                    (unsafe-vector-length (unsafe-fx+ 1 2)) 
                    (unsafe-vector-length (fixnum? 1)) 
                    (unsafe-vector-length (not 1)))))

     `(module
        (call x.1
        (mref L.start.1 -3)
        (mref x.1 -3)
        (mref 440 -3)
        (mref (+ 8 16) -3)
        (mref (if (= (bitwise-and 8 7) 0) 14 6) -3)
        (mref (if (!= 8 6) 6 14) -3)))))

(test-case "specrep 16 - unsafe-vector-set!"
    (check-match
        (specify-representation
            `(module 
                (begin 
                    (unsafe-vector-set! vec.4 0 0) 
                    (unsafe-vector-set! (unsafe-fx+ 1 2) x.1 L.start.1) 
                    (unsafe-vector-set! (void) #t (boolean? #f)) 
                    (unsafe-vector-set! (call x.1) (cons y.1 y.2) (let ([u.1 2]) u.1)) 
                    (unsafe-vector-set! empty (void) (eq? #f (void))) 
                    (void))))

     `(module
        (begin
            (mset! vec.4 5 0)
            (mset! (+ 8 16) (+ (* (arithmetic-shift-right x.1 3) 8) 5) L.start.1)
            (mset! 30 13 (if (= (bitwise-and 6 247) 6) 14 6))
            (mset!
            (call x.1)
            (+
            (*
            (arithmetic-shift-right
                (let ((tmp.19 (+ (alloc 16) 1)))
                (begin (mset! tmp.19 -1 y.1) (mset! tmp.19 7 y.2) tmp.19))
                3)
            8)
            5)
            (let ((u.1 16)) u.1))
            (mset! 22 29 (if (= 6 30) 14 6))
            30))))

(test-case "specrep 17 - unsafe-vector-ref"
    (check-match
        (specify-representation
            `(module 
                (call x.1 
                    (unsafe-vector-ref vec.4 0) 
                    (unsafe-vector-ref #t L.start.1) 
                    (unsafe-vector-ref (call x.1 x.2) (if (not #t) 55 56))
                    (void))))

     `(module
        (call
        x.1
        (mref vec.4 5)
        (mref 14 (+ (* (arithmetic-shift-right L.start.1 3) 8) 5))
        (mref
            (call x.1 x.2)
            (+ (* (arithmetic-shift-right (if (not (!= 14 6)) 440 448) 3) 8) 5))
        30))))

