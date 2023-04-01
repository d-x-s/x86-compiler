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

     `(module (call L.start.1 14 6 14))))

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
                        (if (fixnum? (void? x.1)) 1 2))))

     `(module
        (call
        x.1
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