#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt"
)


(test-case "impose 1"
   (check-equal?
      (impose-calling-conventions
         '(module
            (begin (set! x.1 9) 100)))

      '(module (begin (set! x.1 9) 100))))

(test-case "impose 2"
   (check-equal?
      (impose-calling-conventions
         '(module
            (begin 
               (set! x.1 9)
               (begin (set! x.2 (* 1 x.2)) (set! a.1 (+ y.1 x.1)))
               (if (= x.1 x.2)
                   (set! x.2 2)
                   (set! x.2 4))
               (+ x.1 1))))

      '(module
         (begin
            (set! x.1 9)
            (begin (set! x.2 (* 1 x.2)) (set! a.1 (+ y.1 x.1)))
            (if (= x.1 x.2) (set! x.2 2) (set! x.2 4))
            (+ x.1 1)))))

(test-case "impose 3"
   (check-equal?
      (impose-calling-conventions
         '(module
            (begin 
               (set! x.1 9)
               (begin 
                  (set! x.2 (* 1 x.2)) 
                  (set! a.1 (+ y.1 x.1))
                  (if (not (not (false))) L.start.1 x.1)))))

      '(module
         (begin
            (set! x.1 9)
            (begin
               (set! x.2 (* 1 x.2))
               (set! a.1 (+ y.1 x.1))
               (if (not (not (false))) L.start.1 x.1))))))

(test-case "impose 4"
   (check-equal?
      (impose-calling-conventions
         '(module
            (begin 
               (set! x.2 (* 1 x.2)) 
               (set! a.1 (+ y.1 x.1))
               (if (begin (set! x.1 9) (set! x.2 1) (> x.1 x.2)) 
                  L.start.1
                  (call L.start.1 x.1 x.2 x.3)))))

      '(module
         (begin
            (set! x.2 (* 1 x.2))
            (set! a.1 (+ y.1 x.1))
            (if (begin (set! x.1 9) (set! x.2 1) (> x.1 x.2))
               L.start.1
               (begin
               (set! rdx x.3)
               (set! rsi x.2)
               (set! rdi x.1)
               (jump L.start.1 rbp rdi rsi rdx)))))))

(test-case "impose 5"
   (check-equal?
      (impose-calling-conventions
         '(module
            (begin (set! x.1 9) (call x.1))))

      '(module (begin (set! x.1 9) (begin (jump x.1 rbp))))))

(test-case "impose 6"
   (check-equal?
      (impose-calling-conventions
         '(module
            (begin (set! x.1 9) (call x.1 100 x.2))))

      '(module
         (begin
            (set! x.1 9)
            (begin (set! rsi x.2) (set! rdi 100) (jump x.1 rbp rdi rsi))))))

(test-case "impose 7"
   (check-equal?
      (impose-calling-conventions
         '(module
            (begin (set! x.1 9) (call x.1 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17))))

      '(module
         (begin
            (set! x.1 9)
            (begin
               (set! fv10 17)
               (set! fv9 16)
               (set! fv8 15)
               (set! fv7 14)
               (set! fv6 13)
               (set! fv5 12)
               (set! fv4 11)
               (set! fv3 10)
               (set! fv2 9)
               (set! fv1 8)
               (set! fv0 7)
               (set! r9 6)
               (set! r8 5)
               (set! rcx 4)
               (set! rdx 3)
               (set! rsi 2)
               (set! rdi 1)
               (jump x.1 rbp rdi rsi rdx rcx r8 r9 fv0 fv1 fv2 fv3 fv4 fv5 fv6 fv7 fv8 fv9 fv10))))))

(test-case "impose 8"
   (check-equal?
      (impose-calling-conventions
         '(module
            (define L.start.1 (lambda (x.5) (call L.start.1)))
            (define L.start.2 (lambda (x.1) (begin (set! x.1 9) 100)))
            (define L.start.3 (lambda (x.1) (begin 
                                                (set! x.1 9)
                                                (if (if (true) (> 1 2) (not (false))) 
                                                   (set! x.2 3)
                                                   (set! x.4 4))
                                                (call L.start.1))))
            (begin (set! x.1 9) (call x.1 100 x.2))))

      '(module
         (define L.start.1 (begin (set! x.5 rdi) (begin (jump L.start.1 rbp))))
         (define L.start.2 (begin (set! x.1 rdi) (begin (set! x.1 9) 100)))
         (define L.start.3
            (begin
               (set! x.1 rdi)
               (begin
                  (set! x.1 9)
                  (if (if (true) (> 1 2) (not (false))) (set! x.2 3) (set! x.4 4))
                  (begin (jump L.start.1 rbp)))))
         (begin
            (set! x.1 9)
            (begin (set! rsi x.2) (set! rdi 100) (jump x.1 rbp rdi rsi))))))

(test-case "impose 9"
   (check-equal?
      (impose-calling-conventions
         '(module
            (define L.start.1 (lambda (x.5) (call L.start.1)))
            (define L.start.2 (lambda (x.1 x.2 x.3 x.4 x.5 x.6 x.7 x.8 x.9 x.10) (begin (set! x.1 9) 100)))
            (begin (set! x.1 9) (call x.1 100 x.2))))

      '(module
         (define L.start.1 (begin (set! x.5 rdi) (begin (jump L.start.1 rbp))))
         (define L.start.2
            (begin
               (set! x.1 rdi)
               (set! x.2 rsi)
               (set! x.3 rdx)
               (set! x.4 rcx)
               (set! x.5 r8)
               (set! x.6 r9)
               (set! x.7 fv0)
               (set! x.8 fv1)
               (set! x.9 fv2)
               (set! x.10 fv3)
               (begin (set! x.1 9) 100)))
         (begin
            (set! x.1 9)
            (begin (set! rsi x.2) (set! rdi 100) (jump x.1 rbp rdi rsi))))))

(test-case "impose 10"
   (check-equal?
      (impose-calling-conventions
         '(module
            (begin 
               (set! x.2 (* 1 x.2)) 
               (set! a.1 (+ y.1 x.1))
               (if (begin (set! x.1 9) (set! x.2 1) (> x.1 x.2)) 
                  (call L.start.1 x.4 x.5 x.6)
                  (call L.start.1 x.1 x.2 x.3)))))
'(module
  (begin
    (set! x.2 (* 1 x.2))
    (set! a.1 (+ y.1 x.1))
    (if (begin (set! x.1 9) (set! x.2 1) (> x.1 x.2))
      (begin
        (set! rdx x.6)
        (set! rsi x.5)
        (set! rdi x.4)
        (jump L.start.1 rbp rdi rsi rdx))
      (begin
        (set! rdx x.3)
        (set! rsi x.2)
        (set! rdi x.1)
        (jump L.start.1 rbp rdi rsi rdx)))))))

(test-case "impose 11"
   (check-equal?
(impose-calling-conventions
'(module(call x.1))
)
'(module (begin (jump x.1 rbp)))
    )
)