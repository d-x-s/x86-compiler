#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt")

(test-case "impose 1 - tail value"
   (check-equal?
(impose-calling-conventions
'(module (begin 
(set! y.4 5)
 (set! x.3 (+ 1 2))
  (set! z.5 2)
   (set! x.3 (+ x.3 y.4))
    (set! x.3 (+ y.4 z.5))
     (set! x.3 x.3)
      x.3)))

    ' (module
  ((new-frames ()))
  (begin
    (set! tmp-ra.1 r15)
    (begin
      (set! y.4 5)
      (set! x.3 (+ 1 2))
      (set! z.5 2)
      (set! x.3 (+ x.3 y.4))
      (set! x.3 (+ y.4 z.5))
      (set! x.3 x.3)
      (begin (set! rax x.3) (jump tmp-ra.1 rbp rax)))))
    )
)

(test-case "impose 2 - transforming procedure"
   (check-equal?
(impose-calling-conventions
'(module (define L.zero.3 (lambda (v0.11 v1.12 v2.13 v3.14) 0)) 0))

    '(module
  ((new-frames ()))
  (define L.zero.3
    ((new-frames ()))
    (begin
      (set! tmp-ra.1 r15)
      (begin
        (set! v0.11 rdi)
        (set! v1.12 rsi)
        (set! v2.13 rdx)
        (set! v3.14 rcx)
        (begin (set! rax 0) (jump tmp-ra.1 rbp rax)))))
      (begin (set! tmp-ra.2 r15) (begin (set! rax 0) (jump tmp-ra.2 rbp rax))))
    )
)

(test-case "impose 3 - tail call + non-tail call"
   (check-equal?
(impose-calling-conventions
'(module (begin 
(set! y.4 5)
 (set! x.3 (+ 1 2))
  (set! z.5 2)
   (set! x.3 (+ x.3 y.4))
    (set! x.3 (+ y.4 z.5))
     (set! x.3 (call x.1))
      (call x.3))))

    '(module
  ((new-frames (())))
  (begin
    (set! tmp-ra.1 r15)
    (begin
      (set! y.4 5)
      (set! x.3 (+ 1 2))
      (set! z.5 2)
      (set! x.3 (+ x.3 y.4))
      (set! x.3 (+ y.4 z.5))
      (begin
        (return-point L.rp.1 (begin (set! r15 L.rp.1) (jump x.1 rbp r15)))
        (set! x.3 rax))
      (begin (set! r15 tmp-ra.1) (jump x.3 rbp r15)))))
    )
)


(test-case "impose 4 - tail value"
   (check-equal?
(impose-calling-conventions
'(module
   100))

    '(module
  ((new-frames ()))
  (begin (set! tmp-ra.1 r15) (begin (set! rax 100) (jump tmp-ra.1 rbp rax))))
)
)

(test-case "impose 5 - predicate tail value"
   (check-equal?
(impose-calling-conventions
'(module
   (if (true) 100  200)))

    '(module
  ((new-frames ()))
  (begin
    (set! tmp-ra.1 r15)
    (if (true)
      (begin (set! rax 100) (jump tmp-ra.1 rbp rax))
      (begin (set! rax 200) (jump tmp-ra.1 rbp rax)))))
))

(test-case "impose 6 - multiple non-tail calls"
   (check-equal?
(impose-calling-conventions
'(module (begin 
(set! y.4 5)
 (set! x.3 (+ 1 2))
  (set! z.5 2)
   (set! x.3 (+ x.3 y.4))
    (set! x.3 (+ y.4 z.5))
     (set! x.3 (call x.1))
      (set! x.2 (call x.2))
      (call x.3))))

    '(module
  ((new-frames (() ())))
  (begin
    (set! tmp-ra.1 r15)
    (begin
      (set! y.4 5)
      (set! x.3 (+ 1 2))
      (set! z.5 2)
      (set! x.3 (+ x.3 y.4))
      (set! x.3 (+ y.4 z.5))
      (begin
        (return-point L.rp.1 (begin (set! r15 L.rp.1) (jump x.1 rbp r15)))
        (set! x.3 rax))
      (begin
        (return-point L.rp.2 (begin (set! r15 L.rp.2) (jump x.2 rbp r15)))
        (set! x.2 rax))
      (begin (set! r15 tmp-ra.1) (jump x.3 rbp r15)))))
))

(test-case "impose 7 - predicate traversal"
   (check-equal?
(impose-calling-conventions
'(module (begin (if (true) (begin (begin (set! z.4 (+ 4 5)) (set! y.4 z.4)) (set! x.3 y.4)) (set! x.3 y.4)) x.3)))

    '(module
  ((new-frames ()))
  (begin
    (set! tmp-ra.1 r15)
    (begin
      (if (true)
        (begin (begin (set! z.4 (+ 4 5)) (set! y.4 z.4)) (set! x.3 y.4))
        (set! x.3 y.4))
      (begin (set! rax x.3) (jump tmp-ra.1 rbp rax)))))
))

(test-case "impose 8 - non-tail call in predicate"
   (check-equal?
(impose-calling-conventions
 '(module 
 (begin 
 (if (begin (set! x.1 (call x.3)) (true)) 
 (begin (begin (set! z.4 (+ 4 5)) (set! y.4 z.4)) (set! x.3 y.4)) 
 (set! x.3 y.4))
  x.3)))

    '(module
  ((new-frames (())))
  (begin
    (set! tmp-ra.1 r15)
    (begin
      (if (begin
            (begin
              (return-point
               L.rp.1
               (begin (set! r15 L.rp.1) (jump x.3 rbp r15)))
              (set! x.1 rax))
            (true))
        (begin (begin (set! z.4 (+ 4 5)) (set! y.4 z.4)) (set! x.3 y.4))
        (set! x.3 y.4))
      (begin (set! rax x.3) (jump tmp-ra.1 rbp rax)))))
))
