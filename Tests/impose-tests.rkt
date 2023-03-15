#lang racket

(require
 cpsc411/compiler-lib
 rackunit "compiler.rkt"
)


(test-case "impose 1"
   (check-equal?
(impose-calling-conventions
'(module
   (begin (set! x.1 9) 100))
)

    '(module (begin (set! x.1 9) 100))
    )
)

(test-case "impose 2"
   (check-equal?
(impose-calling-conventions
'(module
   (begin (set! x.1 9) (call x.1)))
)

    '(module (begin (set! x.1 9) (begin (jump x.1 rbp))))
    )
)

(test-case "impose 3"
   (check-equal?
(impose-calling-conventions
'(module
   (begin (set! x.1 9) (call x.1 100 x.2)))
)


    '(module
  (begin
    (set! x.1 9)
    (begin (set! rsi x.2) (set! rdi 100) (jump x.1 rbp rdi rsi))))
    )
)

(test-case "impose 4"
   (check-equal?
(impose-calling-conventions
'(module
   (begin (set! x.1 9) (call x.1 100 x.2)))
)


    '(module
  (begin
    (set! x.1 9)
    (begin (set! rsi x.2) (set! rdi 100) (jump x.1 rbp rdi rsi))))
    )
)

(test-case "impose 5"
   (check-equal?
(impose-calling-conventions
'(module
   (define L.start.1 (lambda (x.5) (call x.5)))
   (begin (set! x.1 9) (call x.1 100 x.2)))
)

    '(module
  (define L.start.1 (begin (set! x.5 rdi) (begin (jump x.5 rbp))))
  (begin
    (set! x.1 9)
    (begin (set! rsi x.2) (set! rdi 100) (jump x.1 rbp rdi rsi))))
    )
)