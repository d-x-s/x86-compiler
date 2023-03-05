#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../../compiler.rkt"
)


(test-case "uncover 1"
   (check-equal?
   (uncover-locals
   '(module ()
     (begin
       (set! x.1 0)
       (halt x.1))))
    
    '(module ((locals (x.1))) (begin (set! x.1 0) (halt x.1)))
    )
 )

 (test-case "uncover 2"
   (check-equal?
   (uncover-locals
   '(module ()
     (begin
       (set! x.1 0)
       (set! y.1 x.1)
       (set! y.1 (+ y.1 x.1))
       (halt y.1))))
    
    '(module
        ((locals (x.1 y.1)))
        (begin (set! x.1 0) (set! y.1 x.1) (set! y.1 (+ y.1 x.1)) (halt y.1)))
    )
 )

 (test-case "uncover 3"
   (check-equal?
    (uncover-locals
   '(module ()
     (begin
       (set! x.1 0)
       (if (true) (set! x.2 0) (set! x.1 10))
       (halt x.1))))
    
    '(module
  ((locals (x.2 x.1)))
  (begin (set! x.1 0) (if (true) (set! x.2 0) (set! x.1 10)) (halt x.1)))
    )
 )

(test-case "uncover 4"
   (check-equal?
     (uncover-locals
   '(module ()
     (begin
       (set! x.1 0)
       (if (if (true) (false) (true)) (set! x.2 0) (set! x.1 10))
       (halt x.1))))
    
    '(module
  ((locals (x.2 x.1)))
  (begin
    (set! x.1 0)
    (if (if (true) (false) (true)) (set! x.2 0) (set! x.1 10))
    (halt x.1)))
    )
 )

(test-case "uncover 5"
   (check-equal?
      (uncover-locals
   '(module ()
     (begin
       (set! x.1 0)
       (if (if (> x.5 0) (begin (set! x.3 9) (= x.7 30)) (not (> x.10 9))) (set! x.2 0) (set! x.1 10))
       (halt x.1))))
    
    '(module
  ((locals (x.5 x.3 x.7 x.10 x.2 x.1)))
  (begin
    (set! x.1 0)
    (if (if (> x.5 0) (begin (set! x.3 9) (= x.7 30)) (not (> x.10 9)))
      (set! x.2 0)
      (set! x.1 10))
    (halt x.1)))
    )
 )

 (test-case "uncover 6"
   (check-equal?
    (uncover-locals
   '(module ()
       (if (true) (halt x.3) (halt x.3))))
    
    '(module ((locals (x.3))) (if (true) (halt x.3) (halt x.3)))
    )
 )


;doesn't pass because check-equal takes order into account, but the sets are equivalent
(test-case "uncover 7"
   (check-equal?
     (uncover-locals
   '(module ()
       (begin 
(set! x.0 4)
(set! x.0 1)
(set! x.1 (+ x.1 0))
(begin
(set! x.3 3)
(if (> x.3 0) (set! x.3 10) (set! x.11 9))
(begin 
(set! x.0 0)
(if (> x.15 0) (set! x.15 23) (set! x.20 0)))
)
(if (not (> x.11 0)) (halt x.11) (halt 0))
)))
    
    '(module
  ((locals (x.3 x.15 x.20 x.1 x.0 x.11)))
  (begin
    (set! x.0 4)
    (set! x.0 1)
    (set! x.1 (+ x.1 0))
    (begin
      (set! x.3 3)
      (if (> x.3 0) (set! x.3 10) (set! x.11 9))
      (begin (set! x.0 0) (if (> x.15 0) (set! x.15 23) (set! x.20 0))))
    (if (not (> x.11 0)) (halt x.11) (halt 0))))
 )
)

 (test-case "uncover 8"
   (check-equal?
    (uncover-locals
   '(module ()
       (if (> x.1 0) (if (if (true) (false) (= x.5 0)) (halt 0) (halt x.1)) (if (begin (set! x.11 0) (not (> x.7 4))) (halt 0) (halt x.11)))
))
    
    '(module
  ((locals (x.7 x.11 x.5 x.1)))
  (if (> x.1 0)
    (if (if (true) (false) (= x.5 0)) (halt 0) (halt x.1))
    (if (begin (set! x.11 0) (not (> x.7 4))) (halt 0) (halt x.11))))
    )
 )