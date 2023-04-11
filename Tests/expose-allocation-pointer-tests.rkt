#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt")

(test-case "implement-expose-allocation 1 - alloc in tail position"
    (check-equal?
        (expose-allocation-pointer
        '(module
            ((new-frames ()))
            (begin
                (set! x.1 (alloc 10))
                (jump L.done.1)
            )
            )
        )
      '(module
            ((new-frames ()))
                (begin 
                    (begin (set! x.1 r12) (set! r12 (+ r12 10))) 
                    (jump L.done.1)))
    )
)

(test-case "implement-expose-allocation 2 - alloc in tail position, with new frames"
    (check-equal?
        (expose-allocation-pointer
        '(module
            ((new-frames ((x.1) (y.2) (z.3))))
            (begin
                (set! x.1 (alloc 10))
                (jump L.done.1)
            )
            )
        )
      '(module
        ((new-frames ((x.1) (y.2) (z.3))))
        (begin (begin (set! x.1 r12) (set! r12 (+ r12 10))) (jump L.done.1)))
    )
)

(test-case "implement-expose-allocation 3 - trivial, nothing to transform"
    (check-equal?
        (expose-allocation-pointer
        '(module
            ((new-frames ((x.1) (y.2) (z.3))))
            (begin
                (set! x.1 10)
                (jump L.done.1)
            )
            )
        )
      '(module
        ((new-frames ((x.1) (y.2) (z.3))))
        (begin (set! x.1 10) (jump L.done.1)))
    )
)

(test-case "implement-expose-allocation 4 - alloc in pred position"
    (check-equal?
        (expose-allocation-pointer
        '(module
            ((new-frames ((x.1) (y.2) (z.3))))
            (if (begin (set! rax (alloc 10)) (true)) 
                (jump L.done.1) 
                (jump L.done.2)
            )
            )
        )
      '(module
        ((new-frames ((x.1) (y.2) (z.3))))
        (if (begin (begin (set! rax r12) (set! r12 (+ r12 10))) (true))
            (jump L.done.1)
            (jump L.done.2)))
    )
)

(test-case "implement-expose-allocation 5 - alloc in return-point"
    (check-equal?
        (expose-allocation-pointer
        '(module
            ((new-frames ((x.1) (y.2) (z.3))))
            (begin
                (return-point L.done.1             
                   (if (begin (set! rax (alloc 10)) (true)) 
                (jump L.done.1) 
                (jump L.done.2)
            ))
                (jump L.done.1)
            )
            )
        )
      '(module
        ((new-frames ((x.1) (y.2) (z.3))))
        (begin
            (return-point
            L.done.1
            (if (begin (begin (set! rax r12) (set! r12 (+ r12 10))) (true))
            (jump L.done.1)
            (jump L.done.2)))
            (jump L.done.1)))
    )
)
