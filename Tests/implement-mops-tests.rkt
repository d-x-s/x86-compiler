#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt")

(test-case "implement-mops 1 - mref operation"
  (check-equal?
    (implement-mops
    '(begin
        (set! rax 8)
        (set! rax (mref rbx 8))
        )
    )
    
    '(begin (set! rax 8) (set! rax (rbx + 8)))
  )
)

(test-case "implement-mops 2 - mset operation with int32"
  (check-equal?
    (implement-mops
    '(begin
        (set! rax 8)
        (mset! rax 10 12)
        )
    )
    
    '(begin (set! rax 8) (set! (rax + 10) 12))
  )
)

(test-case "implement-mops 3 - mset operation with trg"
  (check-equal?
    (implement-mops
    '(begin
        (set! rax 8)
        (mset! rax 10 L.done.1)
        )
    )
    
    '(begin (set! rax 8) (set! (rax + 10) L.done.1))
  )
)

(test-case "implement-mops 4 - label case"
  (check-equal?
    (implement-mops
    '(begin
        (set! rax 8)
        (with-label L.done.1 (mset! rax 10 L.done.1))
        )
    )
    
    '(begin (set! rax 8) (with-label L.done.1 (set! (rax + 10) L.done.1)))
  )
)

(test-case "implement-mops 5 - int32 boundary for mset"
  (check-equal?
    (implement-mops
    '(begin
        (set! rax 8)
        (with-label L.done.1 (mset! rax 2147483647 L.done.1))
        )
    )
    
    '(begin (set! rax 8) (with-label L.done.1 (set! (rax + 2147483647) L.done.1)))
  )
)