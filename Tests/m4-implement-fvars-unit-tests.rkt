#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt"
)

(test-case "fvars 1"
   (check-equal?
   (implement-fvars
      '(begin (set! fv1 0)))
      '(begin (set! (rbp - 8) 0))
    )
 )

 (test-case "fvars 2"
   (check-equal?
   (implement-fvars
      '(begin (set! fv1 rax)))
      '(begin (set! (rbp - 8) rax))
    )
 )

  (test-case "fvars 3"
   (check-equal?
   (implement-fvars
      '(begin (set! rax fv1)))
      '(begin (set! rax (rbp - 8)))
    )
 )

(test-case "fvars 4"
   (check-equal?
   (implement-fvars
      '(begin (set! rax rcx)))
      '(begin (set! rax rcx))
    )
 )

(test-case "fvars 5"
   (check-equal?
   (implement-fvars
      '(begin (set! rax 0)))
      '(begin (set! rax 0))
    )
 )

 (test-case "fvars 6"
   (check-equal?
   (implement-fvars
      '(begin (set! rax (+ rax fv1))))
      '(begin (set! rax (+ rax (rbp - 8))))
    )
 )

  (test-case "fvars 7"
   (check-equal?
   (implement-fvars
      '(begin (set! rax (+ rax rcx))))
      '(begin (set! rax (+ rax rcx)))
    )
 )

(test-case "fvars 8"
   (check-equal?
   (implement-fvars
      '(begin (set! rax (+ rax 0))))
      '(begin (set! rax (+ rax 0)))
    )
 )

 (test-case "fvars 9"
   (check-equal?
   (implement-fvars
      '(begin (set! rax (+ rax 0))(set! rax (+ rax fv1))))
      '(begin (set! rax (+ rax 0))(set! rax (+ rax (rbp - 8))))
    )
 )

 (test-case "fvars 10"
   (check-equal?
   (implement-fvars
      '(begin (jump rax)))
      '(begin (jump rax))
    )
 )


 (test-case "fvars 11"
   (check-equal?
   (implement-fvars
      '(begin (with-label L.start.1 (set! fv1 0))))
      '(begin (with-label L.start.1 (set! (rbp - 8) 0)))
    )
 )


 (test-case "fvars 12"
   (check-equal?
   (implement-fvars
      '(begin (with-label L.start.1 (jump rax))))
      '(begin (with-label L.start.1 (jump rax)))
    )
 )

  (test-case "fvars 13"
   (check-equal?
   (implement-fvars
      '(begin (with-label L.start.1 (jump rax))))
      '(begin (with-label L.start.1 (jump rax)))
    )
 )

   (test-case "fvars 14"
   (check-equal?
   (implement-fvars
      '(begin (with-label L.start.1 (set! rax (+ rax fv1)))))
      '(begin (with-label L.start.1 (set! rax (+ rax (rbp - 8)))))
    )
 )