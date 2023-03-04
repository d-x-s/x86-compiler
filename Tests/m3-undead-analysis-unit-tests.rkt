#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt"
)

(test-case "undead-effect 1"
    (check-equal?
     (undead-effect '(set! z.5 (+ z.5 t.6)) '(z.5))
     '(t.6 z.5)
    )
)

(test-case "undead-effect 2"
    (check-equal?
     (undead-effect '(set! p.1 -1) '(z.5 t.6 p.1))
     '(z.5 t.6)
    )
)

(test-case "undead-effect 3"
    (check-equal?
     (undead-effect '(set! t.6 y.4) '(z.5 t.6))
     '(y.4 z.5)
    )
)

(test-case "undead-effect 4"
    (check-equal?
     (undead-effect '(set! t.6 y.4) '(z.5 t.6))
     '(y.4 z.5)
    )
)

(test-case "undead-effect 5"
    (check-equal?
     (undead-effect '(begin
                        (set! v.1 1)
                        (set! w.2 46)
                        (set! x.3 v.1)
                        (set! p.1 7))
                    '(z.5 t.6))
     '(y.4 z.5)
    )
)