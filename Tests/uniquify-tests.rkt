#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt"
)

(test-case "uniquify 1 - trivial value"
    (check-match
     (uniquify 
     '(module 10))

     `(module 10)
    )
)

(test-case "uniquify 2 - trivial call"
    (check-match
     (uniquify 
     '(module (call 1 2)))

     `(module (call 1 2)) 
    )
)

(test-case "uniquify 3 - trivial block"
    (check-match
      (uniquify 
          '(module (define x (lambda (y z) 10))
                    10
            )
      )

     `(module (define ,L.x.1 (lambda (,y.2 ,z.1) 10)) 10)
    )
)

(test-case "uniquify 4 - trivial blocks"
    (check-match
      (uniquify 
          '(module (define x (lambda (y z) 10))
                   (define a (lambda (b c) 10))
                    10
            )
      )

     `(module
        (define ,L.x.1 (lambda (,y.2 ,z.1) 10))
        (define ,L.a.2 (lambda (,b.4 ,c.3) 10))
        10)
    )
)

(test-case "uniquify 5 - trivial identically named blocks"
    (check-match
      (uniquify 
          '(module (define x (lambda (y z) 10))
                   (define x (lambda (b c) 10))
                    10
            )
      )

     `(module
        (define ,L.x.2 (lambda (,y.2 ,z.1) 10))
        (define ,L.x.2 (lambda (,b.4 ,c.3) 10))
        10)
    )
)

(test-case "uniquify 6 - trivial identical blocks"
    (check-match
      (uniquify 
          '(module (define x (lambda (y z) 10))
                   (define x (lambda (y z) 10))
                    10
            )
      )

     `(module
        (define ,L.x.2 (lambda (,y.2 ,z.1) 10))
        (define ,L.x.2 (lambda (,y.4 ,z.3) 10))
        10)
    )
)

(test-case "uniquify 7 - call in tail position"
    (check-match
      (uniquify 
          '(module (define x (lambda (y z) 10))
                    (define x (lambda (y z) 10))
                    (define y (lambda (a b) 10))
                    (define y (lambda (a b) 10))              
                    (define name (lambda (a b) 10))
                    (define name (lambda (a b) 10))
                    (call x 10 20 30 40 50)

                    ; what if this was the tail? 
                    ; (call z 10 20 30 40 50)
                    ; dict-ref: no value for key: 'z in: '((x . L.x.2) (y . L.y.4) (name . L.name.6))

                    ; what if this was the tail? 
                    ; (call x y z)
                    ; dict-ref: no value for key: 'z in: '((x . L.x.2) (y . L.y.4) (name . L.name.6))

                    ; in conclusion:
                    ; the binding dictionary for the outer tail, and the define statement labels are shared,
                    ; while there is another separate binding dictionary for the bodies of the define statements
            )
      )

     `(module
        (define ,L.x.2 (lambda (,y.2 ,z.1) 10))
        (define ,L.x.2 (lambda (,y.4 ,z.3) 10))
        (define ,L.y.4 (lambda (,a.6 ,b.5) 10))
        (define ,L.y.4 (lambda (,a.8 ,b.7) 10))
        (define ,L.name.6 (lambda (,a.10 ,b.9) 10))
        (define ,L.name.6 (lambda (,a.12 ,b.11) 10))
        (call ,L.x.2 10 20 30 40 50))
    )
)

(test-case "uniquify 8 - call in tail position and identical blocks"
    (check-match
      (uniquify 
          '(module (define x (lambda (y z) 10))
                    (define x (lambda (x x) 10))
                    (define y (lambda (x x) 10))
                    (define y (lambda (a b) 10))              
                    (define name (lambda (a b) 10))
                    (define name (lambda (a b) 10))
                    (call x 10 20 30 40 50)
            )
      )

     `(module
        (define ,L.x.9 (lambda (,y.27 ,z.28) 10))
        (define ,L.x.9 (lambda (,x.30 ,x.30) 10))
        (define ,L.y.10 (lambda (,x.32 ,x.32) 10))
        (define ,L.y.10 (lambda (,a.33 ,b.34) 10))
        (define ,L.name.11 (lambda (,a.35 ,b.36) 10))
        (define ,L.name.11 (lambda (,a.37 ,b.38) 10))
        (call L.x.9 10 20 30 40 50))
    )
)

(test-case "uniquify 9 - let with predicate and call"
    (check-match
      (uniquify 
        '(module 
            (define id1 (lambda (x) x)) 
            (define id2 (lambda (x) x)) 
            (let ((y (if (true) id1 id2))) (call y 5))))

     `(module
        (define ,L.id1.1 (lambda (,x.1) ,x.1))
        (define ,L.id2.2 (lambda (,x.2) ,x.2))
        (let ((,y.3 (if (true) ,L.id1.1 ,L.id2.2))) (call ,y.3 5)))
    )
)

(test-case "uniquify 10 - let with call"
    (check-match
     (uniquify '(module 
                    (define id (lambda (x) x)) 
                    (let ((y id)) (call y 5))))

     `(module 
        (define ,L.id.1 (lambda (,x.1) ,x.1)) 
        (let ((,y.2 ,L.id.1)) (call ,y.2 5)))
    )
)

(test-case "uniquify 11 - simple nested lets"
    (check-match
     (uniquify '(module 
                    (let ((foo (let ((bar 1)) bar)) (bar 2)) (call foo bar))))

     `(module (let ((,foo.2 (let ((,bar.3 1)) ,bar.3)) (,bar.1 2)) (call ,foo.2 ,bar.1)))
    )
)

(test-case "uniquify 12 - complex nested lets"
    (check-match
        (uniquify '(module (let ((x 1)) (let ((y (let ((z 3)) z))) (let ((z (let ((y 2)) (call y y)))) (if (let ((x 6)) (call x 7)) 9 10))))))

     `(module
        (let ((,x.1 1))
            (let ((,y.2 (let ((,z.3 3)) ,z.3)))
            (let ((,z.4 (let ((,y.5 2)) (call ,y.5 ,y.5))))
                (if (let ((,x.6 6)) (call ,x.6 7)) 9 10)))))
    )
)

(test-case "uniquify 13 - call in value position"
    (check-match
        (uniquify 
            '(module 
                (define id1 (lambda (x) x)) 
                (define id2 (lambda (x) x))
                (let ((y (call id1 10))) (call y 5))
            )
        )

     `(module
        (define ,L.id1.1 (lambda (,x.1) ,x.1))
        (define ,L.id2.2 (lambda (,x.2) ,x.2))
        (let ((,y.3 (call ,L.id1.1 10))) (call ,y.3 5)))
    )
)

(test-case "uniquify 14 - various calls in value position"
    (check-match
        (uniquify 
                '(module 
                    (define id1 (lambda (x) (call 10 2))) 
                    (define id2 (lambda (x) (call x x)))
                    (define id3 (lambda (x) (call x 5)))
                    (let ((y (call id1 10))) (call y 5))
                )
        )

     `(module
        (define ,L.id1.1 (lambda (,x.1) (call 10 2)))
        (define ,L.id2.2 (lambda (,x.2) (call ,x.2 ,x.2)))
        (define ,L.id3.3 (lambda (,x.3) (call ,x.3 5)))
        (let ((,y.4 (call ,L.id1.1 10))) (call ,y.4 5)))
    )
)

(test-case "uniquify 15 - prim-f test"
    (check-match
      (uniquify 
          '(module (define x (lambda (y z) *))
                   (define a (lambda (b c) +))
                   (define a (lambda (b c) -))
                   (define a (lambda (b c) <))
                   (define a (lambda (b c) <=))
                   (define a (lambda (b c) >))
                   (define a (lambda (b c) >=))
                   (define a (lambda (b c) eq?))
                   (define a (lambda (b c) fixnum?))
                   (define a (lambda (b c) boolean?))
                   (define a (lambda (b c) empty?))
                   (define a (lambda (b c) void?))
                   (define a (lambda (b c) ascii-char?))
                   (define a (lambda (b c) error?))
                   (define a (lambda (b c) not))
                   (define a (lambda (b c) pair?))
                   (define a (lambda (b c) vector?))
                   (define a (lambda (b c) cons))
                   (define a (lambda (b c) car))
                   (define a (lambda (b c) cdr))
                   (define a (lambda (b c) make-vector))
                   (define a (lambda (b c) vector-length))
                   (define a (lambda (b c) vector-set!))
                   (define a (lambda (b c) vector-ref))
                *
            )
      )

     `(module
            (define ,L.x.4 (lambda (,y.2 ,z.1) *))
            (define ,L.a.27 (lambda (,b.4 ,c.3) +))
            (define ,L.a.27 (lambda (,b.6 ,c.5) -))
            (define ,L.a.27 (lambda (,b.8 ,c.7) <))
            (define ,L.a.27 (lambda (,b.10 ,c.9) <=))
            (define ,L.a.27 (lambda (,b.12 ,c.11) >))
            (define ,L.a.27 (lambda (,b.14 ,c.13) >=))
            (define ,L.a.27 (lambda (,b.16 ,c.15) eq?))
            (define ,L.a.27 (lambda (,b.18 ,c.17) fixnum?))
            (define ,L.a.27 (lambda (,b.20 ,c.19) boolean?))
            (define ,L.a.27 (lambda (,b.22 ,c.21) empty?))
            (define ,L.a.27 (lambda (,b.24 ,c.23) void?))
            (define ,L.a.27 (lambda (,b.26 ,c.25) ascii-char?))
            (define ,L.a.27 (lambda (,b.28 ,c.27) error?))
            (define ,L.a.27 (lambda (,b.30 ,c.29) not))
            (define ,L.a.27 (lambda (,b.32 ,c.31) pair?))
            (define ,L.a.27 (lambda (,b.34 ,c.33) vector?))
            (define ,L.a.27 (lambda (,b.36 ,c.35) cons))
            (define ,L.a.27 (lambda (,b.38 ,c.37) car))
            (define ,L.a.27 (lambda (,b.40 ,c.39) cdr))
            (define ,L.a.27 (lambda (,b.42 ,c.41) make-vector))
            (define ,L.a.27 (lambda (,b.44 ,c.43) vector-length))
            (define ,L.a.27 (lambda (,b.46 ,c.45) vector-set!))
            (define ,L.a.27 (lambda (,b.48 ,c.47) vector-ref))
            *)
    )
)

(test-case "uniquify 16 - prim-f and nested lets"
    (check-match
(uniquify '(module (let ((x empty?)) (let ((y (let ((z eq?)) z))) (let ((z (let ((y 2)) (call void? ascii-char?)))) (if (let ((x >)) (call x vector-set!)) 9 cons))))))

     `(module
        (let ((,x.1 empty?))
            (let ((,y.2 (let ((,z.3 eq?)) ,z.3)))
            (let ((,z.4 (let ((,y.5 2)) (call void? ascii-char?))))
                (if (let ((,x.6 >)) (call ,x.6 vector-set!)) 9 cons)))))
    )
)

