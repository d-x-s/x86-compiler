#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt"
)

(test-case "uniquify 1"
    (check-match
     (uniquify 
     '(module (+ 2 2)))

     `(module (+ 2 2))
    )
)

(test-case "uniquify 2"
    (check-match
      (uniquify 
          '(module (define x (lambda (y z) 10))
                    10
            )
      )

     `(module (define ,L.x.1 (lambda (,y.2 ,z.1) 10)) 10)
    )
)

(test-case "uniquify 3"
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

(test-case "uniquify 4"
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

(test-case "uniquify 5"
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

(test-case "uniquify 6"
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

(test-case "uniquify 7"
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

(test-case "uniquify 8"
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

(test-case "uniquify 9"
    (check-match
     (uniquify '(module 
                    (define id (lambda (x) x)) 
                    (let ((y id)) (call y 5))))

     `(module 
        (define ,L.id.1 (lambda (,x.1) ,x.1)) 
        (let ((,y.2 ,L.id.1)) (call ,y.2 5)))
    )
)

(test-case "uniquify 10"
    (check-match
     (uniquify '(module 
                    (let ((foo (let ((bar 1)) bar)) (bar 2)) (+ foo bar))))

     `(module (let ((,foo.2 (let ((,bar.3 1)) ,bar.3)) (,bar.1 2)) (+ ,foo.2 ,bar.1)))
    )
)

(test-case "uniquify 11"
    (check-match
        (uniquify '(module (let ((x 1)) (let ((y (let ((z 3)) z))) (let ((z (let ((y 2)) (+ y y)))) (if (let ((x 6)) (> x 7)) 9 10))))))

     `(module
        (let ((,x.1 1))
            (let ((,y.2 (let ((,z.3 3)) ,z.3)))
            (let ((,z.4 (let ((,y.5 2)) (+ ,y.5 ,y.5))))
                (if (let ((,x.6 6)) (> ,x.6 7)) 9 10)))))
    )
)



