#lang racket

(require
 cpsc411/compiler-lib
 rackunit "compiler.rkt"
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
     '(module (let ([x (+ 2 2)]) x)))

     `(module (let ((,x.1 (+ 2 2))) ,x.1))
    )
)

(test-case "uniquify 3"
    (check-match
     (uniquify 
     '(module
        (let ([x 2])
          (let ([x 2])
            (+ x x)))))

     `(module 
        (let ((,x.2 2)) 
          (let ((,x.3 2)) 
            (+ ,x.3 ,x.3))))
    )
)

(test-case "uniquify 4"
    (check-match
     (uniquify 
     '(module
        (let ([x 2])
          (let ([y 2])
           (+ x y)))))

     `(module 
        (let ((,x.4 2)) 
          (let ((,y.5 2)) 
            (+ ,x.4 ,y.5))))
    )
)

(test-case "uniquify 5"
    (check-match
     (uniquify 
     '(module
        (if (true) 
            10
            20)))

     `(module 
        (if (true) 
            10 
            20))
    )
)

(test-case "uniquify 6"
    (check-match
    (uniquify 
     '(module
        (let ([x 2]
              [y 3]) 
             (if (true) x y))))

     `(module 
        (let ((,x.2 2) 
              (,y.1 3)) 
             (if (true) 
                  ,x.2 
                  ,y.1)))
    )
)

(test-case "uniquify 7"
    (check-match
     (uniquify 
     '(module
        (let ([x 2]
              [y 3]) 
             (if (true) 
                 (let ([x 4]) x) 
                  y))))

     `(module (let ((,x.2 2) (,y.1 3)) (if (true) (let ((,x.3 4)) ,x.3) ,y.1)))
    )
)

(test-case "uniquify 8"
    (check-match
     (uniquify 
     '(module
        (let ([x 2]
              [y 3]) 
             (if (true) 
                 (let ([x 4]) x) 
                 (let ([y 5]) y)))))

     `(module (let ((,x.2 2) (,y.1 3)) (if (true) (let ((,x.3 4)) ,x.3) (let ((,y.4 5)) ,y.4))))
    )
)

(test-case "uniquify 9"
    (check-match
     (uniquify 
     '(module
        (if (true) 
            (let ([x 4]) x) 
            (let ([y 5]) y))))

     `(module (if (true) (let ((,x.1 4)) ,x.1) (let ((,y.2 5)) ,y.2)))
    )
)

(test-case "uniquify 10"
    (check-match
     (uniquify 
     '(module
        (if (let ([x 3]) (> x 1))  
            (let ([x 4]) x) 
            (let ([y 5]) y))))

     `(module (if (let ((,x.1 3)) (> ,x.1 1)) (let ((,x.2 4)) ,x.2) (let ((,y.3 5)) ,y.3)))
    )
)