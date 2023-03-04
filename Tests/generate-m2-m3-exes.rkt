#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt"
)

(define (system/exit-code! str [success? zero?])
  (let ([code (system/exit-code str)])
    (unless (success? code)
      (error (format "Command '~a' failed with exit code '~a'" str code)))
    code))

  (define (compile str)
    (define p (path->string (make-temporary-file "~a.s")))
    (define o (string-replace p ".s" ".o"))
    (define exe (string-replace p ".s" ".exe"))
    (with-output-to-file p (thunk (display str)) #:exists 'replace)
    (system/exit-code! (format "nasm -f elf64 ~a -o ~a" p o))
    (system/exit-code! (format "ld -e start -o ~a ~a" exe o))
  exe)

  (define (execute str)
  (system/exit-code! (compile str) number?))

  (println "M2:")
  (compile (compile-m2
   '(module
      (let ([x (let ([y 1]
                     [x 2])
                 (+ y x))])
        (let ([y (let ([x 3]) x)])
          (+ x y))))))

  (println "M3:")
  (compile (compile-m3
   '(module
      (let ([x (let ([y 1]
                     [x 2])
                 (+ y x))])
        (let ([y (let ([x 3]) x)])
          (+ x y))))))

