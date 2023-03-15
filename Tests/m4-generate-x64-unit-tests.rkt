#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt"
)

(test-case "generate-x64 1"
    (check-equal?
      (generate-x64 
        '(begin 
         (set! (rbp - 0) 0) 
         (set! (rbp - 8) 42)))
     
         "mov QWORD [rbp - 0], 0\nmov QWORD [rbp - 8], 42\n"
    )
)

(test-case "generate-x64 2"
    (check-equal?
      (generate-x64
        '(begin
         (set! (rbp - 0) rax)
         (set! (rbp - 8) L.start.1)))
     
         "mov QWORD [rbp - 0], rax\nlea QWORD [rbp - 8], [rel L.start.1]\n"
    )
)

(test-case "generate-x64 3"
    (check-equal?
        (generate-x64
          '(begin
           (set! rax rbx)
           (set! rax (rbp - 8))))
     
         "mov rax, rbx\nmov rax, QWORD [rbp - 8]\n"
    )
)

(test-case "generate-x64 4"
    (check-equal?
        (generate-x64
          '(begin
           (set! rax rbx)
           (set! rax L.start.1)
           (set! rbx 1024)))
     
         "mov rax, rbx\nlea rax, [rel L.start.1]\nmov rbx, 1024\n"
    )
)

(test-case "generate-x64 5"
    (check-equal?
        (generate-x64
          '(begin
           (set! rax (+ rax 1024))
           (set! rbx (* rbx 2048))))
     
         "add rax, 1024\nimul rbx, 2048\n"
    )
)

(test-case "generate-x64 6"
    (check-equal?
        (generate-x64
          '(begin
           (set! rax (+ rax rbx))
           (set! rcx (rbp - 8))))

         "add rax, rbx\nmov rcx, QWORD [rbp - 8]\n"
    )
)

(test-case "generate-x64 7"
    (check-equal?
        (generate-x64
          '(begin
           (set! rax (+ rax rbx))
           (set! rcx (rbp - 8))))

         "add rax, rbx\nmov rcx, QWORD [rbp - 8]\n"
    )
)

(test-case "generate-x64 8"
    (check-equal?
        (generate-x64
          '(begin
           (with-label L.tmp.1 (set! rcx (rbp - 8)))
           (with-label L.tmp.2 (set! rax L.start.1))))

         "L.tmp.1:\nmov rcx, QWORD [rbp - 8]\nL.tmp.2:\nlea rax, [rel L.start.1]\n"
    )
)

(test-case "generate-x64 9"
    (check-equal?
        (generate-x64
          '(begin
           (jump rax)
           (jump L.start.1)))

         "jmp rax\njmp L.start.1\n"
    )
)

(test-case "generate-x64 10"
    (check-equal?
        (generate-x64
          '(begin
           (compare rax 1024)
           (compare rax rbx)))

         "cmp rax, 1024\ncmp rax, rbx\n"
    )
)

(test-case "generate-x64 11"
    (check-equal?
        (generate-x64
           '(begin 
            (jump-if <  L.x.1)
            (jump-if <= L.x.2)
            (jump-if =  L.x.3)
            (jump-if >  L.x.4)
            (jump-if >= L.x.5)
            (jump-if != L.x.6)))

         "jl L.x.1\njle L.x.2\nje L.x.3\njg L.x.4\njge L.x.5\njne L.x.6\n"
    )
)

(test-case "generate-x64 12"
    (check-equal?
        (generate-x64
           '(begin 
            (set! (rbp - 16) -1) 
            (set! r10 0) 
            (set! rax 10) 
            (with-label L.x.1 (set! rax (+ rax (rbp - 16)))) 
            (compare rax r10) 
            (jump-if > L.x.1)))

         "mov QWORD [rbp - 16], -1\nmov r10, 0\nmov rax, 10\nL.x.1:\nadd rax, QWORD [rbp - 16]\ncmp rax, r10\njg L.x.1\n"
    )
)