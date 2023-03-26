#lang racket

(require
 cpsc411/compiler-lib
 rackunit "../compiler.rkt")

; M6 Tests

(test-case "allocate 1 - simple return point, empty call-undead and assignment"
    (check-match
        (allocate-frames
            `(module
                ((new-frames (()))
                (locals (x.3 y.1))
                (call-undead ())
                (undead-out ((y.1) ((rax) ()) ()))
                (conflicts ((x.3 ()) (y.1 (rax)) (rax (y.1))))
                (assignment ()))
                (begin 
                    (set! rax (+ rax x.3)) 
                    (return-point L.one.1 (jump y.1)) 
                    (jump rax))))

     `(module
        ((locals (x.3 y.1))
        (conflicts ((x.3 ()) (y.1 (rax)) (rax (y.1))))
        (assignment ()))
        (begin
            (set! rax (+ rax x.3))
            (begin
                (set! rbp (- rbp 0))
                (return-point L.one.1 (jump y.1))
                (set! rbp (+ rbp 0)))
            (jump rax)))))

(test-case "allocate 2 - simple return points, frame size from call-undead"
    (check-match
        (allocate-frames
            `(module
                ((new-frames (()))
                (locals (x.3 y.1 y.2))
                (call-undead (y.4 y.3 y.5 y.6))
                (undead-out
                    ((y.1 y.4 y.3 y.5 y.6)
                    ((y.4 y.3 y.5 y.6) ())
                    ((y.5 y.6) ((y.4) ()))
                    ((rax) ((y.6 y.5) () ()))
                    ()))
                (conflicts
                    ((x.3 ())
                    (y.1 (rax))
                    (y.2 (y.4))
                    (y.3 (rax))
                    (y.4 (y.2 rax))
                    (y.6 (rax))
                    (y.5 (rax))
                    (rax (y.6 y.5 y.3 y.4 y.1))))
                (assignment ((y.6 fv0) (y.5 fv0) (y.3 fv0) (y.4 fv0))))
                (begin
                    (set! rax (+ rax x.3))
                    (return-point L.one.1 (jump y.1))
                    (return-point L.two.2 (begin (set! y.2 y.3) (jump y.4)))
                    (return-point L.three.3 (if (false) (jump y.5) (jump y.6)))
                    (jump rax))))

     `(module
        ((locals (x.3 y.1 y.2))
        (conflicts
            ((x.3 ())
            (y.1 (rax))
            (y.2 (y.4))
            (y.3 (rax))
            (y.4 (y.2 rax))
            (y.6 (rax))
            (y.5 (rax))
            (rax (y.6 y.5 y.3 y.4 y.1))))
        (assignment ((y.6 fv0) (y.5 fv0) (y.3 fv0) (y.4 fv0))))
        (begin
            (set! rax (+ rax x.3))
            (begin
            (set! rbp (- rbp 32))
            (return-point L.one.1 (jump y.1))
            (set! rbp (+ rbp 32)))
            (begin
            (set! rbp (- rbp 32))
            (return-point L.two.2 (begin (set! y.2 y.3) (jump y.4)))
            (set! rbp (+ rbp 32)))
            (begin
            (set! rbp (- rbp 32))
            (return-point L.three.3 (if (false) (jump y.5) (jump y.6)))
            (set! rbp (+ rbp 32)))
            (jump rax)))))

(test-case "allocate 3 - return point in pred begin, effect begin"
    (check-match
        (allocate-frames
            `(module
                ((new-frames (()))
                (locals (x.1 x.11))
                (call-undead (x.7 x.4 x.2 x.12 x.100))
                (undead-out
                    ((x.7 x.4 x.2 x.12 x.100)
                    (x.1 x.7 x.4 x.2 x.12 x.100)
                    (((x.1 x.7 x.4 x.2 x.12 x.100)
                    ((x.7 x.4 x.2 x.12 x.100) ())
                    (x.7 x.4 x.2 x.12 x.100))
                    (x.2 x.12 x.100)
                    (x.2 x.12 x.100))
                    ((x.2 x.12 x.100) ((x.12 x.100 rax) ()) (x.100 rax x.11) (x.100 rax x.11))
                    (x.100 rax x.11)))
                (conflicts
                    ((x.4 (rax))
                    (x.1 (rax x.100 x.12 x.2 x.7))
                    (x.7 (x.100 x.12 x.2 x.1 rax))
                    (x.11 (rax x.100 r10))
                    (x.12 (rcx x.7 x.1 rax))
                    (x.2 (rcx x.7 x.1 rax))
                    (rax (x.11 r10 x.1 x.100 x.12 x.2 x.4 x.7))
                    (x.100 (x.11 rcx r10 x.7 x.1 rax))
                    (r10 (x.11 rax x.100))
                    (rcx (x.100 x.12 x.2))))
                (assignment ((x.100 fv0) (x.12 fv0) (x.2 fv0) (x.4 fv0) (x.7 fv1))))
                (begin
                    (set! rax L.start.1)
                    (set! x.1 x.4)
                    (if (not (begin (set! rax 1) (return-point L.return.1 (jump x.1)) (true)))
                    (set! rax 0)
                    (set! x.7 (+ x.7 x.4)))
                    (begin
                    (set! rcx 0)
                    (return-point L.return.2 (jump x.2))
                    (set! x.11 x.12)
                    (set! r10 0))
                    (jump L.start.14 x.11 rax x.100))))

     `(module
        ((locals (x.1 x.11))
        (conflicts
            ((x.4 (rax))
            (x.1 (rax x.100 x.12 x.2 x.7))
            (x.7 (x.100 x.12 x.2 x.1 rax))
            (x.11 (rax x.100 r10))
            (x.12 (rcx x.7 x.1 rax))
            (x.2 (rcx x.7 x.1 rax))
            (rax (x.11 r10 x.1 x.100 x.12 x.2 x.4 x.7))
            (x.100 (x.11 rcx r10 x.7 x.1 rax))
            (r10 (x.11 rax x.100))
            (rcx (x.100 x.12 x.2))))
        (assignment ((x.100 fv0) (x.12 fv0) (x.2 fv0) (x.4 fv0) (x.7 fv1))))
        (begin
            (set! rax L.start.1)
            (set! x.1 x.4)
            (if (not
                (begin
                (set! rax 1)
                (begin
                    (set! rbp (- rbp 40))
                    (return-point L.return.1 (jump x.1))
                    (set! rbp (+ rbp 40)))
                (true)))
            (set! rax 0)
            (set! x.7 (+ x.7 x.4)))
            (begin
            (set! rcx 0)
            (begin
                (set! rbp (- rbp 40))
                (return-point L.return.2 (jump x.2))
                (set! rbp (+ rbp 40)))
            (set! x.11 x.12)
            (set! r10 0))
            (jump L.start.14 x.11 rax x.100)))))

(test-case "allocate 4 - return point as if branch"
    (check-match
        (allocate-frames
            `(module
                ((new-frames (()))
                (locals (x.4 x.1 x.2))
                (call-undead (x.100 x.11))
                (undead-out
                    ((x.4 x.2 x.100 x.11)
                    (x.2 x.1 x.100 x.11)
                    ((x.2 x.1 x.100 x.11) ((x.100 rax x.11) ()) ((x.100 rax x.11) ()))
                    (x.100 rax x.11)))
                (conflicts
                    ((x.4 (rax))
                    (x.1 (x.11 x.100 x.2))
                    (x.2 (x.1 rax))
                    (rax (x.11 x.100 x.2 x.4))
                    (x.100 (x.1 rax))
                    (x.11 (x.1 rax))))
                (assignment ((x.11 fv0) (x.100 fv0))))
                (begin
                    (set! rax L.start.1)
                    (set! x.1 x.4)
                    (if (true)
                    (return-point L.return.1 (jump x.1))
                    (return-point L.return.2 (jump x.2)))
                    (jump L.start.14 x.11 rax x.100))))

     `(module
        ((locals (x.4 x.1 x.2))
        (conflicts
            ((x.4 (rax))
            (x.1 (x.11 x.100 x.2))
            (x.2 (x.1 rax))
            (rax (x.11 x.100 x.2 x.4))
            (x.100 (x.1 rax))
            (x.11 (x.1 rax))))
        (assignment ((x.11 fv0) (x.100 fv0))))
        (begin
            (set! rax L.start.1)
            (set! x.1 x.4)
            (if (true)
            (begin
                (set! rbp (- rbp 16))
                (return-point L.return.1 (jump x.1))
                (set! rbp (+ rbp 16)))
            (begin
                (set! rbp (- rbp 16))
                (return-point L.return.2 (jump x.2))
                (set! rbp (+ rbp 16))))
            (jump L.start.14 x.11 rax x.100)))))

(test-case "allocate 5 - largest assignment = call-undead length"
    (check-match
        (allocate-frames
            `(module
                ((new-frames (()))
                (locals (x.4 x.1 x.2))
                (call-undead (x.100))
                (undead-out
                    ((x.4 x.2 x.100 x.11)
                    (x.2 x.1 x.100 x.11)
                    ((x.2 x.1 x.100 x.11) ((x.100 rax x.11) ()) ((x.100 rax x.11) ()))
                    (x.100 rax x.11)))
                (conflicts
                    ((x.4 (rax))
                    (x.1 (x.11 x.100 x.2))
                    (x.2 (x.1 rax))
                    (rax (x.11 x.100 x.2 x.4))
                    (x.100 (x.1 rax))
                    (x.11 (x.1 rax))))
                (assignment ((x.11 fv0) (x.100 fv1))))
                (begin
                    (set! rax L.start.1)
                    (set! x.1 x.4)
                    (if (true)
                    (return-point L.return.1 (jump x.1))
                    (return-point L.return.2 (jump x.2)))
                    (jump L.start.14 x.11 rax x.100))))

     `(module
        ((locals (x.4 x.1 x.2))
        (conflicts
            ((x.4 (rax))
            (x.1 (x.11 x.100 x.2))
            (x.2 (x.1 rax))
            (rax (x.11 x.100 x.2 x.4))
            (x.100 (x.1 rax))
            (x.11 (x.1 rax))))
        (assignment ((x.11 fv0) (x.100 fv1))))
        (begin
            (set! rax L.start.1)
            (set! x.1 x.4)
            (if (true)
            (begin
                (set! rbp (- rbp 16))
                (return-point L.return.1 (jump x.1))
                (set! rbp (+ rbp 16)))
            (begin
                (set! rbp (- rbp 16))
                (return-point L.return.2 (jump x.2))
                (set! rbp (+ rbp 16))))
            (jump L.start.14 x.11 rax x.100)))))

(test-case "allocate 6 - public test 1"
    (check-match
        (allocate-frames
            `(module
                ((new-frames ())
                (locals (tmp-ra.3))
                (call-undead ())
                (undead-out ((tmp-ra.3 rbp) (tmp-ra.3 fv0 rbp) (fv0 r15 rbp) (fv0 r15 rbp)))
                (conflicts
                    ((tmp-ra.3 (fv0 rbp))
                    (rbp (r15 fv0 tmp-ra.3))
                    (fv0 (r15 rbp tmp-ra.3))
                    (r15 (rbp fv0))))
                (assignment ()))
                (define L.id.1
                    ((new-frames ())
                    (locals (tmp-ra.2 x.1))
                    (undead-out
                    ((fv0 tmp-ra.2 rbp) (x.1 tmp-ra.2 rbp) (tmp-ra.2 rax rbp) (rax rbp)))
                    (call-undead ())
                    (conflicts
                    ((tmp-ra.2 (rax x.1 rbp fv0))
                    (x.1 (rbp tmp-ra.2))
                    (fv0 (tmp-ra.2))
                    (rbp (rax x.1 tmp-ra.2))
                    (rax (rbp tmp-ra.2))))
                    (assignment ()))
                    (begin
                    (set! tmp-ra.2 r15)
                    (set! x.1 fv0)
                    (set! rax x.1)
                    (jump tmp-ra.2 rbp rax)))
                (begin
                    (set! tmp-ra.3 r15)
                    (set! fv0 5)
                    (set! r15 tmp-ra.3)
                    (jump L.id.1 rbp r15 fv0))))

     `(module
        ((locals (tmp-ra.3))
        (conflicts
            ((tmp-ra.3 (fv0 rbp))
            (rbp (r15 fv0 tmp-ra.3))
            (fv0 (r15 rbp tmp-ra.3))
            (r15 (rbp fv0))))
        (assignment ()))
        (define L.id.1
            ((locals (tmp-ra.2 x.1))
            (conflicts
            ((tmp-ra.2 (rax x.1 rbp fv0))
            (x.1 (rbp tmp-ra.2))
            (fv0 (tmp-ra.2))
            (rbp (rax x.1 tmp-ra.2))
            (rax (rbp tmp-ra.2))))
            (assignment ()))
            (begin
            (set! tmp-ra.2 r15)
            (set! x.1 fv0)
            (set! rax x.1)
            (jump tmp-ra.2 rbp rax)))
        (begin
            (set! tmp-ra.3 r15)
            (set! fv0 5)
            (set! r15 tmp-ra.3)
            (jump L.id.1 rbp r15 fv0)))))

(test-case "allocate 7 - public test 2"
    (check-match
        (allocate-frames
            `(module
                ((new-frames ((nfv.5)))
                (locals (nfv.5 y.2))
                (call-undead (tmp-ra.4))
                (undead-out
                    ((tmp-ra.4 rbp)
                    ((rax tmp-ra.4 rbp) ((nfv.5 rbp) (nfv.5 r15 rbp) (nfv.5 r15 rbp)))
                    (y.2 tmp-ra.4 rbp)
                    (y.2 rax tmp-ra.4 rbp)
                    (tmp-ra.4 rax rbp)
                    (rax rbp)))
                (conflicts
                    ((tmp-ra.4 (rax y.2 rbp))
                    (nfv.5 (r15 rbp))
                    (y.2 (rax rbp tmp-ra.4))
                    (rbp (rax y.2 r15 nfv.5 tmp-ra.4))
                    (r15 (rbp nfv.5))
                    (rax (rbp tmp-ra.4 y.2))))
                (assignment ((tmp-ra.4 fv0))))
                (define L.id.1
                    ((new-frames ())
                    (locals (tmp-ra.3 x.1))
                    (undead-out
                    ((fv0 tmp-ra.3 rbp) (x.1 tmp-ra.3 rbp) (tmp-ra.3 rax rbp) (rax rbp)))
                    (call-undead ())
                    (conflicts
                    ((tmp-ra.3 (rax x.1 rbp fv0))
                    (x.1 (rbp tmp-ra.3))
                    (fv0 (tmp-ra.3))
                    (rbp (rax x.1 tmp-ra.3))
                    (rax (rbp tmp-ra.3))))
                    (assignment ()))
                    (begin
                    (set! tmp-ra.3 r15)
                    (set! x.1 fv0)
                    (set! rax x.1)
                    (jump tmp-ra.3 rbp rax)))
                (begin
                    (set! tmp-ra.4 r15)
                    (return-point L.rp.2
                    (begin (set! nfv.5 5) (set! r15 L.rp.2) (jump L.id.1 rbp r15 nfv.5)))
                    (set! y.2 rax)
                    (set! rax 5)
                    (set! rax (+ rax y.2))
                    (jump tmp-ra.4 rbp rax))))

     `(module
        ((locals (y.2))
        (conflicts
            ((tmp-ra.4 (rax y.2 rbp))
            (nfv.5 (r15 rbp))
            (y.2 (rax rbp tmp-ra.4))
            (rbp (rax y.2 r15 nfv.5 tmp-ra.4))
            (r15 (rbp nfv.5))
            (rax (rbp tmp-ra.4 y.2))))
        (assignment ((nfv.5 fv1) (tmp-ra.4 fv0))))
        (define L.id.1
            ((locals (tmp-ra.3 x.1))
            (conflicts
            ((tmp-ra.3 (rax x.1 rbp fv0))
            (x.1 (rbp tmp-ra.3))
            (fv0 (tmp-ra.3))
            (rbp (rax x.1 tmp-ra.3))
            (rax (rbp tmp-ra.3))))
            (assignment ()))
            (begin
            (set! tmp-ra.3 r15)
            (set! x.1 fv0)
            (set! rax x.1)
            (jump tmp-ra.3 rbp rax)))
        (begin
            (set! tmp-ra.4 r15)
            (begin
            (set! rbp (- rbp 8))
            (return-point
                L.rp.2
                (begin (set! nfv.5 5) (set! r15 L.rp.2) (jump L.id.1 rbp r15 nfv.5)))
            (set! rbp (+ rbp 8)))
            (set! y.2 rax)
            (set! rax 5)
            (set! rax (+ rax y.2))
            (jump tmp-ra.4 rbp rax)))))

(test-case "allocate 8 - public test 3"
    (check-match
        (allocate-frames
            `(module
                ((new-frames ())
                (locals (tmp-ra.7))
                (call-undead ())
                (undead-out ((tmp-ra.7 rbp) (tmp-ra.7 fv0 rbp) (fv0 r15 rbp) (fv0 r15 rbp)))
                (conflicts
                    ((tmp-ra.7 (fv0 rbp))
                    (rbp (r15 fv0 tmp-ra.7))
                    (fv0 (r15 rbp tmp-ra.7))
                    (r15 (rbp fv0))))
                (assignment ()))
                (define L.odd?.1
                    ((new-frames ())
                    (locals (x.1 tmp-ra.5 y.2))
                    (undead-out
                    ((fv0 tmp-ra.5 rbp)
                    (x.1 tmp-ra.5 rbp)
                    ((x.1 tmp-ra.5 rbp)
                        ((tmp-ra.5 rax rbp) (rax rbp))
                        ((y.2 tmp-ra.5 rbp)
                        (y.2 tmp-ra.5 rbp)
                        (tmp-ra.5 fv0 rbp)
                        (fv0 r15 rbp)
                        (fv0 r15 rbp)))))
                    (call-undead ())
                    (conflicts
                    ((x.1 (rbp tmp-ra.5))
                    (tmp-ra.5 (x.1 rbp rax fv0 y.2))
                    (y.2 (rbp tmp-ra.5))
                    (rbp (x.1 tmp-ra.5 rax r15 fv0 y.2))
                    (fv0 (r15 rbp tmp-ra.5))
                    (r15 (rbp fv0))
                    (rax (rbp tmp-ra.5))))
                    (assignment ()))
                    (begin
                    (set! tmp-ra.5 r15)
                    (set! x.1 fv0)
                    (if (= x.1 0)
                        (begin (set! rax 0) (jump tmp-ra.5 rbp rax))
                        (begin
                        (set! y.2 x.1)
                        (set! y.2 (+ y.2 -1))
                        (set! fv0 y.2)
                        (set! r15 tmp-ra.5)
                        (jump L.even?.2 rbp r15 fv0)))))
                (define L.even?.2
                    ((new-frames ())
                    (locals (x.3 tmp-ra.6 y.4))
                    (undead-out
                    ((fv0 tmp-ra.6 rbp)
                    (x.3 tmp-ra.6 rbp)
                    ((x.3 tmp-ra.6 rbp)
                        ((tmp-ra.6 rax rbp) (rax rbp))
                        ((y.4 tmp-ra.6 rbp)
                        (y.4 tmp-ra.6 rbp)
                        (tmp-ra.6 fv0 rbp)
                        (fv0 r15 rbp)
                        (fv0 r15 rbp)))))
                    (call-undead ())
                    (conflicts
                    ((x.3 (rbp tmp-ra.6))
                    (tmp-ra.6 (x.3 rbp rax fv0 y.4))
                    (y.4 (rbp tmp-ra.6))
                    (rbp (x.3 tmp-ra.6 rax r15 fv0 y.4))
                    (fv0 (r15 rbp tmp-ra.6))
                    (r15 (rbp fv0))
                    (rax (rbp tmp-ra.6))))
                    (assignment ()))
                    (begin
                    (set! tmp-ra.6 r15)
                    (set! x.3 fv0)
                    (if (= x.3 0)
                        (begin (set! rax 1) (jump tmp-ra.6 rbp rax))
                        (begin
                        (set! y.4 x.3)
                        (set! y.4 (+ y.4 -1))
                        (set! fv0 y.4)
                        (set! r15 tmp-ra.6)
                        (jump L.odd?.1 rbp r15 fv0)))))
                (begin
                    (set! tmp-ra.7 r15)
                    (set! fv0 5)
                    (set! r15 tmp-ra.7)
                    (jump L.even?.2 rbp r15 fv0))))

     `(module
        ((locals (tmp-ra.7))
        (conflicts
            ((tmp-ra.7 (fv0 rbp))
            (rbp (r15 fv0 tmp-ra.7))
            (fv0 (r15 rbp tmp-ra.7))
            (r15 (rbp fv0))))
        (assignment ()))
        (define L.odd?.1
            ((locals (x.1 tmp-ra.5 y.2))
            (conflicts
            ((x.1 (rbp tmp-ra.5))
            (tmp-ra.5 (x.1 rbp rax fv0 y.2))
            (y.2 (rbp tmp-ra.5))
            (rbp (x.1 tmp-ra.5 rax r15 fv0 y.2))
            (fv0 (r15 rbp tmp-ra.5))
            (r15 (rbp fv0))
            (rax (rbp tmp-ra.5))))
            (assignment ()))
            (begin
            (set! tmp-ra.5 r15)
            (set! x.1 fv0)
            (if (= x.1 0)
                (begin (set! rax 0) (jump tmp-ra.5 rbp rax))
                (begin
                (set! y.2 x.1)
                (set! y.2 (+ y.2 -1))
                (set! fv0 y.2)
                (set! r15 tmp-ra.5)
                (jump L.even?.2 rbp r15 fv0)))))
        (define L.even?.2
            ((locals (x.3 tmp-ra.6 y.4))
            (conflicts
            ((x.3 (rbp tmp-ra.6))
            (tmp-ra.6 (x.3 rbp rax fv0 y.4))
            (y.4 (rbp tmp-ra.6))
            (rbp (x.3 tmp-ra.6 rax r15 fv0 y.4))
            (fv0 (r15 rbp tmp-ra.6))
            (r15 (rbp fv0))
            (rax (rbp tmp-ra.6))))
            (assignment ()))
            (begin
            (set! tmp-ra.6 r15)
            (set! x.3 fv0)
            (if (= x.3 0)
                (begin (set! rax 1) (jump tmp-ra.6 rbp rax))
                (begin
                (set! y.4 x.3)
                (set! y.4 (+ y.4 -1))
                (set! fv0 y.4)
                (set! r15 tmp-ra.6)
                (jump L.odd?.1 rbp r15 fv0)))))
        (begin
            (set! tmp-ra.7 r15)
            (set! fv0 5)
            (set! r15 tmp-ra.7)
            (jump L.even?.2 rbp r15 fv0)))))

(test-case "allocate 9 - public test 4"
    (check-match
        (allocate-frames
            `(module
                ((new-frames ())
                (locals (tmp-ra.6))
                (call-undead ())
                (undead-out ((tmp-ra.6 rbp) (tmp-ra.6 rax rbp) (rax rbp)))
                (conflicts ((tmp-ra.6 (rax rbp)) (rbp (rax tmp-ra.6)) (rax (rbp tmp-ra.6))))
                (assignment ()))
                (define L.zero.1
                    ((new-frames ())
                    (locals (tmp-ra.5 v0.4 v1.3 v2.2 v3.1))
                    (undead-out
                    ((fv0 fv1 fv2 fv3 tmp-ra.5 rbp)
                    (fv1 fv2 fv3 tmp-ra.5 rbp)
                    (fv2 fv3 tmp-ra.5 rbp)
                    (fv3 tmp-ra.5 rbp)
                    (tmp-ra.5 rbp)
                    (tmp-ra.5 rax rbp)
                    (rax rbp)))
                    (call-undead ())
                    (conflicts
                    ((tmp-ra.5 (rax v3.1 v2.2 v1.3 v0.4 rbp fv3 fv2 fv1 fv0))
                    (v0.4 (rbp tmp-ra.5 fv3 fv2 fv1))
                    (v1.3 (rbp tmp-ra.5 fv3 fv2))
                    (v2.2 (rbp tmp-ra.5 fv3))
                    (v3.1 (rbp tmp-ra.5))
                    (fv0 (tmp-ra.5))
                    (fv1 (v0.4 tmp-ra.5))
                    (fv2 (v1.3 v0.4 tmp-ra.5))
                    (fv3 (v2.2 v1.3 v0.4 tmp-ra.5))
                    (rbp (rax v3.1 v2.2 v1.3 v0.4 tmp-ra.5))
                    (rax (rbp tmp-ra.5))))
                    (assignment ()))
                    (begin
                    (set! tmp-ra.5 r15)
                    (set! v0.4 fv0)
                    (set! v1.3 fv1)
                    (set! v2.2 fv2)
                    (set! v3.1 fv3)
                    (set! rax 0)
                    (jump tmp-ra.5 rbp rax)))
                (begin (set! tmp-ra.6 r15) (set! rax 0) (jump tmp-ra.6 rbp rax))))

     `(module
        ((locals (tmp-ra.6))
        (conflicts ((tmp-ra.6 (rax rbp)) (rbp (rax tmp-ra.6)) (rax (rbp tmp-ra.6))))
        (assignment ()))
        (define L.zero.1
            ((locals (tmp-ra.5 v0.4 v1.3 v2.2 v3.1))
            (conflicts
            ((tmp-ra.5 (rax v3.1 v2.2 v1.3 v0.4 rbp fv3 fv2 fv1 fv0))
            (v0.4 (rbp tmp-ra.5 fv3 fv2 fv1))
            (v1.3 (rbp tmp-ra.5 fv3 fv2))
            (v2.2 (rbp tmp-ra.5 fv3))
            (v3.1 (rbp tmp-ra.5))
            (fv0 (tmp-ra.5))
            (fv1 (v0.4 tmp-ra.5))
            (fv2 (v1.3 v0.4 tmp-ra.5))
            (fv3 (v2.2 v1.3 v0.4 tmp-ra.5))
            (rbp (rax v3.1 v2.2 v1.3 v0.4 tmp-ra.5))
            (rax (rbp tmp-ra.5))))
            (assignment ()))
            (begin
            (set! tmp-ra.5 r15)
            (set! v0.4 fv0)
            (set! v1.3 fv1)
            (set! v2.2 fv2)
            (set! v3.1 fv3)
            (set! rax 0)
            (jump tmp-ra.5 rbp rax)))
        (begin (set! tmp-ra.6 r15) (set! rax 0) (jump tmp-ra.6 rbp rax)))))

(test-case "allocate 10 - public test 5"
    (check-match
        (allocate-frames
            `(module
                ((new-frames ())
                (locals (y.2 tmp-ra.4))
                (call-undead ())
                (undead-out
                    ((tmp-ra.4 rbp)
                    (tmp-ra.4 y.2 rbp)
                    (tmp-ra.4 y.2 fv0 rbp)
                    (y.2 fv0 r15 rbp)
                    (fv0 r15 rbp)))
                (conflicts
                    ((y.2 (r15 fv0 rbp tmp-ra.4))
                    (tmp-ra.4 (fv0 y.2 rbp))
                    (rbp (r15 fv0 y.2 tmp-ra.4))
                    (fv0 (r15 rbp y.2 tmp-ra.4))
                    (r15 (rbp fv0 y.2))))
                (assignment ()))
                (define L.id.1
                    ((new-frames ())
                    (locals (tmp-ra.3 x.1))
                    (undead-out
                    ((fv0 tmp-ra.3 rbp) (x.1 tmp-ra.3 rbp) (tmp-ra.3 rax rbp) (rax rbp)))
                    (call-undead ())
                    (conflicts
                    ((tmp-ra.3 (rax x.1 rbp fv0))
                    (x.1 (rbp tmp-ra.3))
                    (fv0 (tmp-ra.3))
                    (rbp (rax x.1 tmp-ra.3))
                    (rax (rbp tmp-ra.3))))
                    (assignment ()))
                    (begin
                    (set! tmp-ra.3 r15)
                    (set! x.1 fv0)
                    (set! rax x.1)
                    (jump tmp-ra.3 rbp rax)))
                (begin
                    (set! tmp-ra.4 r15)
                    (set! y.2 L.id.1)
                    (set! fv0 5)
                    (set! r15 tmp-ra.4)
                    (jump y.2 rbp r15 fv0))))

     `(module
        ((locals (y.2 tmp-ra.4))
        (conflicts
            ((y.2 (r15 fv0 rbp tmp-ra.4))
            (tmp-ra.4 (fv0 y.2 rbp))
            (rbp (r15 fv0 y.2 tmp-ra.4))
            (fv0 (r15 rbp y.2 tmp-ra.4))
            (r15 (rbp fv0 y.2))))
        (assignment ()))
        (define L.id.1
            ((locals (tmp-ra.3 x.1))
            (conflicts
            ((tmp-ra.3 (rax x.1 rbp fv0))
            (x.1 (rbp tmp-ra.3))
            (fv0 (tmp-ra.3))
            (rbp (rax x.1 tmp-ra.3))
            (rax (rbp tmp-ra.3))))
            (assignment ()))
            (begin
            (set! tmp-ra.3 r15)
            (set! x.1 fv0)
            (set! rax x.1)
            (jump tmp-ra.3 rbp rax)))
        (begin
            (set! tmp-ra.4 r15)
            (set! y.2 L.id.1)
            (set! fv0 5)
            (set! r15 tmp-ra.4)
            (jump y.2 rbp r15 fv0)))))

(test-case "allocate 11 - public test 6"
    (check-match
        (allocate-frames
            `(module
                ((new-frames ())
                (locals (y.3 tmp-ra.6))
                (call-undead ())
                (undead-out
                    ((tmp-ra.6 rbp)
                    ((tmp-ra.6 rbp) (tmp-ra.6 y.3 rbp) (tmp-ra.6 y.3 rbp))
                    (tmp-ra.6 y.3 fv0 rbp)
                    (y.3 fv0 r15 rbp)
                    (fv0 r15 rbp)))
                (conflicts
                    ((y.3 (r15 fv0 rbp tmp-ra.6))
                    (tmp-ra.6 (fv0 y.3 rbp))
                    (rbp (r15 fv0 y.3 tmp-ra.6))
                    (fv0 (r15 rbp y.3 tmp-ra.6))
                    (r15 (rbp fv0 y.3))))
                (assignment ()))
                (define L.id1.1
                    ((new-frames ())
                    (locals (tmp-ra.4 x.1))
                    (undead-out
                    ((fv0 tmp-ra.4 rbp) (x.1 tmp-ra.4 rbp) (tmp-ra.4 rax rbp) (rax rbp)))
                    (call-undead ())
                    (conflicts
                    ((tmp-ra.4 (rax x.1 rbp fv0))
                    (x.1 (rbp tmp-ra.4))
                    (fv0 (tmp-ra.4))
                    (rbp (rax x.1 tmp-ra.4))
                    (rax (rbp tmp-ra.4))))
                    (assignment ()))
                    (begin
                    (set! tmp-ra.4 r15)
                    (set! x.1 fv0)
                    (set! rax x.1)
                    (jump tmp-ra.4 rbp rax)))
                (define L.id2.2
                    ((new-frames ())
                    (locals (tmp-ra.5 x.2))
                    (undead-out
                    ((fv0 tmp-ra.5 rbp) (x.2 tmp-ra.5 rbp) (tmp-ra.5 rax rbp) (rax rbp)))
                    (call-undead ())
                    (conflicts
                    ((tmp-ra.5 (rax x.2 rbp fv0))
                    (x.2 (rbp tmp-ra.5))
                    (fv0 (tmp-ra.5))
                    (rbp (rax x.2 tmp-ra.5))
                    (rax (rbp tmp-ra.5))))
                    (assignment ()))
                    (begin
                    (set! tmp-ra.5 r15)
                    (set! x.2 fv0)
                    (set! rax x.2)
                    (jump tmp-ra.5 rbp rax)))
                (begin
                    (set! tmp-ra.6 r15)
                    (if (true) (set! y.3 L.id1.1) (set! y.3 L.id2.2))
                    (set! fv0 5)
                    (set! r15 tmp-ra.6)
                    (jump y.3 rbp r15 fv0))))

     `(module
        ((locals (y.3 tmp-ra.6))
        (conflicts
            ((y.3 (r15 fv0 rbp tmp-ra.6))
            (tmp-ra.6 (fv0 y.3 rbp))
            (rbp (r15 fv0 y.3 tmp-ra.6))
            (fv0 (r15 rbp y.3 tmp-ra.6))
            (r15 (rbp fv0 y.3))))
        (assignment ()))
        (define L.id1.1
            ((locals (tmp-ra.4 x.1))
            (conflicts
            ((tmp-ra.4 (rax x.1 rbp fv0))
            (x.1 (rbp tmp-ra.4))
            (fv0 (tmp-ra.4))
            (rbp (rax x.1 tmp-ra.4))
            (rax (rbp tmp-ra.4))))
            (assignment ()))
            (begin
            (set! tmp-ra.4 r15)
            (set! x.1 fv0)
            (set! rax x.1)
            (jump tmp-ra.4 rbp rax)))
        (define L.id2.2
            ((locals (tmp-ra.5 x.2))
            (conflicts
            ((tmp-ra.5 (rax x.2 rbp fv0))
            (x.2 (rbp tmp-ra.5))
            (fv0 (tmp-ra.5))
            (rbp (rax x.2 tmp-ra.5))
            (rax (rbp tmp-ra.5))))
            (assignment ()))
            (begin
            (set! tmp-ra.5 r15)
            (set! x.2 fv0)
            (set! rax x.2)
            (jump tmp-ra.5 rbp rax)))
        (begin
            (set! tmp-ra.6 r15)
            (if (true) (set! y.3 L.id1.1) (set! y.3 L.id2.2))
            (set! fv0 5)
            (set! r15 tmp-ra.6)
            (jump y.3 rbp r15 fv0)))))

(test-case "allocate 12 - public test 7"
    (check-match
        (allocate-frames
            `(module
                ((new-frames ())
                (locals (tmp-ra.6))
                (call-undead ())
                (undead-out ((tmp-ra.6 rbp) (tmp-ra.6 fv0 rbp) (fv0 r15 rbp) (fv0 r15 rbp)))
                (conflicts
                    ((tmp-ra.6 (fv0 rbp))
                    (rbp (r15 fv0 tmp-ra.6))
                    (fv0 (r15 rbp tmp-ra.6))
                    (r15 (rbp fv0))))
                (assignment ()))
                (define L.fact.1
                    ((new-frames ((nfv.5)))
                    (locals (y.3 nfv.5 z.2))
                    (undead-out
                    ((fv0 tmp-ra.4 rbp)
                    (x.1 tmp-ra.4 rbp)
                    ((x.1 tmp-ra.4 rbp)
                        ((tmp-ra.4 rax rbp) (rax rbp))
                        ((z.2 x.1 tmp-ra.4 rbp)
                        (z.2 x.1 tmp-ra.4 rbp)
                        ((rax x.1 tmp-ra.4 rbp) ((nfv.5 rbp) (nfv.5 r15 rbp) (nfv.5 r15 rbp)))
                        (x.1 y.3 tmp-ra.4 rbp)
                        (y.3 rax tmp-ra.4 rbp)
                        (tmp-ra.4 rax rbp)
                        (rax rbp)))))
                    (call-undead (x.1 tmp-ra.4))
                    (conflicts
                    ((x.1 (rbp tmp-ra.4 y.3 z.2))
                    (tmp-ra.4 (x.1 rbp fv0 rax y.3 z.2))
                    (y.3 (rax rbp tmp-ra.4 x.1))
                    (nfv.5 (r15 rbp))
                    (z.2 (x.1 rbp tmp-ra.4))
                    (rbp (x.1 tmp-ra.4 rax y.3 r15 nfv.5 z.2))
                    (r15 (rbp nfv.5))
                    (rax (rbp tmp-ra.4 y.3))
                    (fv0 (tmp-ra.4))))
                    (assignment ((tmp-ra.4 fv1) (x.1 fv0))))
                    (begin
                    (set! tmp-ra.4 r15)
                    (set! x.1 fv0)
                    (if (= x.1 0)
                        (begin (set! rax 1) (jump tmp-ra.4 rbp rax))
                        (begin
                        (set! z.2 x.1)
                        (set! z.2 (+ z.2 -1))
                        (return-point L.rp.2
                            (begin
                            (set! nfv.5 z.2)
                            (set! r15 L.rp.2)
                            (jump L.fact.1 rbp r15 nfv.5)))
                        (set! y.3 rax)
                        (set! rax x.1)
                        (set! rax (* rax y.3))
                        (jump tmp-ra.4 rbp rax)))))
                (begin
                    (set! tmp-ra.6 r15)
                    (set! fv0 5)
                    (set! r15 tmp-ra.6)
                    (jump L.fact.1 rbp r15 fv0))))

     `(module
        ((locals (tmp-ra.6))
        (conflicts
            ((tmp-ra.6 (fv0 rbp))
            (rbp (r15 fv0 tmp-ra.6))
            (fv0 (r15 rbp tmp-ra.6))
            (r15 (rbp fv0))))
        (assignment ()))
        (define L.fact.1
            ((locals (y.3 z.2))
            (conflicts
            ((x.1 (rbp tmp-ra.4 y.3 z.2))
            (tmp-ra.4 (x.1 rbp fv0 rax y.3 z.2))
            (y.3 (rax rbp tmp-ra.4 x.1))
            (nfv.5 (r15 rbp))
            (z.2 (x.1 rbp tmp-ra.4))
            (rbp (x.1 tmp-ra.4 rax y.3 r15 nfv.5 z.2))
            (r15 (rbp nfv.5))
            (rax (rbp tmp-ra.4 y.3))
            (fv0 (tmp-ra.4))))
            (assignment ((nfv.5 fv2) (tmp-ra.4 fv1) (x.1 fv0))))
            (begin
            (set! tmp-ra.4 r15)
            (set! x.1 fv0)
            (if (= x.1 0)
                (begin (set! rax 1) (jump tmp-ra.4 rbp rax))
                (begin
                (set! z.2 x.1)
                (set! z.2 (+ z.2 -1))
                (begin
                    (set! rbp (- rbp 16))
                    (return-point
                    L.rp.2
                    (begin
                    (set! nfv.5 z.2)
                    (set! r15 L.rp.2)
                    (jump L.fact.1 rbp r15 nfv.5)))
                    (set! rbp (+ rbp 16)))
                (set! y.3 rax)
                (set! rax x.1)
                (set! rax (* rax y.3))
                (jump tmp-ra.4 rbp rax)))))
        (begin
            (set! tmp-ra.6 r15)
            (set! fv0 5)
            (set! r15 tmp-ra.6)
            (jump L.fact.1 rbp r15 fv0)))))

(test-case "allocate 13 - non-empty new-frames, frame size from assignment"
    (check-match
        (allocate-frames
            `(module
                ((new-frames ())
                (locals (tmp-ra.4))
                (call-undead ())
                (undead-out
                    ((tmp-ra.4 rbp)
                    (tmp-ra.4 fv1 rbp)
                    (tmp-ra.4 fv1 fv0 rbp)
                    (fv1 fv0 r15 rbp)
                    (fv1 fv0 r15 rbp)))
                (conflicts
                    ((tmp-ra.4 (fv0 fv1 rbp))
                    (rbp (r15 fv0 fv1 tmp-ra.4))
                    (fv1 (r15 fv0 rbp tmp-ra.4))
                    (fv0 (r15 rbp fv1 tmp-ra.4))
                    (r15 (rbp fv0 fv1))))
                (assignment ()))
                (define L.swap.1
                    ((new-frames ((nfv.2 nfv.3)))
                    (locals (y.2 x.1 z.3 nfv.3 nfv.2))
                    (undead-out
                        ((fv0 fv1 tmp-ra.1 rbp)
                        (fv1 x.1 tmp-ra.1 rbp)
                        (y.2 x.1 tmp-ra.1 rbp)
                        ((y.2 x.1 tmp-ra.1 rbp)
                            ((tmp-ra.1 rax rbp) (rax rbp))
                            (((rax tmp-ra.1 rbp)
                            ((y.2 nfv.3 rbp)
                            (nfv.3 nfv.2 rbp)
                            (nfv.3 nfv.2 r15 rbp)
                            (nfv.3 nfv.2 r15 rbp)))
                            (z.3 tmp-ra.1 rbp)
                            (tmp-ra.1 rax rbp)
                            (rax rbp)))))
                    (call-undead (tmp-ra.1))
                    (conflicts
                        ((y.2 (rbp tmp-ra.1 x.1 nfv.3))
                        (x.1 (y.2 rbp tmp-ra.1 fv1))
                        (tmp-ra.1 (y.2 x.1 rbp fv1 fv0 rax z.3))
                        (z.3 (rbp tmp-ra.1))
                        (nfv.3 (r15 nfv.2 rbp y.2))
                        (nfv.2 (r15 rbp nfv.3))
                        (rbp (y.2 x.1 tmp-ra.1 rax z.3 r15 nfv.2 nfv.3))
                        (r15 (rbp nfv.2 nfv.3))
                        (rax (rbp tmp-ra.1))
                        (fv0 (tmp-ra.1))
                        (fv1 (x.1 tmp-ra.1))))
                    (assignment ((tmp-ra.1 fv2))))
                    (begin
                        (set! tmp-ra.1 r15)
                        (set! x.1 fv0)
                        (set! y.2 fv1)
                        (if (< y.2 x.1)
                            (begin (set! rax x.1) (jump tmp-ra.1 rbp rax))
                            (begin
                                (return-point L.rp.1
                                    (begin
                                    (set! nfv.3 x.1)
                                    (set! nfv.2 y.2)
                                    (set! r15 L.rp.1)
                                    (jump L.swap.1 rbp r15 nfv.2 nfv.3)))
                                (set! z.3 rax)
                                (set! rax z.3)
                                (jump tmp-ra.1 rbp rax)))))
                (begin
                    (set! tmp-ra.4 r15)
                    (set! fv1 2)
                    (set! fv0 1)
                    (set! r15 tmp-ra.4)
                    (jump L.swap.1 rbp r15 fv0 fv1))))

     `(module
        ((locals (tmp-ra.4))
        (conflicts
            ((tmp-ra.4 (fv0 fv1 rbp))
            (rbp (r15 fv0 fv1 tmp-ra.4))
            (fv1 (r15 fv0 rbp tmp-ra.4))
            (fv0 (r15 rbp fv1 tmp-ra.4))
            (r15 (rbp fv0 fv1))))
        (assignment ()))
        (define L.swap.1
            ((locals (y.2 x.1 z.3))
            (conflicts
            ((y.2 (rbp tmp-ra.1 x.1 nfv.3))
            (x.1 (y.2 rbp tmp-ra.1 fv1))
            (tmp-ra.1 (y.2 x.1 rbp fv1 fv0 rax z.3))
            (z.3 (rbp tmp-ra.1))
            (nfv.3 (r15 nfv.2 rbp y.2))
            (nfv.2 (r15 rbp nfv.3))
            (rbp (y.2 x.1 tmp-ra.1 rax z.3 r15 nfv.2 nfv.3))
            (r15 (rbp nfv.2 nfv.3))
            (rax (rbp tmp-ra.1))
            (fv0 (tmp-ra.1))
            (fv1 (x.1 tmp-ra.1))))
            (assignment ((nfv.3 fv4) (nfv.2 fv3) (tmp-ra.1 fv2)))) 
            (begin
            (set! tmp-ra.1 r15)
            (set! x.1 fv0)
            (set! y.2 fv1)
            (if (< y.2 x.1)
                (begin (set! rax x.1) (jump tmp-ra.1 rbp rax))
                (begin
                (begin
                    (set! rbp (- rbp 24))
                    (return-point
                    L.rp.1
                    (begin
                    (set! nfv.3 x.1)
                    (set! nfv.2 y.2)
                    (set! r15 L.rp.1)
                    (jump L.swap.1 rbp r15 nfv.2 nfv.3)))
                    (set! rbp (+ rbp 24)))
                (set! z.3 rax)
                (set! rax z.3)
                (jump tmp-ra.1 rbp rax)))))
        (begin
            (set! tmp-ra.4 r15)
            (set! fv1 2)
            (set! fv0 1)
            (set! r15 tmp-ra.4)
            (jump L.swap.1 rbp r15 fv0 fv1)))))

