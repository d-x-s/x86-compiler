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
        ((locals (y.1 x.3))
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
        ((locals (y.2 y.1 x.3))
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
        ((locals (x.11 x.1))
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
        ((locals (x.2 x.1 x.4))
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

(test-case "allocate 5 - n = fv referenced in assignment"
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
        ((locals (x.2 x.1 x.4))
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

(test-case "allocate 6 - public test 2"
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
        (assignment ((tmp-ra.4 fv0) (nfv.5 fv1))))
        (define L.id.1
            ((locals (x.1 tmp-ra.3))
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

(test-case "allocate 7 - non-empty new-frames, frame size from assignment reference"
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
            (assignment ((tmp-ra.1 fv2) (nfv.2 fv3) (nfv.3 fv4)))) 
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

(test-case "allocate 8 - multiple frames in new-frames, dupes in new-frames, n = call-undead length"
    (check-match
        (allocate-frames
            `(module
                ((new-frames ((nfv.1 nfv.2 nfv.2) (nfv.3 nfv.4)))
                (locals (x.3 y.1 nfv.1 nfv.2 nfv.3 nfv.4))
                (call-undead (x.1 x.2 fv0))
                (conflicts ((x.3 ()) (y.1 (rax)) (rax (y.1))))
                (assignment ((tmp.1 fv50))))

                (begin 
                    (set! rax (+ rax x.3)) 
                    (return-point L.one.1 (jump y.1)) 
                    (jump rax))))

     `(module
        ((locals (y.1 x.3))
        (conflicts ((x.3 ()) (y.1 (rax)) (rax (y.1))))
        (assignment ((tmp.1 fv50) (nfv.1 fv3) (nfv.2 fv5) (nfv.3 fv3) (nfv.4 fv4))))
        (begin
            (set! rax (+ rax x.3))
            (begin
            (set! rbp (- rbp 24))
            (return-point L.one.1 (jump y.1))
            (set! rbp (+ rbp 24)))
            (jump rax)))))

(test-case "allocate 9 - n = fvar in call-undead"
    (check-match
        (allocate-frames
            `(module
                ((new-frames ((nfv.1 nfv.2 nfv.2) (nfv.3 nfv.4)))
                (locals (x.3 y.1 nfv.1 nfv.2 nfv.3 nfv.4))
                (call-undead (x.1 x.2 fv20))
                (conflicts ((x.3 ()) (y.1 (rax)) (rax (y.1))))
                (assignment ((tmp.1 fv50))))

                (begin 
                    (set! rax (+ rax x.3)) 
                    (return-point L.one.1 (jump y.1)) 
                    (jump rax))))

     `(module
        ((locals (y.1 x.3))
        (conflicts ((x.3 ()) (y.1 (rax)) (rax (y.1))))
        (assignment
            ((tmp.1 fv50) (nfv.1 fv21) (nfv.2 fv23) (nfv.3 fv21) (nfv.4 fv22))))
        (begin
            (set! rax (+ rax x.3))
            (begin
            (set! rbp (- rbp 168))
            (return-point L.one.1 (jump y.1))
            (set! rbp (+ rbp 168)))
            (jump rax)))))

(test-case "allocate 10 - n = assignment reference"
    (check-match
        (allocate-frames
            `(module
                ((new-frames ((nfv.1 nfv.2 nfv.2) (nfv.3 nfv.4)))
                (locals (x.3 y.1 nfv.1 nfv.2 nfv.3 nfv.4))
                (call-undead (x.1 x.2 fv20 tmp.1))
                (conflicts ((x.3 ()) (y.1 (rax)) (rax (y.1))))
                (assignment ((tmp.1 fv50))))

                (begin 
                    (set! rax (+ rax x.3)) 
                    (return-point L.one.1 (jump y.1)) 
                    (jump rax))))

     `(module
        ((locals (y.1 x.3))
        (conflicts ((x.3 ()) (y.1 (rax)) (rax (y.1))))
        (assignment
            ((tmp.1 fv50) (nfv.1 fv51) (nfv.2 fv53) (nfv.3 fv51) (nfv.4 fv52))))
        (begin
            (set! rax (+ rax x.3))
            (begin
            (set! rbp (- rbp 408))
            (return-point L.one.1 (jump y.1))
            (set! rbp (+ rbp 408)))
            (jump rax)))))

; M7 Tests

(test-case "allocate 11 - extend binops"
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
                    (set! rax (bitwise-and rax x.3)) 
                    (return-point L.one.1 (jump y.1)) 
                    (jump rax))))

     `(module
        ((locals (y.1 x.3))
        (conflicts ((x.3 ()) (y.1 (rax)) (rax (y.1))))
        (assignment ()))
        (begin
            (set! rax (bitwise-and rax x.3))
            (begin
            (set! rbp (- rbp 0))
            (return-point L.one.1 (jump y.1))
            (set! rbp (+ rbp 0)))
            (jump rax)))))

; very large test
; (test-case "allocate 12 - complex test"
;     (check-match
;         (allocate-frames
;             `(module 
;                 ((new-frames ()) 
;                  (locals (tmp-ra.157)) 
;                  (call-undead ()) 
;                  (undead-out ((tmp-ra.157 rbp) (tmp-ra.157 rbp fv0) (tmp-ra.157 rbp r9 fv0) (tmp-ra.157 rbp r8 r9 fv0) (tmp-ra.157 rbp rcx r8 r9 fv0) (tmp-ra.157 rbp rdx rcx r8 r9 fv0) (tmp-ra.157 rbp rsi rdx rcx r8 r9 fv0) (tmp-ra.157 rbp rdi rsi rdx rcx r8 r9 fv0) (rbp r15 rdi rsi rdx rcx r8 r9 fv0) (rbp r15 rdi rsi rdx rcx r8 r9 fv0))) 
;                  (conflicts ((tmp-ra.157 (rdi rsi rdx rcx r8 r9 fv0 rbp)) (rbp (r15 rdi rsi rdx rcx r8 r9 fv0 tmp-ra.157)) (fv0 (r15 rdi rsi rdx rcx r8 r9 rbp tmp-ra.157)) (r9 (r15 rdi rsi rdx rcx r8 fv0 rbp tmp-ra.157)) (r8 (r15 rdi rsi rdx rcx fv0 r9 rbp tmp-ra.157)) (rcx (r15 rdi rsi rdx fv0 r9 r8 rbp tmp-ra.157)) (rdx (r15 rdi rsi fv0 r9 r8 rcx rbp tmp-ra.157)) (rsi (r15 rdi fv0 r9 r8 rcx rdx rbp tmp-ra.157)) (rdi (r15 fv0 r9 r8 rcx rdx rsi rbp tmp-ra.157)) (r15 (fv0 r9 r8 rcx rdx rsi rdi rbp)))) 
;                  (assignment ())) 
;                 (define L.+.1 
;                     ((new-frames ()) 
;                      (locals (tmp.147 tmp.4 tmp.148 tmp-ra.158 tmp.150 tmp.3 tmp.149)) 
;                      (undead-out ((rdi rsi tmp-ra.158 rbp) (rsi tmp.3 tmp-ra.158 rbp) (tmp.3 tmp.4 tmp-ra.158 rbp) (((((tmp.148 tmp.3 tmp.4 tmp-ra.158 rbp) (tmp.148 tmp.3 tmp.4 tmp-ra.158 rbp) (tmp.3 tmp.4 tmp-ra.158 rbp)) (tmp.147 tmp.3 tmp.4 tmp-ra.158 rbp) (tmp.147 tmp.3 tmp.4 tmp-ra.158 rbp)) (tmp.3 tmp.4 tmp-ra.158 rbp)) (((((tmp.150 tmp.3 tmp.4 tmp-ra.158 rbp) (tmp.150 tmp.3 tmp.4 tmp-ra.158 rbp) (tmp.3 tmp.4 tmp-ra.158 rbp)) (tmp.149 tmp.3 tmp.4 tmp-ra.158 rbp) (tmp.149 tmp.3 tmp.4 tmp-ra.158 rbp)) (tmp.3 tmp.4 tmp-ra.158 rbp)) ((tmp.4 tmp-ra.158 rbp rax) (tmp-ra.158 rbp rax) (rbp rax)) ((tmp-ra.158 rbp rax) (rbp rax))) ((tmp-ra.158 rbp rax) (rbp rax))))) 
;                      (call-undead ()) 
;                      (conflicts ((tmp.147 (rbp tmp-ra.158 tmp.4 tmp.3)) (tmp.4 (rax tmp.149 tmp.150 tmp.147 tmp.148 rbp tmp-ra.158 tmp.3)) (tmp.148 (tmp.4 rbp tmp-ra.158 tmp.3)) (tmp-ra.158 (rax tmp.149 tmp.150 tmp.147 tmp.148 tmp.4 tmp.3 rbp rsi rdi)) (tmp.150 (tmp.3 rbp tmp-ra.158 tmp.4)) (tmp.3 (tmp.149 tmp.150 tmp.147 tmp.148 tmp.4 rbp tmp-ra.158 rsi)) (tmp.149 (rbp tmp-ra.158 tmp.4 tmp.3)) (rdi (tmp-ra.158)) (rsi (tmp.3 tmp-ra.158)) (rbp (rax tmp.149 tmp.150 tmp.147 tmp.148 tmp.4 tmp.3 tmp-ra.158)) (rax (rbp tmp-ra.158 tmp.4)))) 
;                      (assignment ())) 
;                     (begin 
;                         (set! tmp-ra.158 r15) 
;                         (set! tmp.3 rdi) 
;                         (set! tmp.4 rsi) 
;                         (if (begin (if (begin (set! tmp.148 tmp.4) (set! tmp.148 (bitwise-and tmp.148 7)) (= tmp.148 0)) (set! tmp.147 14) (set! tmp.147 6)) (!= tmp.147 6)) 
;                             (if (begin (if (begin (set! tmp.150 tmp.3) (set! tmp.150 (bitwise-and tmp.150 7)) (= tmp.150 0)) (set! tmp.149 14) (set! tmp.149 6)) (!= tmp.149 6)) (begin (set! rax tmp.3) (set! rax (+ rax tmp.4)) (jump tmp-ra.158 rbp rax)) (begin (set! rax 574) (jump tmp-ra.158 rbp rax))) 
;                             (begin (set! rax 574) (jump tmp-ra.158 rbp rax))))) 
;                 (define L.F.10 
;                     ((new-frames ()) 
;                      (locals (tmp.151 g.35 f.34 e.33 d.32 c.31 b.30 a.29)) 
;                      (undead-out ((rdi rsi rdx rcx r8 r9 fv0 tmp-ra.159 rbp) (rsi rdx rcx r8 r9 fv0 tmp-ra.159 a.29 rbp) (rdx rcx r8 r9 fv0 tmp-ra.159 b.30 a.29 rbp) (rcx r8 r9 fv0 tmp-ra.159 c.31 b.30 a.29 rbp) (r8 r9 fv0 tmp-ra.159 d.32 c.31 b.30 a.29 rbp) (r9 fv0 tmp-ra.159 e.33 d.32 c.31 b.30 a.29 rbp) (fv0 tmp-ra.159 f.34 e.33 d.32 c.31 b.30 a.29 rbp) (tmp-ra.159 g.35 f.34 e.33 d.32 c.31 b.30 a.29 rbp) ((rax tmp-ra.159 rbp) ((g.35 f.34 e.33 d.32 c.31 b.30 a.29 rbp fv1) (f.34 e.33 d.32 c.31 b.30 a.29 rbp fv0 fv1) (e.33 d.32 c.31 b.30 a.29 rbp r9 fv0 fv1) (d.32 c.31 b.30 a.29 rbp r8 r9 fv0 fv1) (c.31 b.30 a.29 rbp rcx r8 r9 fv0 fv1) (b.30 a.29 rbp rdx rcx r8 r9 fv0 fv1) (a.29 rbp rsi rdx rcx r8 r9 fv0 fv1) (rbp rdi rsi rdx rcx r8 r9 fv0 fv1) (rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1) (rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1))) (tmp.151 tmp-ra.159 rbp) (tmp-ra.159 rbp rsi) (tmp-ra.159 rbp rdi rsi) (rbp r15 rdi rsi) (rbp r15 rdi rsi))) 
;                      (call-undead (tmp-ra.159)) 
;                      (conflicts ((tmp-ra.159 (tmp.151 g.35 f.34 e.33 d.32 c.31 b.30 a.29 rbp fv0 r9 r8 rcx rdx rsi rdi)) (tmp.151 (rbp tmp-ra.159)) (g.35 (fv1 rbp a.29 b.30 c.31 d.32 e.33 f.34 tmp-ra.159)) (f.34 (fv1 g.35 rbp a.29 b.30 c.31 d.32 e.33 tmp-ra.159 fv0)) (e.33 (fv1 g.35 f.34 rbp a.29 b.30 c.31 d.32 tmp-ra.159 fv0 r9)) (d.32 (fv1 g.35 f.34 e.33 rbp a.29 b.30 c.31 tmp-ra.159 fv0 r9 r8)) (c.31 (fv1 g.35 f.34 e.33 d.32 rbp a.29 b.30 tmp-ra.159 fv0 r9 r8 rcx)) (b.30 (fv1 g.35 f.34 e.33 d.32 c.31 rbp a.29 tmp-ra.159 fv0 r9 r8 rcx rdx)) (a.29 (fv1 g.35 f.34 e.33 d.32 c.31 b.30 rbp tmp-ra.159 fv0 r9 r8 rcx rdx rsi)) (rdi (r15 fv1 fv0 r9 r8 rcx rdx rsi rbp tmp-ra.159)) (rsi (r15 rdi fv1 fv0 r9 r8 rcx rdx rbp a.29 tmp-ra.159)) (rdx (r15 rdi rsi fv1 fv0 r9 r8 rcx rbp b.30 a.29 tmp-ra.159)) (rcx (r15 rdi rsi rdx fv1 fv0 r9 r8 rbp c.31 b.30 a.29 tmp-ra.159)) (r8 (r15 rdi rsi rdx rcx fv1 fv0 r9 rbp d.32 c.31 b.30 a.29 tmp-ra.159)) (r9 (r15 rdi rsi rdx rcx r8 fv1 fv0 rbp e.33 d.32 c.31 b.30 a.29 tmp-ra.159)) (fv0 (r15 rdi rsi rdx rcx r8 r9 fv1 rbp f.34 e.33 d.32 c.31 b.30 a.29 tmp-ra.159)) (rbp (tmp.151 r15 rdi rsi rdx rcx r8 r9 fv0 fv1 g.35 f.34 e.33 d.32 c.31 b.30 a.29 tmp-ra.159)) (fv1 (r15 rdi rsi rdx rcx r8 r9 fv0 rbp a.29 b.30 c.31 d.32 e.33 f.34 g.35)) (r15 (fv1 fv0 r9 r8 rcx rdx rsi rdi rbp)))) 
;                      (assignment ((tmp-ra.159 fv0)))) 
;                     (begin 
;                         (set! tmp-ra.159 r15) 
;                         (set! a.29 rdi) 
;                         (set! b.30 rsi) 
;                         (set! c.31 rdx) 
;                         (set! d.32 rcx) 
;                         (set! e.33 r8) 
;                         (set! f.34 r9) 
;                         (set! g.35 fv0) 
;                         (return-point L.rp.13 (begin (set! fv1 64) (set! fv0 g.35) (set! r9 f.34) (set! r8 e.33) (set! rcx d.32) (set! rdx c.31) (set! rsi b.30) (set! rdi a.29) (set! r15 L.rp.13) (jump L.G.11 rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1))) 
;                         (set! tmp.151 rax) 
;                         (set! rsi tmp.151) 
;                         (set! rdi 80) 
;                         (set! r15 tmp-ra.159) 
;                         (jump L.+.1 rbp r15 rdi rsi))) 
;                 (define L.G.11 
;                     ((new-frames ()) 
;                      (locals (tmp-ra.160 a.36 b.37 c.38 d.39 e.40 f.41 g.42 h.43)) 
;                      (undead-out ((rdi rsi rdx rcx r8 r9 fv0 fv1 tmp-ra.160 rbp) (rsi rdx rcx r8 r9 fv0 fv1 a.36 tmp-ra.160 rbp) (rdx rcx r8 r9 fv0 fv1 b.37 a.36 tmp-ra.160 rbp) (rcx r8 r9 fv0 fv1 c.38 b.37 a.36 tmp-ra.160 rbp) (r8 r9 fv0 fv1 d.39 c.38 b.37 a.36 tmp-ra.160 rbp) (r9 fv0 fv1 e.40 d.39 c.38 b.37 a.36 tmp-ra.160 rbp) (fv0 fv1 f.41 e.40 d.39 c.38 b.37 a.36 tmp-ra.160 rbp) (fv1 g.42 f.41 e.40 d.39 c.38 b.37 a.36 tmp-ra.160 rbp) (h.43 g.42 f.41 e.40 d.39 c.38 b.37 a.36 tmp-ra.160 rbp) (h.43 g.42 f.41 e.40 d.39 c.38 b.37 a.36 tmp-ra.160 rbp fv2) (g.42 f.41 e.40 d.39 c.38 b.37 a.36 tmp-ra.160 rbp fv1 fv2) (f.41 e.40 d.39 c.38 b.37 a.36 tmp-ra.160 rbp fv0 fv1 fv2) (e.40 d.39 c.38 b.37 a.36 tmp-ra.160 rbp r9 fv0 fv1 fv2) (d.39 c.38 b.37 a.36 tmp-ra.160 rbp r8 r9 fv0 fv1 fv2) (c.38 b.37 a.36 tmp-ra.160 rbp rcx r8 r9 fv0 fv1 fv2) (b.37 a.36 tmp-ra.160 rbp rdx rcx r8 r9 fv0 fv1 fv2) (a.36 tmp-ra.160 rbp rsi rdx rcx r8 r9 fv0 fv1 fv2) (tmp-ra.160 rbp rdi rsi rdx rcx r8 r9 fv0 fv1 fv2) (rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2) (rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2))) 
;                      (call-undead ()) 
;                      (conflicts ((tmp-ra.160 (fv2 h.43 g.42 f.41 e.40 d.39 c.38 b.37 a.36 rbp fv1 fv0 r9 r8 rcx rdx rsi rdi)) (a.36 (fv2 h.43 g.42 f.41 e.40 d.39 c.38 b.37 rbp tmp-ra.160 fv1 fv0 r9 r8 rcx rdx rsi)) (b.37 (fv2 h.43 g.42 f.41 e.40 d.39 c.38 rbp tmp-ra.160 a.36 fv1 fv0 r9 r8 rcx rdx)) (c.38 (fv2 h.43 g.42 f.41 e.40 d.39 rbp tmp-ra.160 a.36 b.37 fv1 fv0 r9 r8 rcx)) (d.39 (fv2 h.43 g.42 f.41 e.40 rbp tmp-ra.160 a.36 b.37 c.38 fv1 fv0 r9 r8)) (e.40 (fv2 h.43 g.42 f.41 rbp tmp-ra.160 a.36 b.37 c.38 d.39 fv1 fv0 r9)) (f.41 (fv2 h.43 g.42 rbp tmp-ra.160 a.36 b.37 c.38 d.39 e.40 fv1 fv0)) (g.42 (fv2 h.43 rbp tmp-ra.160 a.36 b.37 c.38 d.39 e.40 f.41 fv1)) (h.43 (fv2 rbp tmp-ra.160 a.36 b.37 c.38 d.39 e.40 f.41 g.42)) (rdi (r15 fv2 fv1 fv0 r9 r8 rcx rdx rsi rbp tmp-ra.160)) (rsi (r15 rdi fv2 fv1 fv0 r9 r8 rcx rdx rbp a.36 tmp-ra.160)) (rdx (r15 rdi rsi fv2 fv1 fv0 r9 r8 rcx rbp b.37 a.36 tmp-ra.160)) (rcx (r15 rdi rsi rdx fv2 fv1 fv0 r9 r8 rbp c.38 b.37 a.36 tmp-ra.160)) (r8 (r15 rdi rsi rdx rcx fv2 fv1 fv0 r9 rbp d.39 c.38 b.37 a.36 tmp-ra.160)) (r9 (r15 rdi rsi rdx rcx r8 fv2 fv1 fv0 rbp e.40 d.39 c.38 b.37 a.36 tmp-ra.160)) (fv0 (r15 rdi rsi rdx rcx r8 r9 fv2 fv1 rbp f.41 e.40 d.39 c.38 b.37 a.36 tmp-ra.160)) (fv1 (r15 rdi rsi rdx rcx r8 r9 fv0 fv2 rbp g.42 f.41 e.40 d.39 c.38 b.37 a.36 tmp-ra.160)) (rbp (r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2 h.43 g.42 f.41 e.40 d.39 c.38 b.37 a.36 tmp-ra.160)) (fv2 (r15 rdi rsi rdx rcx r8 r9 fv0 fv1 rbp tmp-ra.160 a.36 b.37 c.38 d.39 e.40 f.41 g.42 h.43)) (r15 (fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp)))) 
;                      (assignment ())) 
;                     (begin 
;                         (set! tmp-ra.160 r15) 
;                         (set! a.36 rdi) 
;                         (set! b.37 rsi) 
;                         (set! c.38 rdx) 
;                         (set! d.39 rcx) 
;                         (set! e.40 r8) 
;                         (set! f.41 r9) 
;                         (set! g.42 fv0) 
;                         (set! h.43 fv1) 
;                         (set! fv2 72) 
;                         (set! fv1 h.43) 
;                         (set! fv0 g.42) 
;                         (set! r9 f.41) 
;                         (set! r8 e.40) 
;                         (set! rcx d.39) 
;                         (set! rdx c.38) 
;                         (set! rsi b.37) 
;                         (set! rdi a.36) 
;                         (set! r15 tmp-ra.160) 
;                         (jump L.H.12 rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2))) 
;                 (define L.H.12 
;                     ((new-frames ()) 
;                      (locals (r7.59 r6.58 r5.57 r4.56 r3.55 r2.54 r1.53 b.45 a.44)) 
;                      (undead-out ((rdi rsi rdx rcx r8 r9 fv0 fv1 fv2 tmp-ra.161 rbp) (rsi rdx rcx r8 r9 fv0 fv1 fv2 tmp-ra.161 a.44 rbp) (rdx rcx r8 r9 fv0 fv1 fv2 tmp-ra.161 b.45 a.44 rbp) (rcx r8 r9 fv0 fv1 fv2 c.46 tmp-ra.161 b.45 a.44 rbp) (r8 r9 fv0 fv1 fv2 c.46 tmp-ra.161 d.47 b.45 a.44 rbp) (r9 fv0 fv1 fv2 c.46 e.48 tmp-ra.161 d.47 b.45 a.44 rbp) (fv0 fv1 fv2 c.46 e.48 tmp-ra.161 f.49 d.47 b.45 a.44 rbp) (fv1 fv2 c.46 e.48 g.50 tmp-ra.161 f.49 d.47 b.45 a.44 rbp) (fv2 c.46 e.48 g.50 tmp-ra.161 h.51 f.49 d.47 b.45 a.44 rbp) (c.46 e.48 g.50 tmp-ra.161 j.52 h.51 f.49 d.47 b.45 a.44 rbp) ((rax d.47 f.49 h.51 j.52 tmp-ra.161 g.50 e.48 c.46 rbp) ((a.44 rbp rsi) (rbp rdi rsi) (rbp r15 rdi rsi) (rbp r15 rdi rsi))) (d.47 f.49 h.51 j.52 tmp-ra.161 g.50 e.48 c.46 r1.53 rbp) ((rax e.48 g.50 tmp-ra.161 j.52 h.51 f.49 d.47 rbp) ((r1.53 rbp rsi) (rbp rdi rsi) (rbp r15 rdi rsi) (rbp r15 rdi rsi))) (e.48 g.50 tmp-ra.161 j.52 h.51 f.49 d.47 r2.54 rbp) ((rax f.49 h.51 j.52 tmp-ra.161 g.50 e.48 rbp) ((r2.54 rbp rsi) (rbp rdi rsi) (rbp r15 rdi rsi) (rbp r15 rdi rsi))) (f.49 h.51 j.52 tmp-ra.161 g.50 e.48 r3.55 rbp) ((rax g.50 tmp-ra.161 j.52 h.51 f.49 rbp) ((r3.55 rbp rsi) (rbp rdi rsi) (rbp r15 rdi rsi) (rbp r15 rdi rsi))) (g.50 tmp-ra.161 j.52 h.51 f.49 r4.56 rbp) ((rax h.51 j.52 tmp-ra.161 g.50 rbp) ((r4.56 rbp rsi) (rbp rdi rsi) (rbp r15 rdi rsi) (rbp r15 rdi rsi))) (h.51 j.52 tmp-ra.161 g.50 r5.57 rbp) ((rax tmp-ra.161 j.52 h.51 rbp) ((r5.57 rbp rsi) (rbp rdi rsi) (rbp r15 rdi rsi) (rbp r15 rdi rsi))) (tmp-ra.161 j.52 h.51 r6.58 rbp) ((rax j.52 tmp-ra.161 rbp) ((r6.58 rbp rsi) (rbp rdi rsi) (rbp r15 rdi rsi) (rbp r15 rdi rsi))) (j.52 r7.59 tmp-ra.161 rbp) (r7.59 tmp-ra.161 rbp rsi) (tmp-ra.161 rbp rdi rsi) (rbp r15 rdi rsi) (rbp r15 rdi rsi))) 
;                      (call-undead (j.52 d.47 e.48 c.46 f.49 tmp-ra.161 g.50 h.51)) 
;                      (conflicts ((tmp-ra.161 (r7.59 r6.58 r5.57 r4.56 r3.55 r2.54 r1.53 j.52 h.51 g.50 f.49 e.48 d.47 c.46 b.45 a.44 rbp fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi)) (r7.59 (rsi rbp tmp-ra.161 j.52)) (j.52 (r7.59 r6.58 r5.57 r4.56 r3.55 r2.54 r1.53 rbp a.44 b.45 d.47 f.49 h.51 tmp-ra.161 g.50 e.48 c.46)) (h.51 (r6.58 r5.57 r4.56 r3.55 r2.54 r1.53 j.52 rbp a.44 b.45 d.47 f.49 tmp-ra.161 g.50 e.48 c.46 fv2)) (r6.58 (rsi rbp h.51 j.52 tmp-ra.161)) (g.50 (r5.57 r4.56 r3.55 r2.54 r1.53 j.52 h.51 rbp a.44 b.45 d.47 f.49 tmp-ra.161 e.48 c.46 fv2 fv1)) (r5.57 (rsi rbp g.50 tmp-ra.161 j.52 h.51)) (f.49 (r4.56 r3.55 r2.54 r1.53 j.52 h.51 g.50 rbp a.44 b.45 d.47 tmp-ra.161 e.48 c.46 fv2 fv1 fv0)) (r4.56 (rsi rbp f.49 h.51 j.52 tmp-ra.161 g.50)) (e.48 (r3.55 r2.54 r1.53 j.52 h.51 g.50 f.49 rbp a.44 b.45 d.47 tmp-ra.161 c.46 fv2 fv1 fv0 r9)) (r3.55 (rsi rbp e.48 g.50 tmp-ra.161 j.52 h.51 f.49)) (d.47 (r2.54 r1.53 j.52 h.51 g.50 f.49 e.48 rbp a.44 b.45 tmp-ra.161 c.46 fv2 fv1 fv0 r9 r8)) (r2.54 (rsi rbp d.47 f.49 h.51 j.52 tmp-ra.161 g.50 e.48)) (c.46 (r1.53 j.52 h.51 g.50 f.49 e.48 d.47 rbp a.44 b.45 tmp-ra.161 fv2 fv1 fv0 r9 r8 rcx)) (r1.53 (rsi rbp c.46 e.48 g.50 tmp-ra.161 j.52 h.51 f.49 d.47)) (b.45 (j.52 h.51 g.50 f.49 e.48 d.47 c.46 rbp a.44 tmp-ra.161 fv2 fv1 fv0 r9 r8 rcx rdx)) (a.44 (j.52 h.51 g.50 f.49 e.48 d.47 c.46 b.45 rbp tmp-ra.161 fv2 fv1 fv0 r9 r8 rcx rdx rsi)) (rdi (r15 rsi rbp tmp-ra.161)) (rsi (r7.59 r6.58 r5.57 r4.56 r3.55 r2.54 r1.53 r15 rdi rbp a.44 tmp-ra.161)) (rdx (b.45 a.44 tmp-ra.161)) (rcx (c.46 b.45 a.44 tmp-ra.161)) (r8 (d.47 c.46 b.45 a.44 tmp-ra.161)) (r9 (e.48 d.47 c.46 b.45 a.44 tmp-ra.161)) (fv0 (f.49 e.48 d.47 c.46 b.45 a.44 tmp-ra.161)) (fv1 (g.50 f.49 e.48 d.47 c.46 b.45 a.44 tmp-ra.161)) (fv2 (h.51 g.50 f.49 e.48 d.47 c.46 b.45 a.44 tmp-ra.161)) (rbp (r7.59 r6.58 r5.57 r4.56 r3.55 r2.54 r1.53 r15 rdi rsi j.52 h.51 g.50 f.49 e.48 d.47 c.46 b.45 a.44 tmp-ra.161)) (r15 (rsi rdi rbp)))) 
;                      (assignment ((h.51 fv0) (g.50 fv3) (tmp-ra.161 fv4) (f.49 fv5) (c.46 fv6) (e.48 fv7) (d.47 fv8) (j.52 fv1)))) 
;                     (begin 
;                         (set! tmp-ra.161 r15) 
;                         (set! a.44 rdi) 
;                         (set! b.45 rsi) 
;                         (set! c.46 rdx) 
;                         (set! d.47 rcx) 
;                         (set! e.48 r8) 
;                         (set! f.49 r9) 
;                         (set! g.50 fv0) 
;                         (set! h.51 fv1) 
;                         (set! j.52 fv2) 
;                         (return-point L.rp.14 (begin (set! rsi b.45) (set! rdi a.44) (set! r15 L.rp.14) (jump L.+.1 rbp r15 rdi rsi))) 
;                         (set! r1.53 rax) 
;                         (return-point L.rp.15 (begin (set! rsi c.46) (set! rdi r1.53) (set! r15 L.rp.15) (jump L.+.1 rbp r15 rdi rsi))) 
;                         (set! r2.54 rax) 
;                         (return-point L.rp.16 (begin (set! rsi d.47) (set! rdi r2.54) (set! r15 L.rp.16) (jump L.+.1 rbp r15 rdi rsi))) 
;                         (set! r3.55 rax) 
;                         (return-point L.rp.17 (begin (set! rsi e.48) (set! rdi r3.55) (set! r15 L.rp.17) (jump L.+.1 rbp r15 rdi rsi))) 
;                         (set! r4.56 rax) 
;                         (return-point L.rp.18 (begin (set! rsi f.49) (set! rdi r4.56) (set! r15 L.rp.18) (jump L.+.1 rbp r15 rdi rsi))) 
;                         (set! r5.57 rax) 
;                         (return-point L.rp.19 (begin (set! rsi g.50) (set! rdi r5.57) (set! r15 L.rp.19) (jump L.+.1 rbp r15 rdi rsi))) 
;                         (set! r6.58 rax) 
;                         (return-point L.rp.20 (begin (set! rsi h.51) (set! rdi r6.58) (set! r15 L.rp.20) (jump L.+.1 rbp r15 rdi rsi))) 
;                         (set! r7.59 rax) 
;                         (set! rsi j.52) 
;                         (set! rdi r7.59) 
;                         (set! r15 tmp-ra.161) 
;                         (jump L.+.1 rbp r15 rdi rsi))) 
;                 (begin 
;                     (set! tmp-ra.157 r15) 
;                     (set! fv0 56) 
;                     (set! r9 48) 
;                     (set! r8 40) 
;                     (set! rcx 32) 
;                     (set! rdx 24) 
;                     (set! rsi 16) 
;                     (set! rdi 8) 
;                     (set! r15 tmp-ra.157) 
;                     (jump L.F.10 rbp r15 rdi rsi rdx rcx r8 r9 fv0))))

;      `(module
;         ((locals (tmp-ra.157))
;         (conflicts
;             ((tmp-ra.157 (rdi rsi rdx rcx r8 r9 fv0 rbp))
;             (rbp (r15 rdi rsi rdx rcx r8 r9 fv0 tmp-ra.157))
;             (fv0 (r15 rdi rsi rdx rcx r8 r9 rbp tmp-ra.157))
;             (r9 (r15 rdi rsi rdx rcx r8 fv0 rbp tmp-ra.157))
;             (r8 (r15 rdi rsi rdx rcx fv0 r9 rbp tmp-ra.157))
;             (rcx (r15 rdi rsi rdx fv0 r9 r8 rbp tmp-ra.157))
;             (rdx (r15 rdi rsi fv0 r9 r8 rcx rbp tmp-ra.157))
;             (rsi (r15 rdi fv0 r9 r8 rcx rdx rbp tmp-ra.157))
;             (rdi (r15 fv0 r9 r8 rcx rdx rsi rbp tmp-ra.157))
;             (r15 (fv0 r9 r8 rcx rdx rsi rdi rbp))))
;         (assignment ()))
;         (define L.+.1
;             ((locals (tmp.149 tmp.3 tmp.150 tmp-ra.158 tmp.148 tmp.4 tmp.147))
;             (conflicts
;             ((tmp.147 (rbp tmp-ra.158 tmp.4 tmp.3))
;             (tmp.4 (rax tmp.149 tmp.150 tmp.147 tmp.148 rbp tmp-ra.158 tmp.3))
;             (tmp.148 (tmp.4 rbp tmp-ra.158 tmp.3))
;             (tmp-ra.158
;                 (rax tmp.149 tmp.150 tmp.147 tmp.148 tmp.4 tmp.3 rbp rsi rdi))
;             (tmp.150 (tmp.3 rbp tmp-ra.158 tmp.4))
;             (tmp.3 (tmp.149 tmp.150 tmp.147 tmp.148 tmp.4 rbp tmp-ra.158 rsi))
;             (tmp.149 (rbp tmp-ra.158 tmp.4 tmp.3))
;             (rdi (tmp-ra.158))
;             (rsi (tmp.3 tmp-ra.158))
;             (rbp (rax tmp.149 tmp.150 tmp.147 tmp.148 tmp.4 tmp.3 tmp-ra.158))
;             (rax (rbp tmp-ra.158 tmp.4))))
;             (assignment ()))
;             (begin
;             (set! tmp-ra.158 r15)
;             (set! tmp.3 rdi)
;             (set! tmp.4 rsi)
;             (if (begin
;                     (if (begin
;                         (set! tmp.148 tmp.4)
;                         (set! tmp.148 (bitwise-and tmp.148 7))
;                         (= tmp.148 0))
;                     (set! tmp.147 14)
;                     (set! tmp.147 6))
;                     (!= tmp.147 6))
;                 (if (begin
;                     (if (begin
;                             (set! tmp.150 tmp.3)
;                             (set! tmp.150 (bitwise-and tmp.150 7))
;                             (= tmp.150 0))
;                         (set! tmp.149 14)
;                         (set! tmp.149 6))
;                     (!= tmp.149 6))
;                 (begin
;                     (set! rax tmp.3)
;                     (set! rax (+ rax tmp.4))
;                     (jump tmp-ra.158 rbp rax))
;                 (begin (set! rax 574) (jump tmp-ra.158 rbp rax)))
;                 (begin (set! rax 574) (jump tmp-ra.158 rbp rax)))))
;         (define L.F.10
;             ((locals (a.29 b.30 c.31 d.32 e.33 f.34 g.35 tmp.151))
;              (conflicts ((tmp-ra.159 (tmp.151 g.35 f.34 e.33 d.32 c.31 b.30 a.29 rbp fv0 r9 r8 rcx rdx rsi rdi)) (tmp.151 (rbp tmp-ra.159)) (g.35 (fv1 rbp a.29 b.30 c.31 d.32 e.33 f.34 tmp-ra.159)) (f.34 (fv1 g.35 rbp a.29 b.30 c.31 d.32 e.33 tmp-ra.159 fv0)) (e.33 (fv1 g.35 f.34 rbp a.29 b.30 c.31 d.32 tmp-ra.159 fv0 r9)) (d.32 (fv1 g.35 f.34 e.33 rbp a.29 b.30 c.31 tmp-ra.159 fv0 r9 r8)) (c.31 (fv1 g.35 f.34 e.33 d.32 rbp a.29 b.30 tmp-ra.159 fv0 r9 r8 rcx)) (b.30 (fv1 g.35 f.34 e.33 d.32 c.31 rbp a.29 tmp-ra.159 fv0 r9 r8 rcx rdx)) (a.29 (fv1 g.35 f.34 e.33 d.32 c.31 b.30 rbp tmp-ra.159 fv0 r9 r8 rcx rdx rsi)) (rdi (r15 fv1 fv0 r9 r8 rcx rdx rsi rbp tmp-ra.159)) (rsi (r15 rdi fv1 fv0 r9 r8 rcx rdx rbp a.29 tmp-ra.159)) (rdx (r15 rdi rsi fv1 fv0 r9 r8 rcx rbp b.30 a.29 tmp-ra.159)) (rcx (r15 rdi rsi rdx fv1 fv0 r9 r8 rbp c.31 b.30 a.29 tmp-ra.159)) (r8 (r15 rdi rsi rdx rcx fv1 fv0 r9 rbp d.32 c.31 b.30 a.29 tmp-ra.159)) (r9 (r15 rdi rsi rdx rcx r8 fv1 fv0 rbp e.33 d.32 c.31 b.30 a.29 tmp-ra.159)) (fv0 (r15 rdi rsi rdx rcx r8 r9 fv1 rbp f.34 e.33 d.32 c.31 b.30 a.29 tmp-ra.159)) (rbp (tmp.151 r15 rdi rsi rdx rcx r8 r9 fv0 fv1 g.35 f.34 e.33 d.32 c.31 b.30 a.29 tmp-ra.159)) (fv1 (r15 rdi rsi rdx rcx r8 r9 fv0 rbp a.29 b.30 c.31 d.32 e.33 f.34 g.35)) (r15 (fv1 fv0 r9 r8 rcx rdx rsi rdi rbp))))
;              (assignment ((tmp-ra.159 fv0))))
;             (begin
;                 (set! tmp-ra.159 r15)
;                 (set! a.29 rdi)
;                 (set! b.30 rsi)
;                 (set! c.31 rdx)
;                 (set! d.32 rcx)
;                 (set! e.33 r8)
;                 (set! f.34 r9)
;                 (set! g.35 fv0)
;                 (begin
;                     (set! rbp (- rbp 8))
;                     (return-point
;                     L.rp.13
;                     (begin
;                     (set! fv1 64)
;                     (set! fv0 g.35)
;                     (set! r9 f.34)
;                     (set! r8 e.33)
;                     (set! rcx d.32)
;                     (set! rdx c.31)
;                     (set! rsi b.30)
;                     (set! rdi a.29)
;                     (set! r15 L.rp.13)
;                     (jump L.G.11 rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1)))
;                     (set! rbp (+ rbp 8)))
;                 (set! tmp.151 rax)
;                 (set! rsi tmp.151)
;                 (set! rdi 80)
;                 (set! r15 tmp-ra.159)
;                 (jump L.+.1 rbp r15 rdi rsi)))
;         (define L.G.11
;             ((locals (h.43 g.42 f.41 e.40 d.39 c.38 b.37 a.36 tmp-ra.160))
;             (conflicts ((tmp-ra.160 (fv2 h.43 g.42 f.41 e.40 d.39 c.38 b.37 a.36 rbp fv1 fv0 r9 r8 rcx rdx rsi rdi)) (a.36 (fv2 h.43 g.42 f.41 e.40 d.39 c.38 b.37 rbp tmp-ra.160 fv1 fv0 r9 r8 rcx rdx rsi)) (b.37 (fv2 h.43 g.42 f.41 e.40 d.39 c.38 rbp tmp-ra.160 a.36 fv1 fv0 r9 r8 rcx rdx)) (c.38 (fv2 h.43 g.42 f.41 e.40 d.39 rbp tmp-ra.160 a.36 b.37 fv1 fv0 r9 r8 rcx)) (d.39 (fv2 h.43 g.42 f.41 e.40 rbp tmp-ra.160 a.36 b.37 c.38 fv1 fv0 r9 r8)) (e.40 (fv2 h.43 g.42 f.41 rbp tmp-ra.160 a.36 b.37 c.38 d.39 fv1 fv0 r9)) (f.41 (fv2 h.43 g.42 rbp tmp-ra.160 a.36 b.37 c.38 d.39 e.40 fv1 fv0)) (g.42 (fv2 h.43 rbp tmp-ra.160 a.36 b.37 c.38 d.39 e.40 f.41 fv1)) (h.43 (fv2 rbp tmp-ra.160 a.36 b.37 c.38 d.39 e.40 f.41 g.42)) (rdi (r15 fv2 fv1 fv0 r9 r8 rcx rdx rsi rbp tmp-ra.160)) (rsi (r15 rdi fv2 fv1 fv0 r9 r8 rcx rdx rbp a.36 tmp-ra.160)) (rdx (r15 rdi rsi fv2 fv1 fv0 r9 r8 rcx rbp b.37 a.36 tmp-ra.160)) (rcx (r15 rdi rsi rdx fv2 fv1 fv0 r9 r8 rbp c.38 b.37 a.36 tmp-ra.160)) (r8 (r15 rdi rsi rdx rcx fv2 fv1 fv0 r9 rbp d.39 c.38 b.37 a.36 tmp-ra.160)) (r9 (r15 rdi rsi rdx rcx r8 fv2 fv1 fv0 rbp e.40 d.39 c.38 b.37 a.36 tmp-ra.160)) (fv0 (r15 rdi rsi rdx rcx r8 r9 fv2 fv1 rbp f.41 e.40 d.39 c.38 b.37 a.36 tmp-ra.160)) (fv1 (r15 rdi rsi rdx rcx r8 r9 fv0 fv2 rbp g.42 f.41 e.40 d.39 c.38 b.37 a.36 tmp-ra.160)) (rbp (r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2 h.43 g.42 f.41 e.40 d.39 c.38 b.37 a.36 tmp-ra.160)) (fv2 (r15 rdi rsi rdx rcx r8 r9 fv0 fv1 rbp tmp-ra.160 a.36 b.37 c.38 d.39 e.40 f.41 g.42 h.43)) (r15 (fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp))))
;             (assignment ()))
;             (begin
;             (set! tmp-ra.160 r15)
;             (set! a.36 rdi)
;             (set! b.37 rsi)
;             (set! c.38 rdx)
;             (set! d.39 rcx)
;             (set! e.40 r8)
;             (set! f.41 r9)
;             (set! g.42 fv0)
;             (set! h.43 fv1)
;             (set! fv2 72)
;             (set! fv1 h.43)
;             (set! fv0 g.42)
;             (set! r9 f.41)
;             (set! r8 e.40)
;             (set! rcx d.39)
;             (set! rdx c.38)
;             (set! rsi b.37)
;             (set! rdi a.36)
;             (set! r15 tmp-ra.160)
;             (jump L.H.12 rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2)))
;         (define L.H.12
;             ((locals (a.44 b.45 r1.53 r2.54 r3.55 r4.56 r5.57 r6.58 r7.59))
;             (conflicts ((tmp-ra.161 (r7.59 r6.58 r5.57 r4.56 r3.55 r2.54 r1.53 j.52 h.51 g.50 f.49 e.48 d.47 c.46 b.45 a.44 rbp fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi)) (r7.59 (rsi rbp tmp-ra.161 j.52)) (j.52 (r7.59 r6.58 r5.57 r4.56 r3.55 r2.54 r1.53 rbp a.44 b.45 d.47 f.49 h.51 tmp-ra.161 g.50 e.48 c.46)) (h.51 (r6.58 r5.57 r4.56 r3.55 r2.54 r1.53 j.52 rbp a.44 b.45 d.47 f.49 tmp-ra.161 g.50 e.48 c.46 fv2)) (r6.58 (rsi rbp h.51 j.52 tmp-ra.161)) (g.50 (r5.57 r4.56 r3.55 r2.54 r1.53 j.52 h.51 rbp a.44 b.45 d.47 f.49 tmp-ra.161 e.48 c.46 fv2 fv1)) (r5.57 (rsi rbp g.50 tmp-ra.161 j.52 h.51)) (f.49 (r4.56 r3.55 r2.54 r1.53 j.52 h.51 g.50 rbp a.44 b.45 d.47 tmp-ra.161 e.48 c.46 fv2 fv1 fv0)) (r4.56 (rsi rbp f.49 h.51 j.52 tmp-ra.161 g.50)) (e.48 (r3.55 r2.54 r1.53 j.52 h.51 g.50 f.49 rbp a.44 b.45 d.47 tmp-ra.161 c.46 fv2 fv1 fv0 r9)) (r3.55 (rsi rbp e.48 g.50 tmp-ra.161 j.52 h.51 f.49)) (d.47 (r2.54 r1.53 j.52 h.51 g.50 f.49 e.48 rbp a.44 b.45 tmp-ra.161 c.46 fv2 fv1 fv0 r9 r8)) (r2.54 (rsi rbp d.47 f.49 h.51 j.52 tmp-ra.161 g.50 e.48)) (c.46 (r1.53 j.52 h.51 g.50 f.49 e.48 d.47 rbp a.44 b.45 tmp-ra.161 fv2 fv1 fv0 r9 r8 rcx)) (r1.53 (rsi rbp c.46 e.48 g.50 tmp-ra.161 j.52 h.51 f.49 d.47)) (b.45 (j.52 h.51 g.50 f.49 e.48 d.47 c.46 rbp a.44 tmp-ra.161 fv2 fv1 fv0 r9 r8 rcx rdx)) (a.44 (j.52 h.51 g.50 f.49 e.48 d.47 c.46 b.45 rbp tmp-ra.161 fv2 fv1 fv0 r9 r8 rcx rdx rsi)) (rdi (r15 rsi rbp tmp-ra.161)) (rsi (r7.59 r6.58 r5.57 r4.56 r3.55 r2.54 r1.53 r15 rdi rbp a.44 tmp-ra.161)) (rdx (b.45 a.44 tmp-ra.161)) (rcx (c.46 b.45 a.44 tmp-ra.161)) (r8 (d.47 c.46 b.45 a.44 tmp-ra.161)) (r9 (e.48 d.47 c.46 b.45 a.44 tmp-ra.161)) (fv0 (f.49 e.48 d.47 c.46 b.45 a.44 tmp-ra.161)) (fv1 (g.50 f.49 e.48 d.47 c.46 b.45 a.44 tmp-ra.161)) (fv2 (h.51 g.50 f.49 e.48 d.47 c.46 b.45 a.44 tmp-ra.161)) (rbp (r7.59 r6.58 r5.57 r4.56 r3.55 r2.54 r1.53 r15 rdi rsi j.52 h.51 g.50 f.49 e.48 d.47 c.46 b.45 a.44 tmp-ra.161)) (r15 (rsi rdi rbp))))
;             (assignment ((h.51 fv0) (g.50 fv3) (tmp-ra.161 fv4) (f.49 fv5) (c.46 fv6) (e.48 fv7) (d.47 fv8) (j.52 fv1))))
;             (begin
;             (set! tmp-ra.161 r15)
;             (set! a.44 rdi)
;             (set! b.45 rsi)
;             (set! c.46 rdx)
;             (set! d.47 rcx)
;             (set! e.48 r8)
;             (set! f.49 r9)
;             (set! g.50 fv0)
;             (set! h.51 fv1)
;             (set! j.52 fv2)
;             (begin
;                 (set! rbp (- rbp 72))
;                 (return-point
;                 L.rp.14
;                 (begin
;                 (set! rsi b.45)
;                 (set! rdi a.44)
;                 (set! r15 L.rp.14)
;                 (jump L.+.1 rbp r15 rdi rsi)))
;                 (set! rbp (+ rbp 72)))
;             (set! r1.53 rax)
;             (begin
;                 (set! rbp (- rbp 72))
;                 (return-point
;                 L.rp.15
;                 (begin
;                 (set! rsi c.46)
;                 (set! rdi r1.53)
;                 (set! r15 L.rp.15)
;                 (jump L.+.1 rbp r15 rdi rsi)))
;                 (set! rbp (+ rbp 72)))
;             (set! r2.54 rax)
;             (begin
;                 (set! rbp (- rbp 72))
;                 (return-point
;                 L.rp.16
;                 (begin
;                 (set! rsi d.47)
;                 (set! rdi r2.54)
;                 (set! r15 L.rp.16)
;                 (jump L.+.1 rbp r15 rdi rsi)))
;                 (set! rbp (+ rbp 72)))
;             (set! r3.55 rax)
;             (begin
;                 (set! rbp (- rbp 72))
;                 (return-point
;                 L.rp.17
;                 (begin
;                 (set! rsi e.48)
;                 (set! rdi r3.55)
;                 (set! r15 L.rp.17)
;                 (jump L.+.1 rbp r15 rdi rsi)))
;                 (set! rbp (+ rbp 72)))
;             (set! r4.56 rax)
;             (begin
;                 (set! rbp (- rbp 72))
;                 (return-point
;                 L.rp.18
;                 (begin
;                 (set! rsi f.49)
;                 (set! rdi r4.56)
;                 (set! r15 L.rp.18)
;                 (jump L.+.1 rbp r15 rdi rsi)))
;                 (set! rbp (+ rbp 72)))
;             (set! r5.57 rax)
;             (begin
;                 (set! rbp (- rbp 72))
;                 (return-point
;                 L.rp.19
;                 (begin
;                 (set! rsi g.50)
;                 (set! rdi r5.57)
;                 (set! r15 L.rp.19)
;                 (jump L.+.1 rbp r15 rdi rsi)))
;                 (set! rbp (+ rbp 72)))
;             (set! r6.58 rax)
;             (begin
;                 (set! rbp (- rbp 72))
;                 (return-point
;                 L.rp.20
;                 (begin
;                 (set! rsi h.51)
;                 (set! rdi r6.58)
;                 (set! r15 L.rp.20)
;                 (jump L.+.1 rbp r15 rdi rsi)))
;                 (set! rbp (+ rbp 72)))
;             (set! r7.59 rax)
;             (set! rsi j.52)
;             (set! rdi r7.59)
;             (set! r15 tmp-ra.161)
;             (jump L.+.1 rbp r15 rdi rsi)))
;         (begin
;             (set! tmp-ra.157 r15)
;             (set! fv0 56)
;             (set! r9 48)
;             (set! r8 40)
;             (set! rcx 32)
;             (set! rdx 24)
;             (set! rsi 16)
;             (set! rdi 8)
;             (set! r15 tmp-ra.157)
;             (jump L.F.10 rbp r15 rdi rsi rdx rcx r8 r9 fv0)))))

; M8 Tests

(test-case "allocate 13 - effect is mref"
    (check-match
        (allocate-frames
            `(module
                ((new-frames (()))
                (locals (y.3 x.3 y.2 x.1 y.1))
                (call-undead ())
                (undead-out
                    ((y.1 x.1 r15 y.3 fv4 x.3 fv3 fv2 y.2 r14 fv1)
                    (y.2 fv2 fv3 x.3 fv4 y.3 r15 x.1 y.1)
                    ((y.3 fv4 x.3 y.1 fv3 fv2 y.2) (y.1 x.3 fv4 y.3) (y.1))
                    ((rax) ())
                    ()))
                (conflicts ((y.3 ()) (x.3 ()) (y.2 ()) (x.1 ()) (y.1 ())))
                (assignment ()))
                (begin
                    (mset! rsp 5 L.s.1)
                    (mset! fv1 r14 6)
                    (begin (mset! y.1 x.1 r15) (mset! y.2 fv2 fv3) (mset! y.3 fv4 x.3))
                    (return-point L.one.1 (jump y.1))
                    (jump rax))))

     `(module
        ((locals (y.1 x.1 y.2 x.3 y.3))
        (conflicts ((y.3 ()) (x.3 ()) (y.2 ()) (x.1 ()) (y.1 ())))
        (assignment ()))
        (begin
            (mset! rsp 5 L.s.1)
            (mset! fv1 r14 6)
            (begin (mset! y.1 x.1 r15) (mset! y.2 fv2 fv3) (mset! y.3 fv4 x.3))
            (begin
            (set! rbp (- rbp 0))
            (return-point L.one.1 (jump y.1))
            (set! rbp (+ rbp 0)))
            (jump rax)))))

(test-case "allocate 14 - effect is mset"
    (check-match
        (allocate-frames
            `(module
                ((new-frames (()))
                (locals (y.2 x.1 y.1 x.3 y.3))
                (call-undead ())
                (undead-out
                    ((fv1 r14 rsp fv2 fv3 x.3 y.3 r15 x.1 y.1 fv4 y.2)
                    (y.1 x.1 r15 y.3 x.3 fv3 fv2 rsp r14 fv1)
                    (rsp fv2 fv3 x.3 y.3 r15 x.1 y.1)
                    (y.3 x.3 y.1 fv3 fv2 rsp)
                    (y.1 x.3 y.3)
                    (y.1)
                    ((rax) ())
                    ()))
                (conflicts
                    ((y.2 (r14))
                    (x.1 (r14))
                    (y.1 (r14))
                    (x.3 (r14))
                    (y.3 (r14))
                    (r14 (y.2 fv4 y.1 x.1 r15 y.3 x.3 fv3 fv2 rsp fv1))
                    (fv1 (r14))
                    (rsp (r14))
                    (fv2 (r14))
                    (fv3 (r14))
                    (r15 (r14))
                    (fv4 (r14))))
                (assignment ()))
                (begin
                    (set! r14 L.s.1)
                    (mset! y.2 fv4 L.s.1)
                    (mset! fv1 r14 6)
                    (mset! y.1 x.1 r15)
                    (mset! rsp fv2 fv3)
                    (mset! y.3 5 x.3)
                    (return-point L.one.1 (jump y.1))
                    (jump rax))))

     `(module
        ((locals (y.3 x.3 y.1 x.1 y.2))
        (conflicts
            ((y.2 (r14))
            (x.1 (r14))
            (y.1 (r14))
            (x.3 (r14))
            (y.3 (r14))
            (r14 (y.2 fv4 y.1 x.1 r15 y.3 x.3 fv3 fv2 rsp fv1))
            (fv1 (r14))
            (rsp (r14))
            (fv2 (r14))
            (fv3 (r14))
            (r15 (r14))
            (fv4 (r14))))
        (assignment ()))
        (begin
            (set! r14 L.s.1)
            (mset! y.2 fv4 L.s.1)
            (mset! fv1 r14 6)
            (mset! y.1 x.1 r15)
            (mset! rsp fv2 fv3)
            (mset! y.3 5 x.3)
            (begin
            (set! rbp (- rbp 0))
            (return-point L.one.1 (jump y.1))
            (set! rbp (+ rbp 0)))
            (jump rax)))))
