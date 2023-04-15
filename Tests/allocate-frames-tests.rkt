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
            ((locals (z.3 x.1 y.2))
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

; M8 Tests

(test-case "allocate 12 - effect is mref"
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

(test-case "allocate 13 - effect is mset"
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

(test-case "allocate 14 - large test"
    (check-match
        (allocate-frames
            `(module 
                ((new-frames (())) 
                (locals (tmp.192)) 
                (call-undead (tmp-ra.325)) 
                (undead-out ((tmp-ra.325 rbp) ((rax tmp-ra.325 rbp) ((rbp rsi) (rbp rdi rsi) (rbp r15 rdi rsi) (rbp r15 rdi rsi))) (tmp.192 tmp-ra.325 rbp) (tmp-ra.325 rbp rdi) (rbp r15 rdi) (rbp r15 rdi))) 
                (conflicts ((tmp-ra.325 (rdi tmp.192 rbp)) (tmp.192 (rbp tmp-ra.325)) (rbp (tmp.192 r15 rdi rsi tmp-ra.325)) (rsi (r15 rdi rbp)) (rdi (tmp-ra.325 r15 rsi rbp)) (r15 (rsi rdi rbp)))) 
                (assignment ((tmp-ra.325 fv0)))) 
                (define L.car.6 
                    ((new-frames ()) 
                    (locals (tmp.190 tmp.35 tmp-ra.326 tmp.189)) 
                    (undead-out ((rdi tmp-ra.326 rbp) (tmp.35 tmp-ra.326 rbp) (((((tmp.190 tmp.35 tmp-ra.326 rbp) (tmp.190 tmp.35 tmp-ra.326 rbp) (tmp.35 tmp-ra.326 rbp)) (tmp.189 tmp.35 tmp-ra.326 rbp) (tmp.189 tmp.35 tmp-ra.326 rbp)) (tmp.35 tmp-ra.326 rbp)) ((tmp-ra.326 rbp rax) (rbp rax)) ((tmp-ra.326 rbp rax) (rbp rax))))) 
                    (call-undead ()) 
                    (conflicts ((tmp.190 (tmp.35 rbp tmp-ra.326)) (tmp.35 (tmp.189 tmp.190 rbp tmp-ra.326)) (tmp-ra.326 (rax tmp.189 tmp.190 tmp.35 rbp rdi)) (tmp.189 (rbp tmp-ra.326 tmp.35)) (rdi (tmp-ra.326)) (rbp (rax tmp.189 tmp.190 tmp.35 tmp-ra.326)) (rax (rbp tmp-ra.326)))) 
                    (assignment ())) 
                    (begin (set! tmp-ra.326 r15) (set! tmp.35 rdi) (if (begin (if (begin (set! tmp.190 tmp.35) (set! tmp.190 (bitwise-and tmp.190 7)) (= tmp.190 1)) (set! tmp.189 14) (set! tmp.189 6)) (!= tmp.189 6)) (begin (set! rax (mref tmp.35 -1)) (jump tmp-ra.326 rbp rax)) (begin (set! rax 3134) (jump tmp-ra.326 rbp rax))))) 
                (define L.cons.5 
                    ((new-frames ()) 
                    (locals (tmp.191 tmp.46 tmp.47 tmp.69 tmp-ra.327)) 
                    (undead-out ((rdi rsi r12 tmp-ra.327 rbp) (rsi r12 tmp.46 tmp-ra.327 rbp) (r12 tmp.46 tmp.47 tmp-ra.327 rbp) ((r12 tmp.191 tmp.46 tmp.47 tmp-ra.327 rbp) (tmp.191 tmp.46 tmp.47 tmp-ra.327 rbp)) (tmp.46 tmp.47 tmp.69 tmp-ra.327 rbp) (tmp.46 tmp.47 tmp.69 tmp-ra.327 rbp) (tmp.47 tmp.69 tmp-ra.327 rbp) (tmp.69 tmp-ra.327 rbp) (tmp-ra.327 rbp rax) (rbp rax))) 
                    (call-undead ()) 
                    (conflicts ((tmp.191 (r12 rbp tmp-ra.327 tmp.47 tmp.46)) (tmp.46 (tmp.69 tmp.191 tmp.47 rbp tmp-ra.327 r12 rsi)) (tmp.47 (tmp.69 tmp.191 rbp tmp-ra.327 tmp.46 r12)) (tmp.69 (rbp tmp-ra.327 tmp.47 tmp.46)) (tmp-ra.327 (rax tmp.69 tmp.191 tmp.47 tmp.46 rbp r12 rsi rdi)) (rdi (tmp-ra.327)) (rsi (tmp.46 tmp-ra.327)) (r12 (rbp tmp.191 tmp.47 tmp.46 tmp-ra.327)) (rbp (rax tmp.69 r12 tmp.191 tmp.47 tmp.46 tmp-ra.327)) (rax (rbp tmp-ra.327)))) 
                    (assignment ())) 
                    (begin (set! tmp-ra.327 r15) (set! tmp.46 rdi) (set! tmp.47 rsi) (begin (set! tmp.191 r12) (set! r12 (+ r12 16))) (set! tmp.69 tmp.191) (set! tmp.69 (+ tmp.69 1)) (mset! tmp.69 -1 tmp.46) (mset! tmp.69 7 tmp.47) (set! rax tmp.69) (jump tmp-ra.327 rbp rax))) 
                    (begin (set! tmp-ra.325 r15) (return-point L.rp.28 (begin (set! rsi 22) (set! rdi 56) (set! r15 L.rp.28) (jump L.cons.5 rbp r15 rdi rsi))) (set! tmp.192 rax) (set! rdi tmp.192) (set! r15 tmp-ra.325) (jump L.car.6 rbp r15 rdi))))

     `(module
        ((locals (tmp.192))
        (conflicts
            ((tmp-ra.325 (rdi tmp.192 rbp))
            (tmp.192 (rbp tmp-ra.325))
            (rbp (tmp.192 r15 rdi rsi tmp-ra.325))
            (rsi (r15 rdi rbp))
            (rdi (tmp-ra.325 r15 rsi rbp))
            (r15 (rsi rdi rbp))))
        (assignment ((tmp-ra.325 fv0))))
        (define L.car.6
            ((locals (tmp.189 tmp-ra.326 tmp.35 tmp.190))
            (conflicts
            ((tmp.190 (tmp.35 rbp tmp-ra.326))
            (tmp.35 (tmp.189 tmp.190 rbp tmp-ra.326))
            (tmp-ra.326 (rax tmp.189 tmp.190 tmp.35 rbp rdi))
            (tmp.189 (rbp tmp-ra.326 tmp.35))
            (rdi (tmp-ra.326))
            (rbp (rax tmp.189 tmp.190 tmp.35 tmp-ra.326))
            (rax (rbp tmp-ra.326))))
            (assignment ()))
            (begin
            (set! tmp-ra.326 r15)
            (set! tmp.35 rdi)
            (if (begin
                    (if (begin
                        (set! tmp.190 tmp.35)
                        (set! tmp.190 (bitwise-and tmp.190 7))
                        (= tmp.190 1))
                    (set! tmp.189 14)
                    (set! tmp.189 6))
                    (!= tmp.189 6))
                (begin (set! rax (mref tmp.35 -1)) (jump tmp-ra.326 rbp rax))
                (begin (set! rax 3134) (jump tmp-ra.326 rbp rax)))))
        (define L.cons.5
            ((locals (tmp-ra.327 tmp.69 tmp.47 tmp.46 tmp.191))
            (conflicts
            ((tmp.191 (r12 rbp tmp-ra.327 tmp.47 tmp.46))
            (tmp.46 (tmp.69 tmp.191 tmp.47 rbp tmp-ra.327 r12 rsi))
            (tmp.47 (tmp.69 tmp.191 rbp tmp-ra.327 tmp.46 r12))
            (tmp.69 (rbp tmp-ra.327 tmp.47 tmp.46))
            (tmp-ra.327 (rax tmp.69 tmp.191 tmp.47 tmp.46 rbp r12 rsi rdi))
            (rdi (tmp-ra.327))
            (rsi (tmp.46 tmp-ra.327))
            (r12 (rbp tmp.191 tmp.47 tmp.46 tmp-ra.327))
            (rbp (rax tmp.69 r12 tmp.191 tmp.47 tmp.46 tmp-ra.327))
            (rax (rbp tmp-ra.327))))
            (assignment ()))
            (begin
            (set! tmp-ra.327 r15)
            (set! tmp.46 rdi)
            (set! tmp.47 rsi)
            (begin (set! tmp.191 r12) (set! r12 (+ r12 16)))
            (set! tmp.69 tmp.191)
            (set! tmp.69 (+ tmp.69 1))
            (mset! tmp.69 -1 tmp.46)
            (mset! tmp.69 7 tmp.47)
            (set! rax tmp.69)
            (jump tmp-ra.327 rbp rax)))
        (begin
            (set! tmp-ra.325 r15)
            (begin
            (set! rbp (- rbp 8))
            (return-point
            L.rp.28
            (begin
                (set! rsi 22)
                (set! rdi 56)
                (set! r15 L.rp.28)
                (jump L.cons.5 rbp r15 rdi rsi)))
            (set! rbp (+ rbp 8)))
            (set! tmp.192 rax)
            (set! rdi tmp.192)
            (set! r15 tmp-ra.325)
            (jump L.car.6 rbp r15 rdi)))))




