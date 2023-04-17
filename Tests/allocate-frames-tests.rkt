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

(test-case "allocate-14 - stack smash"
    (check-match
        (allocate-frames
            `(module 
                ((new-frames ()) 
                 (locals (tmp-ra.426)) 
                 (call-undead ()) 
                 (undead-out ((tmp-ra.426 rbp) (tmp-ra.426 rbp fv0) (tmp-ra.426 rbp r9 fv0) (tmp-ra.426 rbp r8 r9 fv0) (tmp-ra.426 rbp rcx r8 r9 fv0) (tmp-ra.426 rbp rdx rcx r8 r9 fv0) (tmp-ra.426 rbp rsi rdx rcx r8 r9 fv0) (tmp-ra.426 rbp rdi rsi rdx rcx r8 r9 fv0) (rbp r15 rdi rsi rdx rcx r8 r9 fv0) (rbp r15 rdi rsi rdx rcx r8 r9 fv0))) 
                 (conflicts ((tmp-ra.426 (rdi rsi rdx rcx r8 r9 fv0 rbp)) (rbp (r15 rdi rsi rdx rcx r8 r9 fv0 tmp-ra.426)) (fv0 (r15 rdi rsi rdx rcx r8 r9 rbp tmp-ra.426)) (r9 (r15 rdi rsi rdx rcx r8 fv0 rbp tmp-ra.426)) (r8 (r15 rdi rsi rdx rcx fv0 r9 rbp tmp-ra.426)) (rcx (r15 rdi rsi rdx fv0 r9 r8 rbp tmp-ra.426)) (rdx (r15 rdi rsi fv0 r9 r8 rcx rbp tmp-ra.426)) (rsi (r15 rdi fv0 r9 r8 rcx rdx rbp tmp-ra.426)) (rdi (r15 fv0 r9 r8 rcx rdx rsi rbp tmp-ra.426)) (r15 (fv0 r9 r8 rcx rdx rsi rdi rbp)))) 
                 (assignment ())) 
                (define L.+.5 
                    ((new-frames ()) 
                     (locals (tmp-ra.427 tmp.17 tmp.16 tmp.122 tmp.121 tmp.124 tmp.123)) 
                     (undead-out ((rdi rsi tmp-ra.427 rbp) (rsi tmp.16 tmp-ra.427 rbp) (tmp.16 tmp.17 tmp-ra.427 rbp) ((((((tmp.122 tmp.16 tmp.17 tmp-ra.427 rbp) (tmp.122 tmp.16 tmp.17 tmp-ra.427 rbp)) (tmp.16 tmp.17 tmp-ra.427 rbp)) (tmp.121 tmp.16 tmp.17 tmp-ra.427 rbp) (tmp.121 tmp.16 tmp.17 tmp-ra.427 rbp)) (tmp.16 tmp.17 tmp-ra.427 rbp)) ((((((tmp.124 tmp.16 tmp.17 tmp-ra.427 rbp) (tmp.124 tmp.16 tmp.17 tmp-ra.427 rbp)) (tmp.16 tmp.17 tmp-ra.427 rbp)) (tmp.123 tmp.16 tmp.17 tmp-ra.427 rbp) (tmp.123 tmp.16 tmp.17 tmp-ra.427 rbp)) (tmp.16 tmp.17 tmp-ra.427 rbp)) ((tmp.17 tmp-ra.427 rbp rax) (tmp-ra.427 rbp rax) (rbp rax)) ((tmp-ra.427 rbp rax) (rbp rax))) ((tmp-ra.427 rbp rax) (rbp rax))))) 
                     (call-undead ()) 
                     (conflicts ((tmp-ra.427 (rax tmp.123 tmp.124 tmp.121 tmp.122 tmp.17 tmp.16 rbp rsi rdi)) (tmp.17 (rax tmp.123 tmp.124 tmp.121 tmp.122 rbp tmp-ra.427 tmp.16)) (tmp.16 (tmp.123 tmp.124 tmp.121 tmp.122 tmp.17 rbp tmp-ra.427 rsi)) (tmp.122 (tmp.17 rbp tmp-ra.427 tmp.16)) (tmp.121 (rbp tmp-ra.427 tmp.17 tmp.16)) (tmp.124 (tmp.16 rbp tmp-ra.427 tmp.17)) (tmp.123 (rbp tmp-ra.427 tmp.17 tmp.16)) (rdi (tmp-ra.427)) (rsi (tmp.16 tmp-ra.427)) (rbp (rax tmp.123 tmp.124 tmp.121 tmp.122 tmp.17 tmp.16 tmp-ra.427)) (rax (rbp tmp-ra.427 tmp.17)))) 
                     (assignment ())) 
                     (begin (set! tmp-ra.427 r15) (set! tmp.16 rdi) (set! tmp.17 rsi) (if (begin (if (begin (begin (set! tmp.122 tmp.17) (set! tmp.122 (bitwise-and tmp.122 7))) (= tmp.122 0)) (set! tmp.121 14) (set! tmp.121 6)) (!= tmp.121 6)) (if (begin (if (begin (begin (set! tmp.124 tmp.16) (set! tmp.124 (bitwise-and tmp.124 7))) (= tmp.124 0)) (set! tmp.123 14) (set! tmp.123 6)) (!= tmp.123 6)) (begin (set! rax tmp.16) (set! rax (+ rax tmp.17)) (jump tmp-ra.427 rbp rax)) (begin (set! rax 574) (jump tmp-ra.427 rbp rax))) (begin (set! rax 574) (jump tmp-ra.427 rbp rax))))) 
                (define L.F.6 
                    ((new-frames ((nfv.429 nfv.430))) 
                     (locals (b.19 nfv.430 d.21 g.24 tmp.125 nfv.429 a.18 f.23 c.20 e.22)) 
                     (undead-out ((rdi rsi rdx rcx r8 r9 fv0 tmp-ra.428 rbp) (rsi rdx rcx r8 r9 fv0 tmp-ra.428 a.18 rbp) (rdx rcx r8 r9 fv0 tmp-ra.428 b.19 a.18 rbp) (rcx r8 r9 fv0 tmp-ra.428 c.20 b.19 a.18 rbp) (r8 r9 fv0 tmp-ra.428 d.21 c.20 b.19 a.18 rbp) (r9 fv0 tmp-ra.428 e.22 d.21 c.20 b.19 a.18 rbp) (fv0 tmp-ra.428 f.23 e.22 d.21 c.20 b.19 a.18 rbp) (tmp-ra.428 g.24 f.23 e.22 d.21 c.20 b.19 a.18 rbp) ((rax tmp-ra.428 rbp) ((g.24 f.23 e.22 d.21 c.20 b.19 a.18 rbp nfv.430) (f.23 e.22 d.21 c.20 b.19 a.18 rbp nfv.429 nfv.430) (e.22 d.21 c.20 b.19 a.18 rbp r9 nfv.429 nfv.430) (d.21 c.20 b.19 a.18 rbp r8 r9 nfv.429 nfv.430) (c.20 b.19 a.18 rbp rcx r8 r9 nfv.429 nfv.430) (b.19 a.18 rbp rdx rcx r8 r9 nfv.429 nfv.430) (a.18 rbp rsi rdx rcx r8 r9 nfv.429 nfv.430) (rbp rdi rsi rdx rcx r8 r9 nfv.429 nfv.430) (rbp r15 rdi rsi rdx rcx r8 r9 nfv.429 nfv.430) (rbp r15 rdi rsi rdx rcx r8 r9 nfv.429 nfv.430))) (tmp.125 tmp-ra.428 rbp) (tmp-ra.428 rbp rsi) (tmp-ra.428 rbp rdi rsi) (rbp r15 rdi rsi) (rbp r15 rdi rsi))) 
                     (call-undead (tmp-ra.428)) 
                     (conflicts ((tmp-ra.428 (tmp.125 g.24 f.23 e.22 d.21 c.20 b.19 a.18 rbp fv0 r9 r8 rcx rdx rsi rdi)) (b.19 (nfv.429 nfv.430 g.24 f.23 e.22 d.21 c.20 rbp a.18 tmp-ra.428 fv0 r9 r8 rcx rdx)) (nfv.430 (r15 rdi rsi rdx rcx r8 r9 nfv.429 rbp a.18 b.19 c.20 d.21 e.22 f.23 g.24)) (d.21 (nfv.429 nfv.430 g.24 f.23 e.22 rbp a.18 b.19 c.20 tmp-ra.428 fv0 r9 r8)) (g.24 (nfv.430 rbp a.18 b.19 c.20 d.21 e.22 f.23 tmp-ra.428)) (tmp.125 (rbp tmp-ra.428)) (nfv.429 (r15 rdi rsi rdx rcx r8 r9 nfv.430 rbp a.18 b.19 c.20 d.21 e.22 f.23)) (a.18 (nfv.429 nfv.430 g.24 f.23 e.22 d.21 c.20 b.19 rbp tmp-ra.428 fv0 r9 r8 rcx rdx rsi)) (f.23 (nfv.429 nfv.430 g.24 rbp a.18 b.19 c.20 d.21 e.22 tmp-ra.428 fv0)) (c.20 (nfv.429 nfv.430 g.24 f.23 e.22 d.21 rbp a.18 b.19 tmp-ra.428 fv0 r9 r8 rcx)) (e.22 (nfv.429 nfv.430 g.24 f.23 rbp a.18 b.19 c.20 d.21 tmp-ra.428 fv0 r9)) (rdi (r15 nfv.430 nfv.429 r9 r8 rcx rdx rsi rbp tmp-ra.428)) (rsi (r15 rdi nfv.430 nfv.429 r9 r8 rcx rdx rbp a.18 tmp-ra.428)) (rdx (r15 rdi rsi nfv.430 nfv.429 r9 r8 rcx rbp b.19 a.18 tmp-ra.428)) (rcx (r15 rdi rsi rdx nfv.430 nfv.429 r9 r8 rbp c.20 b.19 a.18 tmp-ra.428)) (r8 (r15 rdi rsi rdx rcx nfv.430 nfv.429 r9 rbp d.21 c.20 b.19 a.18 tmp-ra.428)) (r9 (r15 rdi rsi rdx rcx r8 nfv.430 nfv.429 rbp e.22 d.21 c.20 b.19 a.18 tmp-ra.428)) (fv0 (f.23 e.22 d.21 c.20 b.19 a.18 tmp-ra.428)) (rbp (tmp.125 r15 rdi rsi rdx rcx r8 r9 nfv.429 nfv.430 g.24 f.23 e.22 d.21 c.20 b.19 a.18 tmp-ra.428)) (r15 (nfv.430 nfv.429 r9 r8 rcx rdx rsi rdi rbp)))) 
                     (assignment ((tmp-ra.428 fv0)))) 
                     (begin (set! tmp-ra.428 r15) (set! a.18 rdi) (set! b.19 rsi) (set! c.20 rdx) (set! d.21 rcx) (set! e.22 r8) (set! f.23 r9) (set! g.24 fv0) (return-point L.rp.60 (begin (set! nfv.430 64) (set! nfv.429 g.24) (set! r9 f.23) (set! r8 e.22) (set! rcx d.21) (set! rdx c.20) (set! rsi b.19) (set! rdi a.18) (set! r15 L.rp.60) (jump L.G.7 rbp r15 rdi rsi rdx rcx r8 r9 nfv.429 nfv.430))) (set! tmp.125 rax) (set! rsi tmp.125) (set! rdi 80) (set! r15 tmp-ra.428) (jump L.+.5 rbp r15 rdi rsi))) 
                (define L.G.7 
                    ((new-frames ()) 
                     (locals (e.29 c.27 a.25 tmp-ra.431 d.28 f.30 g.31 h.32 b.26)) 
                     (undead-out ((rdi rsi rdx rcx r8 r9 fv0 fv1 tmp-ra.431 rbp) (rsi rdx rcx r8 r9 fv0 fv1 a.25 tmp-ra.431 rbp) (rdx rcx r8 r9 fv0 fv1 b.26 a.25 tmp-ra.431 rbp) (rcx r8 r9 fv0 fv1 c.27 b.26 a.25 tmp-ra.431 rbp) (r8 r9 fv0 fv1 d.28 c.27 b.26 a.25 tmp-ra.431 rbp) (r9 fv0 fv1 e.29 d.28 c.27 b.26 a.25 tmp-ra.431 rbp) (fv0 fv1 f.30 e.29 d.28 c.27 b.26 a.25 tmp-ra.431 rbp) (fv1 g.31 f.30 e.29 d.28 c.27 b.26 a.25 tmp-ra.431 rbp) (h.32 g.31 f.30 e.29 d.28 c.27 b.26 a.25 tmp-ra.431 rbp) (h.32 g.31 f.30 e.29 d.28 c.27 b.26 a.25 tmp-ra.431 rbp fv2) (g.31 f.30 e.29 d.28 c.27 b.26 a.25 tmp-ra.431 rbp fv1 fv2) (f.30 e.29 d.28 c.27 b.26 a.25 tmp-ra.431 rbp fv0 fv1 fv2) (e.29 d.28 c.27 b.26 a.25 tmp-ra.431 rbp r9 fv0 fv1 fv2) (d.28 c.27 b.26 a.25 tmp-ra.431 rbp r8 r9 fv0 fv1 fv2) (c.27 b.26 a.25 tmp-ra.431 rbp rcx r8 r9 fv0 fv1 fv2) (b.26 a.25 tmp-ra.431 rbp rdx rcx r8 r9 fv0 fv1 fv2) (a.25 tmp-ra.431 rbp rsi rdx rcx r8 r9 fv0 fv1 fv2) (tmp-ra.431 rbp rdi rsi rdx rcx r8 r9 fv0 fv1 fv2) (rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2) (rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2))) 
                     (call-undead ()) 
                     (conflicts ((e.29 (fv2 h.32 g.31 f.30 rbp tmp-ra.431 a.25 b.26 c.27 d.28 fv1 fv0 r9)) (c.27 (fv2 h.32 g.31 f.30 e.29 d.28 rbp tmp-ra.431 a.25 b.26 fv1 fv0 r9 r8 rcx)) (a.25 (fv2 h.32 g.31 f.30 e.29 d.28 c.27 b.26 rbp tmp-ra.431 fv1 fv0 r9 r8 rcx rdx rsi)) (tmp-ra.431 (fv2 h.32 g.31 f.30 e.29 d.28 c.27 b.26 a.25 rbp fv1 fv0 r9 r8 rcx rdx rsi rdi)) (d.28 (fv2 h.32 g.31 f.30 e.29 rbp tmp-ra.431 a.25 b.26 c.27 fv1 fv0 r9 r8)) (f.30 (fv2 h.32 g.31 rbp tmp-ra.431 a.25 b.26 c.27 d.28 e.29 fv1 fv0)) (g.31 (fv2 h.32 rbp tmp-ra.431 a.25 b.26 c.27 d.28 e.29 f.30 fv1)) (h.32 (fv2 rbp tmp-ra.431 a.25 b.26 c.27 d.28 e.29 f.30 g.31)) (b.26 (fv2 h.32 g.31 f.30 e.29 d.28 c.27 rbp tmp-ra.431 a.25 fv1 fv0 r9 r8 rcx rdx)) (rdi (r15 fv2 fv1 fv0 r9 r8 rcx rdx rsi rbp tmp-ra.431)) (rsi (r15 rdi fv2 fv1 fv0 r9 r8 rcx rdx rbp a.25 tmp-ra.431)) (rdx (r15 rdi rsi fv2 fv1 fv0 r9 r8 rcx rbp b.26 a.25 tmp-ra.431)) (rcx (r15 rdi rsi rdx fv2 fv1 fv0 r9 r8 rbp c.27 b.26 a.25 tmp-ra.431)) (r8 (r15 rdi rsi rdx rcx fv2 fv1 fv0 r9 rbp d.28 c.27 b.26 a.25 tmp-ra.431)) (r9 (r15 rdi rsi rdx rcx r8 fv2 fv1 fv0 rbp e.29 d.28 c.27 b.26 a.25 tmp-ra.431)) (fv0 (r15 rdi rsi rdx rcx r8 r9 fv2 fv1 rbp f.30 e.29 d.28 c.27 b.26 a.25 tmp-ra.431)) (fv1 (r15 rdi rsi rdx rcx r8 r9 fv0 fv2 rbp g.31 f.30 e.29 d.28 c.27 b.26 a.25 tmp-ra.431)) (rbp (r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2 h.32 g.31 f.30 e.29 d.28 c.27 b.26 a.25 tmp-ra.431)) (fv2 (r15 rdi rsi rdx rcx r8 r9 fv0 fv1 rbp tmp-ra.431 a.25 b.26 c.27 d.28 e.29 f.30 g.31 h.32)) (r15 (fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp)))) 
                     (assignment ())) 
                     (begin (set! tmp-ra.431 r15) (set! a.25 rdi) (set! b.26 rsi) (set! c.27 rdx) (set! d.28 rcx) (set! e.29 r8) (set! f.30 r9) (set! g.31 fv0) (set! h.32 fv1) (set! fv2 72) (set! fv1 h.32) (set! fv0 g.31) (set! r9 f.30) (set! r8 e.29) (set! rcx d.28) (set! rdx c.27) (set! rsi b.26) (set! rdi a.25) (set! r15 tmp-ra.431) (jump L.H.8 rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2))) 
                (define L.H.8 
                    ((new-frames (() () () () () () ())) 
                     (locals (r5.46 r6.47 r7.48 r4.45 a.33 r2.43 r3.44 r1.42 b.34)) 
                     (undead-out ((rdi rsi rdx rcx r8 r9 fv0 fv1 fv2 tmp-ra.432 rbp) (rsi rdx rcx r8 r9 fv0 fv1 fv2 tmp-ra.432 a.33 rbp) (rdx rcx r8 r9 fv0 fv1 fv2 tmp-ra.432 b.34 a.33 rbp) (rcx r8 r9 fv0 fv1 fv2 c.35 tmp-ra.432 b.34 a.33 rbp) (r8 r9 fv0 fv1 fv2 c.35 tmp-ra.432 d.36 b.34 a.33 rbp) (r9 fv0 fv1 fv2 c.35 e.37 tmp-ra.432 d.36 b.34 a.33 rbp) (fv0 fv1 fv2 c.35 e.37 tmp-ra.432 f.38 d.36 b.34 a.33 rbp) (fv1 fv2 c.35 e.37 g.39 tmp-ra.432 f.38 d.36 b.34 a.33 rbp) (fv2 c.35 e.37 g.39 tmp-ra.432 h.40 f.38 d.36 b.34 a.33 rbp) (c.35 e.37 g.39 tmp-ra.432 j.41 h.40 f.38 d.36 b.34 a.33 rbp) ((rax d.36 f.38 h.40 j.41 tmp-ra.432 g.39 e.37 c.35 rbp) ((a.33 rbp rsi) (rbp rdi rsi) (rbp r15 rdi rsi) (rbp r15 rdi rsi))) (d.36 f.38 h.40 j.41 tmp-ra.432 g.39 e.37 c.35 r1.42 rbp) ((rax e.37 g.39 tmp-ra.432 j.41 h.40 f.38 d.36 rbp) ((r1.42 rbp rsi) (rbp rdi rsi) (rbp r15 rdi rsi) (rbp r15 rdi rsi))) (e.37 g.39 tmp-ra.432 j.41 h.40 f.38 d.36 r2.43 rbp) ((rax f.38 h.40 j.41 tmp-ra.432 g.39 e.37 rbp) ((r2.43 rbp rsi) (rbp rdi rsi) (rbp r15 rdi rsi) (rbp r15 rdi rsi))) (f.38 h.40 j.41 tmp-ra.432 g.39 e.37 r3.44 rbp) ((rax g.39 tmp-ra.432 j.41 h.40 f.38 rbp) ((r3.44 rbp rsi) (rbp rdi rsi) (rbp r15 rdi rsi) (rbp r15 rdi rsi))) (g.39 tmp-ra.432 j.41 h.40 f.38 r4.45 rbp) ((rax h.40 j.41 tmp-ra.432 g.39 rbp) ((r4.45 rbp rsi) (rbp rdi rsi) (rbp r15 rdi rsi) (rbp r15 rdi rsi))) (h.40 j.41 tmp-ra.432 g.39 r5.46 rbp) ((rax tmp-ra.432 j.41 h.40 rbp) ((r5.46 rbp rsi) (rbp rdi rsi) (rbp r15 rdi rsi) (rbp r15 rdi rsi))) (tmp-ra.432 j.41 h.40 r6.47 rbp) ((rax j.41 tmp-ra.432 rbp) ((r6.47 rbp rsi) (rbp rdi rsi) (rbp r15 rdi rsi) (rbp r15 rdi rsi))) (j.41 r7.48 tmp-ra.432 rbp) (r7.48 tmp-ra.432 rbp rsi) (tmp-ra.432 rbp rdi rsi) (rbp r15 rdi rsi) (rbp r15 rdi rsi))) 
                     (call-undead (tmp-ra.432 f.38 h.40 d.36 g.39 e.37 j.41 c.35)) 
                     (conflicts ((r5.46 (rsi rbp g.39 tmp-ra.432 j.41 h.40)) (c.35 (r1.42 j.41 h.40 g.39 f.38 e.37 d.36 rbp a.33 b.34 tmp-ra.432 fv2 fv1 fv0 r9 r8 rcx)) (r6.47 (rsi rbp h.40 j.41 tmp-ra.432)) (g.39 (r5.46 r4.45 r3.44 r2.43 r1.42 j.41 h.40 rbp a.33 b.34 d.36 f.38 tmp-ra.432 e.37 c.35 fv2 fv1)) (tmp-ra.432 (r7.48 r6.47 r5.46 r4.45 r3.44 r2.43 r1.42 j.41 h.40 g.39 f.38 e.37 d.36 c.35 b.34 a.33 rbp fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi)) (d.36 (r2.43 r1.42 j.41 h.40 g.39 f.38 e.37 rbp a.33 b.34 tmp-ra.432 c.35 fv2 fv1 fv0 r9 r8)) (f.38 (r4.45 r3.44 r2.43 r1.42 j.41 h.40 g.39 rbp a.33 b.34 d.36 tmp-ra.432 e.37 c.35 fv2 fv1 fv0)) (r7.48 (rsi rbp tmp-ra.432 j.41)) (r4.45 (rsi rbp f.38 h.40 j.41 tmp-ra.432 g.39)) (a.33 (j.41 h.40 g.39 f.38 e.37 d.36 c.35 b.34 rbp tmp-ra.432 fv2 fv1 fv0 r9 r8 rcx rdx rsi)) (e.37 (r3.44 r2.43 r1.42 j.41 h.40 g.39 f.38 rbp a.33 b.34 d.36 tmp-ra.432 c.35 fv2 fv1 fv0 r9)) (r2.43 (rsi rbp d.36 f.38 h.40 j.41 tmp-ra.432 g.39 e.37)) (r3.44 (rsi rbp e.37 g.39 tmp-ra.432 j.41 h.40 f.38)) (r1.42 (rsi rbp c.35 e.37 g.39 tmp-ra.432 j.41 h.40 f.38 d.36)) (h.40 (r6.47 r5.46 r4.45 r3.44 r2.43 r1.42 j.41 rbp a.33 b.34 d.36 f.38 tmp-ra.432 g.39 e.37 c.35 fv2)) (j.41 (r7.48 r6.47 r5.46 r4.45 r3.44 r2.43 r1.42 rbp a.33 b.34 d.36 f.38 h.40 tmp-ra.432 g.39 e.37 c.35)) (b.34 (j.41 h.40 g.39 f.38 e.37 d.36 c.35 rbp a.33 tmp-ra.432 fv2 fv1 fv0 r9 r8 rcx rdx)) (rdi (r15 rsi rbp tmp-ra.432)) (rsi (r7.48 r6.47 r5.46 r4.45 r3.44 r2.43 r1.42 r15 rdi rbp a.33 tmp-ra.432)) (rdx (b.34 a.33 tmp-ra.432)) (rcx (c.35 b.34 a.33 tmp-ra.432)) (r8 (d.36 c.35 b.34 a.33 tmp-ra.432)) (r9 (e.37 d.36 c.35 b.34 a.33 tmp-ra.432)) (fv0 (f.38 e.37 d.36 c.35 b.34 a.33 tmp-ra.432)) (fv1 (g.39 f.38 e.37 d.36 c.35 b.34 a.33 tmp-ra.432)) (fv2 (h.40 g.39 f.38 e.37 d.36 c.35 b.34 a.33 tmp-ra.432)) (rbp (r7.48 r6.47 r5.46 r4.45 r3.44 r2.43 r1.42 r15 rdi rsi j.41 h.40 g.39 f.38 e.37 d.36 c.35 b.34 a.33 tmp-ra.432)) (r15 (rsi rdi rbp)))) 
                     (assignment ((c.35 fv0) (j.41 fv1) (e.37 fv3) (g.39 fv4) (d.36 fv5) (h.40 fv6) (f.38 fv7) (tmp-ra.432 fv8)))) 
                     (begin (set! tmp-ra.432 r15) (set! a.33 rdi) (set! b.34 rsi) (set! c.35 rdx) (set! d.36 rcx) (set! e.37 r8) (set! f.38 r9) (set! g.39 fv0) (set! h.40 fv1) (set! j.41 fv2) (return-point L.rp.61 (begin (set! rsi b.34) (set! rdi a.33) (set! r15 L.rp.61) (jump L.+.5 rbp r15 rdi rsi))) (set! r1.42 rax) (return-point L.rp.62 (begin (set! rsi c.35) (set! rdi r1.42) (set! r15 L.rp.62) (jump L.+.5 rbp r15 rdi rsi))) (set! r2.43 rax) (return-point L.rp.63 (begin (set! rsi d.36) (set! rdi r2.43) (set! r15 L.rp.63) (jump L.+.5 rbp r15 rdi rsi))) (set! r3.44 rax) (return-point L.rp.64 (begin (set! rsi e.37) (set! rdi r3.44) (set! r15 L.rp.64) (jump L.+.5 rbp r15 rdi rsi))) (set! r4.45 rax) (return-point L.rp.65 (begin (set! rsi f.38) (set! rdi r4.45) (set! r15 L.rp.65) (jump L.+.5 rbp r15 rdi rsi))) (set! r5.46 rax) (return-point L.rp.66 (begin (set! rsi g.39) (set! rdi r5.46) (set! r15 L.rp.66) (jump L.+.5 rbp r15 rdi rsi))) (set! r6.47 rax) (return-point L.rp.67 (begin (set! rsi h.40) (set! rdi r6.47) (set! r15 L.rp.67) (jump L.+.5 rbp r15 rdi rsi))) (set! r7.48 rax) (set! rsi j.41) (set! rdi r7.48) (set! r15 tmp-ra.432) (jump L.+.5 rbp r15 rdi rsi))) 
                (begin (set! tmp-ra.426 r15) (set! fv0 56) (set! r9 48) (set! r8 40) (set! rcx 32) (set! rdx 24) (set! rsi 16) (set! rdi 8) (set! r15 tmp-ra.426) (jump L.F.6 rbp r15 rdi rsi rdx rcx r8 r9 fv0))))
        `(module
            ((locals (tmp-ra.426))
            (conflicts
                ((tmp-ra.426 (rdi rsi rdx rcx r8 r9 fv0 rbp))
                (rbp (r15 rdi rsi rdx rcx r8 r9 fv0 tmp-ra.426))
                (fv0 (r15 rdi rsi rdx rcx r8 r9 rbp tmp-ra.426))
                (r9 (r15 rdi rsi rdx rcx r8 fv0 rbp tmp-ra.426))
                (r8 (r15 rdi rsi rdx rcx fv0 r9 rbp tmp-ra.426))
                (rcx (r15 rdi rsi rdx fv0 r9 r8 rbp tmp-ra.426))
                (rdx (r15 rdi rsi fv0 r9 r8 rcx rbp tmp-ra.426))
                (rsi (r15 rdi fv0 r9 r8 rcx rdx rbp tmp-ra.426))
                (rdi (r15 fv0 r9 r8 rcx rdx rsi rbp tmp-ra.426))
                (r15 (fv0 r9 r8 rcx rdx rsi rdi rbp))))
            (assignment ()))
            (define L.+.5
                ((locals (tmp.123 tmp.124 tmp.121 tmp.122 tmp.16 tmp.17 tmp-ra.427))
                (conflicts
                ((tmp-ra.427
                    (rax tmp.123 tmp.124 tmp.121 tmp.122 tmp.17 tmp.16 rbp rsi rdi))
                (tmp.17 (rax tmp.123 tmp.124 tmp.121 tmp.122 rbp tmp-ra.427 tmp.16))
                (tmp.16 (tmp.123 tmp.124 tmp.121 tmp.122 tmp.17 rbp tmp-ra.427 rsi))
                (tmp.122 (tmp.17 rbp tmp-ra.427 tmp.16))
                (tmp.121 (rbp tmp-ra.427 tmp.17 tmp.16))
                (tmp.124 (tmp.16 rbp tmp-ra.427 tmp.17))
                (tmp.123 (rbp tmp-ra.427 tmp.17 tmp.16))
                (rdi (tmp-ra.427))
                (rsi (tmp.16 tmp-ra.427))
                (rbp (rax tmp.123 tmp.124 tmp.121 tmp.122 tmp.17 tmp.16 tmp-ra.427))
                (rax (rbp tmp-ra.427 tmp.17))))
                (assignment ()))
                (begin
                (set! tmp-ra.427 r15)
                (set! tmp.16 rdi)
                (set! tmp.17 rsi)
                (if (begin
                        (if (begin
                            (begin
                                (set! tmp.122 tmp.17)
                                (set! tmp.122 (bitwise-and tmp.122 7)))
                            (= tmp.122 0))
                        (set! tmp.121 14)
                        (set! tmp.121 6))
                        (!= tmp.121 6))
                    (if (begin
                        (if (begin
                                (begin
                                (set! tmp.124 tmp.16)
                                (set! tmp.124 (bitwise-and tmp.124 7)))
                                (= tmp.124 0))
                            (set! tmp.123 14)
                            (set! tmp.123 6))
                        (!= tmp.123 6))
                    (begin
                        (set! rax tmp.16)
                        (set! rax (+ rax tmp.17))
                        (jump tmp-ra.427 rbp rax))
                    (begin (set! rax 574) (jump tmp-ra.427 rbp rax)))
                    (begin (set! rax 574) (jump tmp-ra.427 rbp rax)))))
            (define L.F.6
                ((locals (e.22 c.20 f.23 a.18 tmp.125 g.24 d.21 b.19))
                (conflicts
                ((tmp-ra.428 (tmp.125 g.24 f.23 e.22 d.21 c.20 b.19 a.18 rbp fv0 r9 r8 rcx rdx rsi rdi)) (b.19 (nfv.429 nfv.430 g.24 f.23 e.22 d.21 c.20 rbp a.18 tmp-ra.428 fv0 r9 r8 rcx rdx)) (nfv.430 (r15 rdi rsi rdx rcx r8 r9 nfv.429 rbp a.18 b.19 c.20 d.21 e.22 f.23 g.24)) (d.21 (nfv.429 nfv.430 g.24 f.23 e.22 rbp a.18 b.19 c.20 tmp-ra.428 fv0 r9 r8)) (g.24 (nfv.430 rbp a.18 b.19 c.20 d.21 e.22 f.23 tmp-ra.428)) (tmp.125 (rbp tmp-ra.428)) (nfv.429 (r15 rdi rsi rdx rcx r8 r9 nfv.430 rbp a.18 b.19 c.20 d.21 e.22 f.23)) (a.18 (nfv.429 nfv.430 g.24 f.23 e.22 d.21 c.20 b.19 rbp tmp-ra.428 fv0 r9 r8 rcx rdx rsi)) (f.23 (nfv.429 nfv.430 g.24 rbp a.18 b.19 c.20 d.21 e.22 tmp-ra.428 fv0)) (c.20 (nfv.429 nfv.430 g.24 f.23 e.22 d.21 rbp a.18 b.19 tmp-ra.428 fv0 r9 r8 rcx)) (e.22 (nfv.429 nfv.430 g.24 f.23 rbp a.18 b.19 c.20 d.21 tmp-ra.428 fv0 r9)) (rdi (r15 nfv.430 nfv.429 r9 r8 rcx rdx rsi rbp tmp-ra.428)) (rsi (r15 rdi nfv.430 nfv.429 r9 r8 rcx rdx rbp a.18 tmp-ra.428)) (rdx (r15 rdi rsi nfv.430 nfv.429 r9 r8 rcx rbp b.19 a.18 tmp-ra.428)) (rcx (r15 rdi rsi rdx nfv.430 nfv.429 r9 r8 rbp c.20 b.19 a.18 tmp-ra.428)) (r8 (r15 rdi rsi rdx rcx nfv.430 nfv.429 r9 rbp d.21 c.20 b.19 a.18 tmp-ra.428)) (r9 (r15 rdi rsi rdx rcx r8 nfv.430 nfv.429 rbp e.22 d.21 c.20 b.19 a.18 tmp-ra.428)) (fv0 (f.23 e.22 d.21 c.20 b.19 a.18 tmp-ra.428)) (rbp (tmp.125 r15 rdi rsi rdx rcx r8 r9 nfv.429 nfv.430 g.24 f.23 e.22 d.21 c.20 b.19 a.18 tmp-ra.428)) (r15 (nfv.430 nfv.429 r9 r8 rcx rdx rsi rdi rbp))))
                (assignment ((tmp-ra.428 fv0) (nfv.429 fv1) (nfv.430 fv2))))
                (begin
                (set! tmp-ra.428 r15)
                (set! a.18 rdi)
                (set! b.19 rsi)
                (set! c.20 rdx)
                (set! d.21 rcx)
                (set! e.22 r8)
                (set! f.23 r9)
                (set! g.24 fv0)
                (begin
                    (set! rbp (- rbp 8))
                    (return-point
                    L.rp.60
                    (begin
                    (set! nfv.430 64)
                    (set! nfv.429 g.24)
                    (set! r9 f.23)
                    (set! r8 e.22)
                    (set! rcx d.21)
                    (set! rdx c.20)
                    (set! rsi b.19)
                    (set! rdi a.18)
                    (set! r15 L.rp.60)
                    (jump L.G.7 rbp r15 rdi rsi rdx rcx r8 r9 nfv.429 nfv.430)))
                    (set! rbp (+ rbp 8)))
                (set! tmp.125 rax)
                (set! rsi tmp.125)
                (set! rdi 80)
                (set! r15 tmp-ra.428)
                (jump L.+.5 rbp r15 rdi rsi)))
            (define L.G.7
                ((locals (b.26 h.32 g.31 f.30 d.28 tmp-ra.431 a.25 c.27 e.29))
                (conflicts ((e.29 (fv2 h.32 g.31 f.30 rbp tmp-ra.431 a.25 b.26 c.27 d.28 fv1 fv0 r9)) (c.27 (fv2 h.32 g.31 f.30 e.29 d.28 rbp tmp-ra.431 a.25 b.26 fv1 fv0 r9 r8 rcx)) (a.25 (fv2 h.32 g.31 f.30 e.29 d.28 c.27 b.26 rbp tmp-ra.431 fv1 fv0 r9 r8 rcx rdx rsi)) (tmp-ra.431 (fv2 h.32 g.31 f.30 e.29 d.28 c.27 b.26 a.25 rbp fv1 fv0 r9 r8 rcx rdx rsi rdi)) (d.28 (fv2 h.32 g.31 f.30 e.29 rbp tmp-ra.431 a.25 b.26 c.27 fv1 fv0 r9 r8)) (f.30 (fv2 h.32 g.31 rbp tmp-ra.431 a.25 b.26 c.27 d.28 e.29 fv1 fv0)) (g.31 (fv2 h.32 rbp tmp-ra.431 a.25 b.26 c.27 d.28 e.29 f.30 fv1)) (h.32 (fv2 rbp tmp-ra.431 a.25 b.26 c.27 d.28 e.29 f.30 g.31)) (b.26 (fv2 h.32 g.31 f.30 e.29 d.28 c.27 rbp tmp-ra.431 a.25 fv1 fv0 r9 r8 rcx rdx)) (rdi (r15 fv2 fv1 fv0 r9 r8 rcx rdx rsi rbp tmp-ra.431)) (rsi (r15 rdi fv2 fv1 fv0 r9 r8 rcx rdx rbp a.25 tmp-ra.431)) (rdx (r15 rdi rsi fv2 fv1 fv0 r9 r8 rcx rbp b.26 a.25 tmp-ra.431)) (rcx (r15 rdi rsi rdx fv2 fv1 fv0 r9 r8 rbp c.27 b.26 a.25 tmp-ra.431)) (r8 (r15 rdi rsi rdx rcx fv2 fv1 fv0 r9 rbp d.28 c.27 b.26 a.25 tmp-ra.431)) (r9 (r15 rdi rsi rdx rcx r8 fv2 fv1 fv0 rbp e.29 d.28 c.27 b.26 a.25 tmp-ra.431)) (fv0 (r15 rdi rsi rdx rcx r8 r9 fv2 fv1 rbp f.30 e.29 d.28 c.27 b.26 a.25 tmp-ra.431)) (fv1 (r15 rdi rsi rdx rcx r8 r9 fv0 fv2 rbp g.31 f.30 e.29 d.28 c.27 b.26 a.25 tmp-ra.431)) (rbp (r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2 h.32 g.31 f.30 e.29 d.28 c.27 b.26 a.25 tmp-ra.431)) (fv2 (r15 rdi rsi rdx rcx r8 r9 fv0 fv1 rbp tmp-ra.431 a.25 b.26 c.27 d.28 e.29 f.30 g.31 h.32)) (r15 (fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp))))
                (assignment ()))
                (begin
                (set! tmp-ra.431 r15)
                (set! a.25 rdi)
                (set! b.26 rsi)
                (set! c.27 rdx)
                (set! d.28 rcx)
                (set! e.29 r8)
                (set! f.30 r9)
                (set! g.31 fv0)
                (set! h.32 fv1)
                (set! fv2 72)
                (set! fv1 h.32)
                (set! fv0 g.31)
                (set! r9 f.30)
                (set! r8 e.29)
                (set! rcx d.28)
                (set! rdx c.27)
                (set! rsi b.26)
                (set! rdi a.25)
                (set! r15 tmp-ra.431)
                (jump L.H.8 rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2)))
            (define L.H.8
                ((locals (b.34 r1.42 r3.44 r2.43 a.33 r4.45 r7.48 r6.47 r5.46))
                (conflicts ((r5.46 (rsi rbp g.39 tmp-ra.432 j.41 h.40)) (c.35 (r1.42 j.41 h.40 g.39 f.38 e.37 d.36 rbp a.33 b.34 tmp-ra.432 fv2 fv1 fv0 r9 r8 rcx)) (r6.47 (rsi rbp h.40 j.41 tmp-ra.432)) (g.39 (r5.46 r4.45 r3.44 r2.43 r1.42 j.41 h.40 rbp a.33 b.34 d.36 f.38 tmp-ra.432 e.37 c.35 fv2 fv1)) (tmp-ra.432 (r7.48 r6.47 r5.46 r4.45 r3.44 r2.43 r1.42 j.41 h.40 g.39 f.38 e.37 d.36 c.35 b.34 a.33 rbp fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi)) (d.36 (r2.43 r1.42 j.41 h.40 g.39 f.38 e.37 rbp a.33 b.34 tmp-ra.432 c.35 fv2 fv1 fv0 r9 r8)) (f.38 (r4.45 r3.44 r2.43 r1.42 j.41 h.40 g.39 rbp a.33 b.34 d.36 tmp-ra.432 e.37 c.35 fv2 fv1 fv0)) (r7.48 (rsi rbp tmp-ra.432 j.41)) (r4.45 (rsi rbp f.38 h.40 j.41 tmp-ra.432 g.39)) (a.33 (j.41 h.40 g.39 f.38 e.37 d.36 c.35 b.34 rbp tmp-ra.432 fv2 fv1 fv0 r9 r8 rcx rdx rsi)) (e.37 (r3.44 r2.43 r1.42 j.41 h.40 g.39 f.38 rbp a.33 b.34 d.36 tmp-ra.432 c.35 fv2 fv1 fv0 r9)) (r2.43 (rsi rbp d.36 f.38 h.40 j.41 tmp-ra.432 g.39 e.37)) (r3.44 (rsi rbp e.37 g.39 tmp-ra.432 j.41 h.40 f.38)) (r1.42 (rsi rbp c.35 e.37 g.39 tmp-ra.432 j.41 h.40 f.38 d.36)) (h.40 (r6.47 r5.46 r4.45 r3.44 r2.43 r1.42 j.41 rbp a.33 b.34 d.36 f.38 tmp-ra.432 g.39 e.37 c.35 fv2)) (j.41 (r7.48 r6.47 r5.46 r4.45 r3.44 r2.43 r1.42 rbp a.33 b.34 d.36 f.38 h.40 tmp-ra.432 g.39 e.37 c.35)) (b.34 (j.41 h.40 g.39 f.38 e.37 d.36 c.35 rbp a.33 tmp-ra.432 fv2 fv1 fv0 r9 r8 rcx rdx)) (rdi (r15 rsi rbp tmp-ra.432)) (rsi (r7.48 r6.47 r5.46 r4.45 r3.44 r2.43 r1.42 r15 rdi rbp a.33 tmp-ra.432)) (rdx (b.34 a.33 tmp-ra.432)) (rcx (c.35 b.34 a.33 tmp-ra.432)) (r8 (d.36 c.35 b.34 a.33 tmp-ra.432)) (r9 (e.37 d.36 c.35 b.34 a.33 tmp-ra.432)) (fv0 (f.38 e.37 d.36 c.35 b.34 a.33 tmp-ra.432)) (fv1 (g.39 f.38 e.37 d.36 c.35 b.34 a.33 tmp-ra.432)) (fv2 (h.40 g.39 f.38 e.37 d.36 c.35 b.34 a.33 tmp-ra.432)) (rbp (r7.48 r6.47 r5.46 r4.45 r3.44 r2.43 r1.42 r15 rdi rsi j.41 h.40 g.39 f.38 e.37 d.36 c.35 b.34 a.33 tmp-ra.432)) (r15 (rsi rdi rbp))))
                (assignment
                ((c.35 fv0)
                (j.41 fv1)
                (e.37 fv3)
                (g.39 fv4)
                (d.36 fv5)
                (h.40 fv6)
                (f.38 fv7)
                (tmp-ra.432 fv8))))
                (begin
                (set! tmp-ra.432 r15)
                (set! a.33 rdi)
                (set! b.34 rsi)
                (set! c.35 rdx)
                (set! d.36 rcx)
                (set! e.37 r8)
                (set! f.38 r9)
                (set! g.39 fv0)
                (set! h.40 fv1)
                (set! j.41 fv2)
                (begin
                    (set! rbp (- rbp 72))
                    (return-point
                    L.rp.61
                    (begin
                    (set! rsi b.34)
                    (set! rdi a.33)
                    (set! r15 L.rp.61)
                    (jump L.+.5 rbp r15 rdi rsi)))
                    (set! rbp (+ rbp 72)))
                (set! r1.42 rax)
                (begin
                    (set! rbp (- rbp 72))
                    (return-point
                    L.rp.62
                    (begin
                    (set! rsi c.35)
                    (set! rdi r1.42)
                    (set! r15 L.rp.62)
                    (jump L.+.5 rbp r15 rdi rsi)))
                    (set! rbp (+ rbp 72)))
                (set! r2.43 rax)
                (begin
                    (set! rbp (- rbp 72))
                    (return-point
                    L.rp.63
                    (begin
                    (set! rsi d.36)
                    (set! rdi r2.43)
                    (set! r15 L.rp.63)
                    (jump L.+.5 rbp r15 rdi rsi)))
                    (set! rbp (+ rbp 72)))
                (set! r3.44 rax)
                (begin
                    (set! rbp (- rbp 72))
                    (return-point
                    L.rp.64
                    (begin
                    (set! rsi e.37)
                    (set! rdi r3.44)
                    (set! r15 L.rp.64)
                    (jump L.+.5 rbp r15 rdi rsi)))
                    (set! rbp (+ rbp 72)))
                (set! r4.45 rax)
                (begin
                    (set! rbp (- rbp 72))
                    (return-point
                    L.rp.65
                    (begin
                    (set! rsi f.38)
                    (set! rdi r4.45)
                    (set! r15 L.rp.65)
                    (jump L.+.5 rbp r15 rdi rsi)))
                    (set! rbp (+ rbp 72)))
                (set! r5.46 rax)
                (begin
                    (set! rbp (- rbp 72))
                    (return-point
                    L.rp.66
                    (begin
                    (set! rsi g.39)
                    (set! rdi r5.46)
                    (set! r15 L.rp.66)
                    (jump L.+.5 rbp r15 rdi rsi)))
                    (set! rbp (+ rbp 72)))
                (set! r6.47 rax)
                (begin
                    (set! rbp (- rbp 72))
                    (return-point
                    L.rp.67
                    (begin
                    (set! rsi h.40)
                    (set! rdi r6.47)
                    (set! r15 L.rp.67)
                    (jump L.+.5 rbp r15 rdi rsi)))
                    (set! rbp (+ rbp 72)))
                (set! r7.48 rax)
                (set! rsi j.41)
                (set! rdi r7.48)
                (set! r15 tmp-ra.432)
                (jump L.+.5 rbp r15 rdi rsi)))
            (begin
                (set! tmp-ra.426 r15)
                (set! fv0 56)
                (set! r9 48)
                (set! r8 40)
                (set! rcx 32)
                (set! rdx 24)
                (set! rsi 16)
                (set! rdi 8)
                (set! r15 tmp-ra.426)
                (jump L.F.6 rbp r15 rdi rsi rdx rcx r8 r9 fv0)))))

