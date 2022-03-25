#lang racket
(require racket/draw)

; Courtyard size
(define n 7)
(define length (expt 2 n))

; 0 <= x, y < 2^n
(define holeCoords '(13 12))


; Drawing the untiled courtyard
(define target (make-bitmap (* 25 length) (* 25 length))) ; 2^n by 2^n courtyard with 25px/square grid
(define dc (new bitmap-dc% [bitmap target]))

(define (draw-grid x y)
    (send dc set-brush "black" 'transparent)
    (send dc set-pen "black" 1 'solid)
    (send dc draw-rectangle
        x y
        25 25
    )
    (cond
        [(= x (- (* 25 length) 25)) (cond 
            [( = y (- (* 25 length) 25)) #t]
            [else (draw-grid 0 (+ y 25))]
            )
        ]
        [else (draw-grid (+ x 25) y)]
    )
)

; Tiling
(define (tile holeX holeY x y size)
    ; random rgb color with all r, g, b between 25 and 225 in order to not generate close to white or black colors
    (define color (make-color (+ 25 (random 201)) (+ 25 (random 201)) (+ 25 (random 201)) 1.0))
    (define brush (make-brush #:color color #:style 'solid))
    (send dc set-brush brush)
    (cond 
    ; Hole is on the right half
    [(>= holeX (/ size 2)) (cond
        ; Hole is in bottom right
        [(>= holeY (/ size 2))
            (define x1 (+ x (* 25 (- (/ size 2) 1)))) ; Quad II bottom right
            (define y1 (+ y (* 25 (- (/ size 2) 1))))
            (define x2 (+ x (* 25 (/ size 2)))) ; Quad I bottom left
            (define y2 (+ y(* 25 (- (/ size 2) 1))))
            (define x3 (+ x (* 25 (- (/ size 2) 1)))) ; Quad III top right
            (define y3 (+ y (* 25 (/ size 2) 1)))
            (send dc draw-rectangle x1 y1 25 25)
            (send dc draw-rectangle x2 y2 25 25)
            (send dc draw-rectangle x3 y3 25 25)
            (cond
                [(= size 2) 0]
                [else (+ 
                        (tile (/ (- x2  (+ x (* 25 (/ size 2)))) 25) (/ y2 25) (+ x (* 25 (/ size 2))) y (/ size 2)) ; Quad I
                        (tile (/ x1 25) (/ y1 25) x y (/ size 2)) ; Quad II
                        (tile (/ x3 25) (/ (- y3 (+ y (* 25 (/ size 2)))) 25) x  (+ y (* 25 (/ size 2))) (/ size 2)) ; Quad III
                        (tile (- holeX (/ size 2)) (- holeY (/ size 2)) (+ x (* 25 (/ size 2))) (+ y (* 25 (/ size 2))) (/ size 2)) ; Quad IV
                )]
            )

        ]
        ; Hole is in top right
        [else 
            (define x1 (+ x (* 25 (- (/ size 2) 1)))) ; Quad II bottom right
            (define y1 (+ y (* 25 (- (/ size 2) 1))))
            (define x2 (+ x (* 25 (/ size 2)))) ; Quad IV top left
            (define y2 (+ y (* 25 (/ size 2))))
            (define x3 (+ x (* 25 (- (/ size 2) 1)))) ; Quad III top right
            (define y3 (+ y (* 25 (/ size 2) 1)))
            (send dc draw-rectangle x1 y1 25 25)
            (send dc draw-rectangle x2 y2 25 25)
            (send dc draw-rectangle x3 y3 25 25)
            (cond
                [(= size 2) 0]
                [else (+ 
                        (tile (- holeX (/ size 2)) holeY (+ x (* 25 (/ size 2))) y (/ size 2)) ; Quad I
                        (tile (/ x2 25) (/ y2 25) x y (/ size 2)) ; Quad II
                        (tile (/ x3 25) (/ (- y3 (+ y (* 25 (/ size 2)))) 25) x  (+ y (* 25 (/ size 2))) (/ size 2)) ; Quad III
                        (tile (/ (- x2 (+ x (* 25 (/ size 2)))) 25) (/ (- y2(+ y (* 25 (/ size 2)))) 25) (+ x (* 25 (/ size 2))) (+ y (* 25 (/ size 2))) (/ size 2)) ; Quad IV
                )]
            )

        ])
    ]
    ; Hole is on the left half
    [else (cond
        ; Hole is in bottom left 
        [(>= holeY (/ size 2))
            (define x1 (+ x (* 25 (- (/ size 2) 1)))) ; Quad II bottom right
            (define y1 (+ y (* 25 (- (/ size 2) 1))))
            (define x2 (+ x (* 25 (/ size 2)))) ; Quad IV top left
            (define y2 (+ y (* 25 (/ size 2))))
            (define x3 (+ x (* 25 (/ size 2)))) ; Quad I bottom left
            (define y3 (+ y (* 25 (- (/ size 2) 1))))
            (send dc draw-rectangle x1 y1 25 25)
            (send dc draw-rectangle x2 y2 25 25)
            (send dc draw-rectangle x3 y3 25 25)
            (cond
                [(= size 2) 0]
                [else (+ 
                        (tile (/ (- x3  (+ x (* 25 (/ size 2)))) 25) (/ y3 25) (+ x (* 25 (/ size 2))) y (/ size 2)) ; Quad I
                        (tile (/ x1 25) (/ y1 25) x y (/ size 2)) ; Quad II
                        (tile holeX (- holeY (/ size 2)) x  (+ y (* 25 (/ size 2))) (/ size 2)) ; Quad III
                        (tile (/ (- x2 (+ x (* 25 (/ size 2)))) 25) (/ (- y2(+ y (* 25 (/ size 2)))) 25) (+ x (* 25 (/ size 2))) (+ y (* 25 (/ size 2))) (/ size 2)) ; Quad IV
                )]
            )
        ]
        ;Hole is in top left
        [else
            (define x1 (+ x (* 25 (- (/ size 2) 1)))) ; Quad III top right
            (define y1 (+ y (* 25 (/ size 2) 1)))
            (define x2 (+ x (* 25 (/ size 2)))) ; Quad IV top left
            (define y2 (+ y(* 25 (/ size 2))))
            (define x3 (+ x (* 25 (/ size 2)))) ; Quad I bottom left
            (define y3 (+ y(* 25 (- (/ size 2) 1))))
            (send dc draw-rectangle x1 y1 25 25)
            (send dc draw-rectangle x2 y2 25 25)
            (send dc draw-rectangle x3 y3 25 25)
            (cond
                [(= size 2) 0]
                [else (+ 
                        (tile (/ (- x3  (+ x (* 25 (/ size 2)))) 25) (/ y3 25) (+ x (* 25 (/ size 2))) y (/ size 2)) ; Quad I
                        (tile holeX holeY x y (/ size 2)) ; Quad II
                        (tile (/ x1 25) (/ (- y1 (+ y (* 25 (/ size 2)))) 25) x (+ y (* 25 (/ size 2))) (/ size 2)) ; Quad III
                        (tile (/ (- x2 (+ x (* 25 (/ size 2)))) 25) (/ (- y2(+ y (* 25 (/ size 2)))) 25) (+ x (* 25 (/ size 2))) (+ y (* 25 (/ size 2))) (/ size 2)) ; Quad IV
                )]
            )
        ]
    )]
    )
)
; Generate file
(draw-grid 0 0)
(tile (car holeCoords) (car (cdr holeCoords)) 0 0 length)
(send target save-file "out.png" 'png)