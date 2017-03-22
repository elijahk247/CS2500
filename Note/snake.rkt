;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

;;;Constants;;;;;;;
(define RADIUS 5)
(define CELL-WIDTH (* RADIUS 2))
(define GRID-WIDTH 30)
(define GRID-HEIGHT 30)
(define HEIGHT (* CELL-WIDTH GRID-HEIGHT))
(define WIDTH (* CELL-WIDTH GRID-WIDTH))
(define BG (empty-scene WIDTH HEIGHT))
(define FOOD-IMG (circle RADIUS 'solid 'green))
(define SEG-IMG (circle RADIUS 'solid 'red))

;;;World Definitions
;; A World is a (make-world Snake Food))
(define-struct world (snake food))

;; A Snake is a (make-snake LOS Direction)
(define-struct snake (segs dir))

;; A Food is a (make-posn Number Number)
;; interp. the x and y positions of the food are represented as grid square numbers

;; A Direction is one of "up" "down" "left" "right"

;; A LOS (list of Posn) is one of:
;; - empty
;; - (cons Posn LOS)
;; interp. the x and y positions of a segment are represented as grid square numbers


;; A NELOS (list of Posn) is one of:
;; - (cons Posn empty)
;; - (cons Posn NELOS)



#;(define (world-temp w)
  ...(world-snake w)
  ...(world-food w))

#;(define (snake-temp sn)
  ...(los-temp (snake-segs sn))
  ...(snake-dir sn))

#;(define (los-temp alist)
  (cond [(empty? alist) ...]
        [(cons? alist) ...(posn-temp (first alist))
                       ...(los-temp (rest alist))]))

#;(define (nelos-temp alist)
  (cond [(empty? (rest alist)) ...(first alist)]
        [else ...(first alist)
              ...(nelos-temp (rest alist))]))

#;(define (posn-temp aposn)
  ...(posn-x aposn)
  ...(posn-y aposn))

#;(define (dir-temp adir)
  (cond [(string=? adir "left")...]
        [(string=? adir "right")...]
        [(string=? adir "up")...]
        [(string=? adir "down")...]))

;; World -> World
;; launches the snake game
#;(define (main w)
  (big-bang w
            [to-draw draw-world]
            [on-tick move-world]
            [on-key change-dir]
            [stop-when collision?]))

(define los1 (cons (make-posn 15 15)
                   (cons (make-posn 15 16)
                         (cons (make-posn 15 17) empty))))
(define snake0 (make-snake los1 "up"))
(define world0 (make-world snake0 (make-posn 3 6)))
(define snake1 (make-snake los1 "right"))
(define world1 (make-world snake1 (make-posn 3 6)))
(define world2 (make-world snake0 (make-posn 15 15)))

;; draw-world : World -> Image
;; draw the world onto the scene
(check-expect 
 (draw-world world0)
 (place-image SEG-IMG 150 150
              (place-image SEG-IMG 150 160
                           (place-image SEG-IMG 150 170
                                        (place-image FOOD-IMG 30 60 BG)))))
(define (draw-world w)
  (draw-seg (world-food w) FOOD-IMG
               (draw-snake-segs (snake-segs (world-snake w)) BG)))

;; draw-snake-segs : LOS Image -> Image
;; draw the snake segments
(check-expect (draw-snake-segs los1 BG)
              (place-image SEG-IMG 150 150
                           (place-image SEG-IMG 150 160
                                        (place-image SEG-IMG 150 170 BG))))
(define (draw-snake-segs alist sc)
  (cond [(empty? alist) sc]
        [(cons? alist) (draw-seg (first alist) SEG-IMG
                                 (draw-snake-segs (rest alist) sc))]))

;; draw-seg : Posn Image Image -> Image
;; draw a segment onto the scene
(check-expect (draw-seg (make-posn 5 7) SEG-IMG BG)
              (place-image SEG-IMG 50 70 BG))
(define (draw-seg aposn seg-img sc)
  (place-image seg-img (* CELL-WIDTH (posn-x aposn))
               (* CELL-WIDTH (posn-y aposn)) sc))

;; move-world : World -> World
;; moves the snake, or eats the food and grows at the next tick
(check-expect (move-world world0)
              (make-world (make-snake (cons (make-posn 15 14)
                                            (cons (make-posn 15 15)
                                                  (cons (make-posn 15 16) empty)))
                                      "up")
                          (make-posn 3 6)))
(check-expect (move-world world1)
              (make-world (make-snake (cons (make-posn 16 15)
                                            (cons (make-posn 15 15)
                                                  (cons (make-posn 15 16) empty)))
                                      "right")
                          (make-posn 3 6)))
(check-random (move-world world2)
              (make-world (make-snake (cons (make-posn 15 14) los1)
                                      "up")
                          (make-posn (add1 (random (sub1 GRID-WIDTH)))
                                     (add1 (random (sub1 GRID-HEIGHT))))))
(define (move-world w)
  (if (posn=? (first (snake-segs (world-snake w))) 
             (world-food w))
      (make-world (grow-snake (world-snake w))
                  (make-posn (add1 (random (sub1 GRID-WIDTH)))
                            (add1 (random (sub1 GRID-HEIGHT)))))
      (make-world (move-snake (world-snake w))
                  (world-food w))))

;; posn=? : Posn Posn -> Boolean
;; are the two posns the same?
(check-expect (posn=? (make-posn 30 30) (make-posn 30 30)) true)
(check-expect (posn=? (make-posn 40 30) (make-posn 30 30)) false)
(define (posn=? p1 p2)
  (and  (= (posn-x p1) (posn-x p2))
        (= (posn-y p1) (posn-y p2))))

;; move-snake : Snake -> Snake
;; moves the snake in its direction
(check-expect (move-snake snake0)
              (make-snake (cons (make-posn 15 14)
                                (cons (make-posn 15 15)
                                      (cons (make-posn 15 16) empty)))
                          "up"))
(define (move-snake sn)
  (make-snake (move-segs (snake-segs sn) (snake-dir sn))
              (snake-dir sn)))

;; move-segs : LOS Direction -> LOS
;; move the snake segs in the given direction
(check-expect (move-segs los1 "up")
              (cons (make-posn 15 14)
                    (cons (make-posn 15 15)
                          (cons (make-posn 15 16) empty))))
(check-expect (move-segs los1 "right")
              (cons (make-posn 16 15)
                    (cons (make-posn 15 15)
                          (cons (make-posn 15 16) empty))))
(define (move-segs alos dir)
  (cons (move-seg (first alos) dir)
        (all-but-last alos)))

;; all-but-last : NELOS -> LOS
;; drop the last segment
(check-expect (all-but-last los1) (cons (make-posn 15 15)
                                        (cons (make-posn 15 16) empty)))
(define (all-but-last alist)
  (cond [(empty? (rest alist)) empty]
        [else (cons (first alist)
                    (all-but-last (rest alist)))]))

;; move-seg : Posn Direction -> Posn
;; move the posn in the given direction
(check-expect (move-seg (make-posn 16 15) "left")
              (make-posn 15 15))
(define (move-seg adir aposn)
  (cond [(string=? adir "left") (make-posn (sub1 (posn-x aposn)) (posn-y aposn))]
        [(string=? adir "right") (make-posn (add1 (posn-x aposn)) (posn-y aposn))]
        [(string=? adir "up") (make-posn (posn-x aposn) (sub1 (posn-y aposn)))]
        [(string=? adir "down") (make-posn (posn-x aposn) (add1 (posn-y aposn)))]))
  
;; grow-snake : Snake -> Snake
;; add another segment to the snake
(check-expect (grow-snake snake0)
              (make-snake (cons (make-posn 15 14) los1) "up"))
(define (grow-snake sn)
  (make-snake (cons (move-seg (first (snake-segs sn)) (snake-dir sn))
                    (snake-segs sn))
              (snake-dir sn)))


