;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Snake really IMPORTANT CHECK again|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; Segment - Snake Game - Really important

;; Constants
(define RADIUS 5)
(define CELL-WIDTH (* 2 RADIUS))
(define HEIGHT (* CELL-WIDTH 30))
(define WIDTH (* CELL-WIDTH 30))
(define BG (empty-scene WIDTH HEIGHT))
(define FOOD-IMG (circle RADIUS "solid" "green"))
(define SEG-IMG (circle RADIUS "solid" "red"))

;; World Definitions
;; A World is a (make-world Snake Fodd)
(define-struct world (snake food))

;; A Snake is a (make-snake LOS Direction)
(define-struct snake (segs dir))

;; A Food is a (make-posn Number Number)

;; A Direction is one of: "Up" "Down" "Left" "Right"

;; A LOS (list of Posn) is one of:
;; - empty
;; - (cons Posn LOS)

;; A NELOS (list of Posn) is one of:
;; - (cons Posn empty)
;; - (cons PSon NELOS)







#;(define (world-temp w)
  ... (world-snake w)
  ... (world-food w))

#;(define (snake-temp sn)
  ... (los-temp (snake-segs sn))
  ... (snake-dir sn))

#;(define (los-temp alist)
  (cond [(empty? alist) ...]
        [(cons? ... (posn-temp (first alist))
                ... (lost-temp (rest alist)))]))

#;(define (nelos-temp alist)
  (cond [(empty? (restalsit)) ... (first alist)]
        [else ... (first alist)
              ... (nelos-temp (rest alist))]))

#;(define (posn-temp aposn)
  ... (posn-x aposn)
  ... (posn-y aposn))

#;(define (dir-temp adir)
  (cond [(string=? adir "left") ...]
        [(string=? adir "right") ...]
        [(string=? adir "up") ...]
        [(string=? adir "down") ...]))
        
              
;; World -> World
;; launches the snake game
(define (main w)
  (big-bang w
            [to-draw draw-world]
            [on-tick move-world]
            [on-key change-dir]
            [stop-when collision?]))


(define los1 (cons (make-posn 15 15)
                   (cons (make-posn 15 16)
                         (cons (make-posn 15 17) empty))))
(define snake0 (make-snake los1 "up"))
(define world0 (snake0 (make-posn 3 6)))

;; draw-world : World -> Image
;; draw the world on the scene
(check-expect (draw-world world0)
              (place-image SEG-IMG 150 150
                           (place-image SEG-IMG 150 160
                                        (place-image SEG-IMG 150 170
                                                     (place-image FOOD-SEG 30 60
                                                                  BG)))))

(define (draw-world w)
 (place-image (draw-snake (world-snake w))))
              
                         

                       
