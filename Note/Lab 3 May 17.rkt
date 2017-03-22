;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Lab 3 May 17|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)


;; Problem 1

(define WIDTH 400)
(define HEIGHT WIDTH)
(define BACKGROUND (empty-scene WIDTH HEIGHT))

; A SW is a structure: (make-shrinking Number Number)
; interpretation: (make-shrinking s t) combines the size s
; of the circle in the world and the running time t of the world
 
(define-struct shrinking (size time))
(define world0 (make-shrinking 200 0))

(define initial-world (make-shrinking 50 0))
(define intermediate-world (make-shrinking 70 10))

;; ----------------------------------------------
; SW -> SW
(define (main world0)
  (big-bang world0
            (to-draw draw-world)
            (on-tick shrink-world)
            (on-mouse grow-world)))

; SW -> Image
; draws a red circle whose size depends on the world
(define (draw-world world0)
  (place-image (circle (shrinking-size world0) "solid" "red") 200 200
               BACKGROUND)) ; TODO: finish function

; SW -> SW
; creates a new SW in which time has been
; incremented by 1 and the size has decreased
(define (shrink-world world0)
  (make-shrinking (sub1 (shrinking-size world0))
          (+ 1 (shrinking-time world0))))


; SW Number Number MouseEvent -> SW
; creates a new SW in which the size is slightly
; larger than in world, if the left mouse button is pressed.
(define (grow-world an-sw x y me)
  (cond [(mouse=? "button-down" me)
  (make-shrinking (+ 2 (shrinking-size world0)) (shrinking-time world0))]
        [else world0]))

;(main world0)


