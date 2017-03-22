;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 516b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; STEP 1: global constants 
(define RADIUS 100)
(define SPEED (quotient RADIUS 5))
(define BALL (circle RADIUS "solid" "red"))
(define WIDTH 800)
(define MID RADIUS)
(define BACKGROUND (empty-scene WIDTH (* 2 RADIUS)))
(define TXT (text "press any key" 33 "black"))

;; STEP 2: data definitions
;; A Running is a (make-running Number Direction)
(define-struct running (x dir))

;; A Resting is a (make-resting Number Direction)
(define-struct resting (x dir))

;; Direction is one of:
;; -1
;; 1
;; interp. -1 means right to left, 1 means left to right

;; A SG is one of:
;; "press any key"
;; Resting 
;; Running
;; interp. "press any key" means game hasn't started

;; SG -> ?
#;(define (sg-temp w)
  (cond [(string? w) ...]
        [(resting? w) ...(resting-x w)
                      ...(resting-dir w)]
        [(running? w) ...(running-x w)
                      ...(running-dir w)]))

(define sg1 "press any key")
(define sg2 (make-running 200 1))
(define sg3 (make-resting 200 -1))

;; Any -> SG
;; launch the world
(define (main x)
  (big-bang "press any key"
            [to-draw render-sg]
            [on-tick next-ball]
            [on-key start-pause]))

;; render-sg : SG -> Image
;; render the current stop-and-go state 
(check-expect (render-sg sg1)
              (place-image TXT (/ WIDTH 2)
                           MID BACKGROUND))
(check-expect (render-sg sg2) (place-image BALL 200 MID BACKGROUND))
(check-expect (render-sg sg3) (place-image BALL 200 MID BACKGROUND))

(define (render-sg w)
  (cond [(string? w) (place-image TXT (/ WIDTH 2) MID BACKGROUND)]
        [(resting? w) (place-image BALL (resting-x w) MID BACKGROUND)]
        [(running? w) (place-image BALL (running-x w) MID BACKGROUND)]))

;; start-pause : SG Key -> SG
;; start, pause or restart the ball
(check-expect (start-pause sg1 " ") (make-running 0 1))
(check-expect (start-pause sg2 "r") (make-resting 200 1))
(check-expect (start-pause sg3 "f") (make-running 200 -1))
(define (start-pause w akey)
  (cond [(string? w) (make-running 0 1)]
        [(resting? w) (make-running (resting-x w)
                                    (resting-dir w))]
        [(running? w) (make-resting (running-x w)
                                    (running-dir w))]))

;; next-ball : SG -> SG
;; move a running ball in the appropriate direction or flip the direction
;; leave all others alone
(check-expect (next-ball sg1) sg1)
(check-expect (next-ball sg2) (make-running 220 1))
(check-expect (next-ball sg3) sg3)
(check-expect (next-ball (make-running 0 -1)) (make-running 1 1))
(check-expect (next-ball (make-running 800 1)) (make-running 799 -1))
(define (next-ball w)
  (cond [(string? w) w]
        [(resting? w) w]
        [(running? w) (if (< 0 (running-x w) WIDTH)
                          (move-ball w)
                          (flip-direction w))]))

;; move-ball : Running -> Running
;; move the ball left or right depending on the direction
(check-expect (move-ball sg2) (make-running 220 1))
(check-expect (move-ball (make-running 200 -1)) (make-running 180 -1))
(define (move-ball r)
  (make-running (+ (running-x r) (* SPEED (running-dir r)))
                (running-dir r)))

;; flip-direction : Running -> Running
(check-expect (flip-direction (make-running 0 -1)) (make-running 1 1))
(check-expect (flip-direction (make-running 800 1)) (make-running 799 -1))
(define (flip-direction r)
  (cond [(<= (running-x r) 0) (make-running 1 1)]
        [(>= (running-x r) WIDTH) (make-running 799 -1)]))












