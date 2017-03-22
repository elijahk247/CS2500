;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Note May 17|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; STEP 1: Global constants
(define RADIUS 100)
(define SPEED (quotient RADIUS 5))
(define BALL (circle RADIUS "solid" "red"))
(define WIDTH 800)
(define MID RADIUS)
(define BACKGROUND (empty-scene WIDTH (* 2 RADIUS)))
(define TXT (text "press any key" 33 "black"))

;; SETP 2: Data defintion
;; A runing is a (make-runing NUmber Direction)
(define-struct running (x dir))

;; A resting is a (make-resting NUmber Direction
(define-struct resting (x dir))

;; Direction is one of
;; -1
;; 1
;; -1 menas right to left, 1 menas left to right

;; A SG is one of
;; "press any key"
;; Resting
;; Running
;; interp. "press any key" menas game hasn't started

(define sg1 "press any key")
(define sg2 (make-runing 200 1))
(define sg3 (make-resting 200 -1))

;; SG --> ?
(define (sg-temp w)
  (cond [(string? w) ...]
        [(resting? w) ...(resting-x w)
                      ...(resting-dir w)]
        [(running? w) ...(running-x w)
                      ...(running-dir w)]))

;; Any -> SG
;; launch the world
#; (define (main x)
     (big-bang "press any key"
               (

;; render-sg : SG -> Image
;; render the current stop-and-go state
(check-expect (render sg sg1)
              [place-image TXT (/ WIDHT 2)
                           (MID BACKGROUND)])
(check-expect (render-sg sg1) (place-image BALL 200 MID BACKGROUND))
(check-expect (render-sg sg1) (place-image BALL 200 MID BACKGROUND))

;; start-pause : SG Key -> SG
;; start, pause or restart the ball
(check-expect (start-pause sg1 " ") (make-running 0 1))
(check-expect (start-pause sg1 "r") (make-running 200 1))
(check-expect (start-pause sg1 "f") (make-running 200 -1))

(define (start-pause w akey)
  (cond [(string? w) (make-running 0 1)]
        [(resting? w) ...(resting-x w)
                      ...(resting-dir w)]
        [(running? w) ...(running-x w)
                      ...(running-dir w)]))








