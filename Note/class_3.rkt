;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |class 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/universe)
(require 2htdp/image)
;;step 1
(define radius 100)
(define speed (quotient radius 5))
(define ball (circle radius "solid" "red"))
(define width 800)
(define mid radius)
(define bg (empty-scene width (* 2 radius)))
(define TXT (text "press my key" 33 'black))

;;Step 2 
;;a running is a (make-running Number Direction)
(define-struct resting (x dir))

(define-struct running (x dir))
;;direction is one of: 
;;-1 
;;1
;;interp, -1 means right to left , 1 means left to right 

;; a sg is one of:
;;"press any key"
;;resting 
;;running 
;;interp. "press the key" means game hasn't startd

(define sg1 "press any key")
(define sg2 (make-running 200 1))
(define sg3 (make-resting 200 -1))

;;sg -> ?
(define (sg-temp w)
  (cond [(string? w)...]
        [(resting? w)...(resting-x w)
                     ...(resting-dir w)]
        [(running? w)...(resting-x w)
                     ...(running-x w)]))
;;any -> sg
;;launch the world 
(define (main x)
  (big-bang txt 
            [to-draw render-sg]
            [on-tick next-ball]
            [on-key start-pause]))

;;render-sg: sg -> image
;; render the current stop-and-go state
(check-expect (render-sg sg1)
              (place-image TXT (/ width 2)
                           mid bg))
(check-expect (render-sg sg2) 
              (place-image ball 200 mid bg))
(check-expect (render-sg sg2) 
              (place-image ball 200 mid bg))

(define (render-sg w)
  (cond [(string? w) (place-image TXT (/ width 2) mid bg)]
        [(resting? w) (place-image ball (resting-x w) mid bg)]
        [(running? w) (place-image ball (resting-x w) mid bg)]))

;;start-pause : sg key -> sg
;;start, pause or restart the ball
(check-expect (start-pause sg1 " ") (make-running 0 1))
(check-expect (start-pause sg2 "r") (make-resting 200 1))
(check-expect (start-pause sg3 "r") (make-resting 200 -1))

(define (start-pause w key)
  (cond [(string? w) (make-running 0 1)]
        [(resting? w) (make-running (resting-x w)
                      (resting-dir w))]
        [(running? w) (make-resting (resting-x w)) 
                      (running-dir w)]))

;;;;;;;;;;;;;;;;;;;;;
;;A rd is one of
;;(make-doll 'solid)
;;(make-doll (make-doll 'solid))
(define (rd-temp ard)
  (cond [(symbol? ard)...]
        [(doll? ard) ...
         (red-temp (doll-contents ard))]))

(define-struct doll (contents))
(define rd1 (make-doll (make-doll (make-doll 'solid))))
(define rd0 'solid)

(define (rd-temp ard)
  (cond [(symble? ard) ...]
        [(doll? ard) ...(red-temp (doll-contents ard))]))

(define (count-shells ard)
  (cond [(symble? ard) 0]
        [(doll? ard) (+ 1 (count-shells (doll-contents ard)))]))

;;a lon is one of:
;; empty 
;;(cons Number on)
(define lon1 empty)
(define lon2 (cons 1 empty))
(define lon3 (cons 1 (cons 3 empty)))

;; a los is one of 
;;empty 
;;(cons string los)
(define los1 (cons "a" (cons "b" (cons "c" empty))))

;;a thing is one of 
;;number 
;;string 

;;a alt is one of 
;;empty
;;(cons thing lot)