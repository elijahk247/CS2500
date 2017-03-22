;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Note May 11|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; An Entree-Price is one of:
;; >1 and <10
;; >=10 and <20
;; >=20 and <40
;; >=40


;; Entree-Price -> String
;; produce the yelp rating for the entree price

(check-expect (yelp 3) "$")
(check-expect (yelp 15) "$$")
(check-expect (yelp 25) "$$$")
(check-expect (yelp 45) "forget it")

(define (yelp a-price)
  (cond [(and (>= a-price 0) (< a-price 10)) "$"]
        [(and (>= a-price 10) (< a-price 20)) "$$"]
        [(and (>= a-price 20) (< a-price 40)) "$$$"]
        [(>= a-price 40) "forget it"]
        [else "error"]))



;; A Wavelength is one of:
;; >=390 and <455
;; >=455 and <492
;; >=492 and <577
;; >=577 and <597

;; Wavelength -> Image
;; draw a circle with a color mathc the given wavelength
;; ' " mark are both fine with image
(check-expect (draw-circle 460)
              (circle 100 "solid" "blue"))
(check-expect (draw-circle 400)
              (circle 100 "solid" "violet"))
(check-expect (draw-circle 500)
              (circle 100 'solid 'green))
(check-expect (draw-circle 596)
              (circle 100 'solid 'yellow))

;(define (draw-circle wl)
 ; (cond [(and (>= wl 390) (< wl 455)) (circle 100 "solid" "violet")]
  ;      [(and (>= wl 455) (< wl 492)) (circle 100 "solid" "blue")]
   ;     [(and (>= wl 492) (< wl 577)) (circle 100 "solid" "green")]
    ;    [(and (>= wl 577) (< wl 597)) (circle 100 "solid" "yellow")]
     ;   [else "error"]))

(define (draw-circle wl)
  (circle 100 "solid" (cond [(and (>= wl 390) (< wl 455)) 'violet]
                            [(and (>= wl 455) (< wl 492)) 'blue]
                            [(and (>= wl 492) (< wl 577)) 'green]
                            [(and (>= wl 577) (< wl 597)) 'yellow]
                            [else "error"])))


;; A TL is one of:
;; "red"
;; "yellow"
;; "green'

(define RADIUS 100)

;; TL -> TL
;; launch the world
(define (main tl)
  (big-bang tl
            [to-draw draw-light]
            [on-tick change-light .3]))

;; TL -> Image
;; draw the light

(check-expect (draw-light "red") (circle RADIUS "solid" "red"))
(check-expect (draw-light "yellow") (circle RADIUS "solid" "yellow"))
(check-expect (draw-light "green") (circle RADIUS "solid" "green"))


(define (draw-light tl)
  (circle RADIUS "solid" tl))

;; TL -> TL
;; change the light at the next tick

(check-expect (change-light "red") "green")
(check-expect (change-light "green") "yellow")
(check-expect (change-light "yellow") "red")

(define (change-light tl)
  (cond [(string=? tl "red") ..]
        [(string=? tl "green") ...]
        [(string=? tl "yellow") ...]))
        
                     