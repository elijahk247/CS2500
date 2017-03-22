;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname unfinished) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; Problem 2
(deifne WIDTH 400)
(deifne HEIGHT (* 2 WIDTH))
(define BG (empty-scene WIDTH HEIGHT))

(define-struct postion (a b))

(define (main w)
  (big-bang w
            [to-draw draw-world]
            [on-tick count-time 1 5]
            [on-mouse draw-line]))

(define (draw-line w)
  (cond [(mouse=? "click") (

  