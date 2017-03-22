;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Note May 17 part 2|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; A RD is one of:
;; 'solid
;; (make-doll RD)
(define-struct doll (contents))
(define rd1 (make-doll (make-doll (make-doll 'solid))))
(define rd0 'solid)

#; (define (rd-temp ard)
     (cond [(symbol? ard) ...]
           [(doll? ard) ... (rd-temp (doll-contents ard))]))

;; RD -> Number
;; counts the shells on the doll

(check-expect (count-shells rd0) 0)
(check-expect (count-shells rd1) 3)

;(define (count-shells ard)
;  (cond [(symbol?










;; A LON is one of:
;; empty
;; (cons Number LON)

(define lon1 empty)
(define lon2 (cons 1 empty))
(define lon3 (cons 1 (cons 5 empty)))


;; A LOS is one of:
;; empty
;; (cons String LOS)

(define los1 empty)
(define los2 (cons "1" (cons "2" (cons "3" empty))))