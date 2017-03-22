;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |note June 7th|) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t quasiquote repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
;; An Atom is one of
;; - String
;; - Number
;; - Symbol

;; Any -> Boolean
;; is it an atom?
(check-expect (atom? 5) true)
(check-expect (atom? true) false)
(check-expect (atom? atom?) false)
(define (atom? x)
  (or (string? x) (number? x) symbol? x))

;; atom=? : Atomo Atom -> Boolean
;; are the two atos the same
(check-expect (atom=? 5 '5) false)
(define (atom=? a1 a2)
  (cond [(and (string? a1) (string? a2) (string=? a1 a2))]
        [(and (string? a1) (symbol? a2) (symbol=? a1 a2))]
        [(and (number? a1) (number?? a2) (= a1 a2))]))

;;d
        
         


; An S-exp is one of:
;; - Atom
;; - [List of S-exp]

;; A [List of S-exp] is one of
;; -empty
;; - (cons S-exp [List of S-exp])


;; S-exp -> ?
(define (s-exp-temp s)
  (cond [(atom? s) ...]
        [else ...]))

;; los-exp -> ?
(define (los-exp-temp alos)
  (cond [(empty? alos) ...]
        [(cons? alos) ... (s-exp-temp (first alos)
                                      (lost-exp-temp (rest alos)))]))