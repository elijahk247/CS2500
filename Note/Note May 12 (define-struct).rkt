;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Note May 12 (define-struct)|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; Cartesian -> Boolean
;; is the point on one of the axes?

;; A Cartesian is a (make-cartesian Number Number)
(define-struct cartesian (x y))

(define p1 (make-cartesian 3 4))
(define p2 (make-cartesian 6 8))
(define p3 (make-cartesian 0 5))


(check-expect (on-axis? p1) false)
(check-expect (on-axis? p3) true)

(define (on-axis? a-point)
  (or (= (cartesian-x a-point) 0)
      (= (cartesian-y a-point) 0)))


;; A Posn is a (make-posn any any)
;; posn-x posn-y posn?

(define posn1 (make-posn 3 4))
(define posn2 (make-posn 6 8))
;; computes distance to zero of the posn

(check-expect (dis-to-0 posn1) 5)
(define (dis-to-0 a-posn)
  (sqrt (+ (sqr (posn-x a-posn)) (sqr (posn-y a-posn)))))


;; ===========================================


;; A book is a (make-book String String Symbol Number)

(define-struct book (title author subject price))

(define b1 (make-book "A" "B" "C" "D"))
(define htdp (make-book "htdp" "fellisen" "softwaredesign" 10))

;; Book Number -> Book
;; increase the price by the given number

(check-expect (raise htdp 10)
              (make-book "htdp" "fellisen" "softwaredesign" 20))


(define (raise a-book inc)
  (make-book (book-title a-book)
             (book-author a-book)
             (book-subject a-book)
             (+ inc (book-price a-book))))

;; Design Recipe for Functions
;; 1. Design your data - make example of the data
;; 2. signature & purpose
;; 3. write examples/tests
;; 4. template
;; 5. code
;; 6. test