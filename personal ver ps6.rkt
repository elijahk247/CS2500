;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |my ver ps6|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; An Atom is one of:
;; - Number
;; - Symbol
;; - String
;;
;; An SExp is one of:
;; - Atom
;; - [List-of SExp]

(define (string-cons sexp)
  (string-append "(" (sexp-string sexp) ")"))

(define (atom? sexp)
  (or (number? sexp)
      (symbol? sexp)
      (string? sexp)))

(define (sexp-string sexp)
  (cond [(empty? sexp) ""]     #;;empty? blank?
        [(atom? (first sexp))
         (cond [(number? (first sexp))
                (string-append (number->string (first sexp))
                               (sexp-string (rest sexp)))]
               [(symbol? (first sexp))
                (string-append (symbol->string (first sexp))
                               (sexp-string (rest sexp)))]
               [(string? (first sexp))
                (string-append "\"" (first sexp) "\""
                       (sexp-string (rest sexp)))])]
        [(list? (first sexp))
         (string-append " (" (sexp-string (first sexp)) ") "
                             (sexp-string (rest sexp)))]))

;(check-expect (string-cons '(a (37 "foo") c)) "(a (37 \"foo\") c)")
 
;(check-expect (sexp-string '(a (37 "foo") c)) "a (37 \"foo\") c")

(define-struct lego (label color width))
; A Lego is a structure:
;    (make-lego Number Symbol Number)
; interpretation: (make-lego l c w) is the lego brick
; with label l, color c, and width w (in pixels).
 
(define-struct bigger (lego left right))
; A alegoldg (lego building) is one of:
; - Lego
; - (make-bigger Lego alegoldg alegoldg)
; interpretation: (make-bigger l lft rgt) makes a bigger
; lego building by putting a lego brick l on top of two lego
; buildings lft (left) and rgt (right).


;; p3

(define (count-bricks alego)
  (cond
    [(lego? alego) 1]
    [else 
     (+ 1 (count-bricks (bigger-left alego)) 
          (count-bricks (bigger-right alego)))]))

;; p4

(define (how-high alego)
  (cond [(lego? alego) 10]
        [(bigger? alego)
         (+ 10  (if
                 (>= (how-high (bigger-left alego)) (how-high (bigger-right alego)))
                 (how-high (bigger-left alego))
                 (how-high (bigger-right alego))))]))

(check-expect (how-high (make-lego 4 'purple 80)) 10)

(check-expect (how-high (make-bigger (make-lego 4 'purple 80)
               (make-bigger (make-lego 2 'blue 60)
                          (make-lego 1 'yellow 40)
                          (make-lego 3 'red 40))
               (make-bigger (make-lego 6 'orange 60)
                          (make-lego 5 'green 40)
                          (make-lego 7 'red 40)))) 30)

;; p5

(define (contains-colored-bricks? alego acolor)
  (cond [(lego? alego) (symbol=? (lego-color alego) acolor)]
        [(bigger? alego) (or
                          (contains-colored-bricks? (bigger-lego alego) acolor)
                          (contains-colored-bricks? (bigger-left alego) acolor)
                          (contains-colored-bricks? (bigger-right alego) acolor))]))

;; p6
(define (find-colored-brick? alego acolor)
  (cond 
    [(empty? alego) false]
    [(lego? alego) (if (symbol=? acolor (lego-color alego)) 
                       alego
                       false)]
    [(bigger? alego)
     (if (symbol=? acolor (lego-color (bigger-lego )))
         (bigger-lego alego)
         (cond
           [(lego? (find-colored-brick? (bigger-right alego) acolor))
            (bigger-right alego)]
           [(lego? (find-colored-brick? (bigger-left alego) acolor))
            (bigger-left alego)]
           [else 
            (or (find-colored-brick? (bigger-left alego) acolor)
                (find-colored-brick? (bigger-right alego) acolor))]))])) 
  
;; p7

(define lego1 (make-bigger (make-lego 4 'purple 80)
             (make-bigger (make-lego 2 'blue 60)
                          (make-lego 1 'black 40)
                          (make-lego 3 'red 40))
             (make-bigger (make-lego 6 'orange 60)
                          (make-lego 5 'green 40)
                          (make-lego 7 'red 40))))
(define lego2 (make-bigger (make-lego 4 'purple 80)
             (make-bigger (make-lego 2 'blue 60)
                          (make-lego 1 'yellow 40)
                          (make-lego 3 'red 40))
             (make-lego 6 'orange 60)))

(define HH 200)
(define WW 200)
(define BG (empty-scene HH WW))


;; Purpose:
;; Takes a lego and produces an image of it
;; Lego -> Image

(define (lego->image a-lego)
  (rectangle (lego-width a-lego) 10 'solid (lego-color a-lego)))

;; Purpose:
;; Call the lb->imageBase function
;; alego -> Image
(define (lb->image LegoB)
  (lb->imageBase LegoB (/ WW 2) (/ HH 2) BG))

;; Purpose:
;; Takes a lego building and prduces an image of the building
;; Lego -> Image

(define (lb->imageBase LegoB x y img)
  (local (;; blah
          (define (difference LegoB)
            (- (+ (- x (/ (lego-width (bigger-lego LegoB)) 2)) 
                  (/ (lego-width (bigger-left LegoB)) 2))
               (- (+ x (/ (lego-width (bigger-lego LegoB)) 2)) 
                  (/ (lego-width (bigger-right LegoB)) 2))))
          (define (placeLeft LegoB x y img)
            (cond
              [(lego? (bigger-left LegoB))
               (lb->imageBase 
                (bigger-left LegoB) 
                (cond
                  [(> (+ (- x (/ (lego-width (bigger-lego LegoB)) 2)) 
                         (/ (lego-width (bigger-left LegoB)) 2))
                      (- (+ x (/ (lego-width (bigger-lego LegoB)) 2)) 
                         (/ (lego-width (bigger-right LegoB)) 2)))
                   (- (+ (- x (/ (lego-width (bigger-lego LegoB)) 2)) 
                         (lego-width (bigger-left LegoB))) 
                      (/ (difference LegoB) 2))]
                  [else
                   (- x (/ (lego-width (bigger-lego LegoB)) 2))]) 
                (+ y 10) img)]
              [(bigger? (bigger-left LegoB))
               (lb->imageBase (bigger-left LegoB) 
                              (- x (/ (lego-width 
                                       (bigger-lego (bigger-left LegoB))) 2)) 
                              (+ y 10) img)])) 
          (define (placeRight LegoB x y img)
            (cond
              [(lego? (bigger-right LegoB))
               (lb->imageBase (bigger-right LegoB) 
                              (cond
                                [(< (- (+ x (/ (lego-width (bigger-lego LegoB))
                                               2)) 
                                       (/ (lego-width (bigger-right LegoB)) 2))
                                    (+ (- x (/ (lego-width (bigger-lego LegoB))
                                               2)) 
                                       (/ (lego-width 
                                           (cond
                                             [(lego? (bigger-left LegoB))
                                              (bigger-left LegoB)]
                                             [(bigger? (bigger-left LegoB))
                                              (bigger-lego 
                                               (bigger-left LegoB))])) 
                                          2)))
                                 (+ (+ x (/ (lego-width (bigger-lego LegoB)) 2)) 
                                    (/ (difference LegoB) 2))]
                                [else 
                                 (+ x (/ (lego-width (bigger-lego LegoB)) 2))]) 
                              (+ y 10) img)]
              [(bigger? LegoB)
               (lb->imageBase (bigger-right LegoB) 
                              (+ x (/ (lego-width (bigger-lego 
                                                   (bigger-right LegoB))) 2)) 
                              (+ y 10) img)]))) 
    (cond
      [(empty? LegoB) BG]
      [(lego? LegoB) (place-image (lego->image LegoB) x y img)]
    [(bigger?  LegoB)
     (place-image (lego->image (bigger-lego LegoB)) x y 
                  (placeRight LegoB x y 
                             (placeLeft LegoB x y img)))])))

(lb->image lego1)
         
                                
         