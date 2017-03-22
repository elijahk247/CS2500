;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname self-referential-dds) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

;; An Icecream-cone is one of:
;; 'cone
;; (make-icc String Icecream-cone)
(define-struct icc (flavor under))

(define icc1 'cone)
(define icc2 (make-icc "chocolate" icc1))
(define icc3 (make-icc "vanilla" (make-icc "strawberry" icc2)))

#;(define (icc-temp an-icc)
  (cond [(symbol? an-icc) ...]
        [(icc? an-icc) ...(icc-flavor an-icc)
                       ...(icc-temp (icc-under an-icc))]))

;; flavor? : Icecream-cone String -> Boolean
;; does the cone have the given flavor?
(check-expect (flavor? icc3 "strawberry") true)
;(check-expect (flavor? icc3 "pistachio") false)
;(check-expect (flavor? icc1 "mocha") false)
(define (flavor? an-icc fl)
  (cond [(symbol? an-icc) false]
        [(icc? an-icc) (or (string=? (icc-flavor an-icc) fl)
                           (flavor? (icc-under an-icc) fl))]))

;; A Foo is one of:
;; Number
;; (make-bar Foo Number)
(define-struct bar (w z))

(define foo1 5)
(define foo2 (make-bar 5 5))
(define foo3 (make-bar (make-bar (make-bar 5 5) 5) 5))

#;(define (foo-temp afoo)
  (cond [(number? afoo) ...]
        [(bar? afoo) ...(foo-temp (bar-w afoo))
                     ...(bar-z afoo)]))
  

;; A LOS (list of Strings) is one of:
;; - empty
;; - (cons String LOS)

(define los1 empty)
(define los2 (cons "doll" empty))
(define los3 (cons "ball" los2))
(define los4 (cons "car" los3))

#;(define (los-temp alos)
  (cond [(empty? alos)...]
        [(cons? alos) ...(first alos)
                      ...(los-temp (rest alos))]))

;; replace : LOS String String -> LOS
;; replaces old if it appears in the list and everywhere it appears with new
(check-expect (replace los4 "ball" "soccer ball")
              (cons "car" (cons "soccer ball" (cons "doll" empty))))
(check-expect (replace los1 "ball" "soccer ball") los1)
(check-expect (replace los4 "doll house" "soccer ball")
              (cons "car" (cons "ball" (cons "doll" empty)))) 
(define (replace alos old new)
  (cond [(empty? alos) empty]
        [(cons? alos) (if (string=? (first alos) old)
                          (cons new (replace (rest alos) old new))
                          (cons (first alos)
                                (replace (rest alos) old new)))]))






















