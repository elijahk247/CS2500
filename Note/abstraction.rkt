;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname abstraction) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

;; A LOS (list of string) is one of:
;; - empty
;; - (cons String LOS)
(define los1 (list "cat" "dog" "mouse"))

;; LOS -> Boolean
;; is "cat" in the list?
(check-expect (cat-in? (list "cat" "dog" "mouse")) true)
(check-expect (cat-in? (list "dog" "mouse")) false)
(check-expect (cat-in? '()) false)
(define (cat-in? alos)
  (in? alos "cat"))


;; LOS -> Boolean
;; is "dog" in the list?
(check-expect (dog-in? los1) true)
(check-expect (dog-in? '()) false)
(define (dog-in? alos)
  (in? alos "dog"))

;; LOS String -> Boolean
;; is the given string in the list?
(check-expect (in? los1 "cat") true)
(check-expect (in? los1 "dog") true)
(check-expect (in? '() "cat") false)
(define (in? alos str)
  (cond [(empty? alos) false]
        [(cons? alos) (or (string=? str (first alos))
                          (in? (rest alos) str))]))

;; Any -> Boolean
;; is the given a list?
(define (list? x)
  (or (empty? x)
      (cons? x)))

;; A LON (list of Number) is one of:
;; - empty
;; - (cons Number LON)

(define lon1 (list 1 2 3))

;; LON -> LON
;; add one to every number in the list
(check-expect (add-one lon1) '(2 3 4))
(check-expect (add-one empty) empty)
(define (add-one alon)
  (add-n alon 1))

;; LON -> LON
;; add ten to every number in the list
(check-expect (add-ten lon1) '(11 12 13))
(check-expect (add-ten empty) empty)
(define (add-ten alon)
  (add-n alon 10))

;; LON Number -> LON
;; add the given number to every number in the list
(check-expect (add-n lon1 1) '(2 3 4))
(check-expect (add-n empty 1) empty)
(define (add-n alon n)
  (cond [(empty? alon) empty]
        [(cons? alon) (cons (+ n (first alon))
                            (add-n (rest alon) n))]))


;; A LOS (list of string) is one of:
;; - empty
;; - (cons String LOS)


;; A LON (list of Number) is one of:
;; - empty
;; - (cons Number LON)

;; A [List-of X] is one of:
;; - empty
;; - (cons X [List-of X])


;; [List-of [List-of Number]]

empty
(list (list 1 2 3) (list 3 4 5))

 '((1 2 3) (3 4 5)) ;correct
'('(1 2 3) '(3 4 5)) ;incorrect

















