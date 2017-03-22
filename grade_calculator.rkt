;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname grade_calculator) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Fill your grades in here
(define MY-HW (list 38 86 71 64 132 88 142))
(define MY-MIDTERM-1 44)
(define MY-MIDTERM-2 36)
(define CORR-QUIZ 11) ;; # of correct quizzes

;; These are the max points and max amount of quizzes
(define HW-MAX (list 38 86 71 64 132 88 142))
(define MAX-MT-1 44)
(define MAX-MT-2 36)
(define MAX-Q 11)

; list-of-hw - > list-of-hw
; translates your raw number score on homeworks to percentages
(define (HW-PERCENT-ARRAY hw hw2)
  (cond
    [(empty? hw) '()]
    [else (cons (/ (first hw) (first hw2))
                (HW-PERCENT-ARRAY (rest hw) (rest hw2)))]))

; list-of-percentages -> percentage
; gets the lowest score on your homeworks
(define (lowest arr)
  (foldr min (first arr) (rest arr)))

; list-of-percentages -> percentage
; Gets the sum of all the homework grades dropping the lowest one
(define (DROP-LOWEST arr)
  (- (foldr + 0 arr) (lowest arr)))

; What percentage of the 35 hw points you got
(define HW-PERCENT
  (/ (DROP-LOWEST (HW-PERCENT-ARRAY MY-HW HW-MAX)) 6))

; This is your grade
(define MY-GRADE
  (+ (* 35 HW-PERCENT)
     (* 25 (/ MY-MIDTERM-1 MAX-MT-1))
     (* 35 (/ MY-MIDTERM-2 MAX-MT-2))
     (* 5  (/ CORR-QUIZ MAX-Q))))

MY-GRADE

; Run this to get a calculation for your grade.
; DISCLAIMER: MAY OR MAY NOT BE ACCURATE +- 5 POINTS LETS GO WITH THAT

; See anything that is off or wrong? Leave a comment.