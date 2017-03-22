;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Exam Review|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; EXAM REVIEW ANSWERS

;; Problem 1

(define a '(1 2))
(define b '((3 4) (5 6)))

;;> (append a b)
;(list 1 2 (list 3 4) (list 5 6))

;;> (list a b)
;(list (list 1 2) (list (list 3 4) (list 5 6)))

;;> (cons a b)
;(list (list 1 2) (list 3 4) (list 5 6))

;;> (apply append a b)
;(list 1 2 3 4 5 6)

;; Problem 2

;;; A BT is one of:
;;; - Number
;;; - (make-node BT BT)
(define-struct node (left right))

(define tree1 (make-node (make-node 10 9)
(make-node 3
(make-node 1 5))))
(define tree2 7)

; flatten : BT -> [List-of Number]
; Makes a list of all the leaves in a BT
(check-expect (flatten tree1) '(10 9 3 1 5))
(check-expect (flatten tree2) '(7))

(define (flatten bt)
  (cond [(number? bt) (list bt)]
        [else (append (flatten (node-left bt))
                      (flatten (node-right bt)))]))

(define tree3 (make-node (make-node 9 10)
(make-node 5
(make-node 0 -7))))

;; Problem 3:
(check-expect (same-shape? 7 7) #t)
(check-expect (same-shape? 7 5) #t)
(check-expect (same-shape? tree1 tree2) #f)
(check-expect (same-shape? tree1 tree3) #t)

; Tree Tree -> Boolean
(define (same-shape? tree1 tree2)
  (cond [(and (number? tree1) (number? tree2)) #t]
        [(and (node? tree1) (node? tree2))
         (... (same-shape (node-left tree1) (node-left tree2))
              (same-shape (node-right tree1) (node-right tree2)))]
        [else #f]))

;; Problem 4
;; pt 1

(define-struct song (title artist length)) 

(define song1 (make-song "Hey, Jude" "The Beatles" 431))
(define song2 (make-song "U Smile" "Justin Bieber" 197))
(define song3 (make-song "Free Bird" "Lynyrd Skynyrd" 608))
(define songs (list song1 song2 song3))

(check-expect (total-time '()) 0)
(check-expect (total-time songs) 1236)

;[List-of-song] -> number
(define (total-time songs)
  ; [list-of-x] -> y
  ; X
  (foldr (lambda (song acc) (+ (song-length song) acc)) 0 songs))

;;pt 2

(check-expect (any-bieber? '()) #f)
(check-expect (any-bieber? songs) #t)

;; are any of these true? ormap
;; are all of these true? andmap
;; basecase in ormap is always true, false for andmap

;[list-of-songs] -> Boolean
(define (any-bieber? songs)
  (ormap (lambda (song) (string=? "Justin Bieber" (song-artist song))) songs))

;; pt 3
(check-expect (remove-artist '() "No One") '())
(check-expect (remove-artist songs "Justin Bieber") (list song1 song3))

;[List-of-songs] String -> [List-of-songs]
(define (remove-artist songs artist)
  (filter (lambda (song) (not (string=? (song-artist song) artist))) songs))
          ;filter moves everything
          ; to where condition is false

;; Problem 4 (other question)
; give contract & Purpose statement for the following function:
(define (fred xs)
  (quicksort xs
             (lambda (a b) (< (song-length a)
                              (song-length b)))))

;; [list-of-song] -> [list-of-song]

;;Problem 5 - Extra Credit
(define-struct node (left val right))

;; A tree is one of:
;; -a Grade
;; - (make-node GradeTree Grade GradeTree)
;;
;; A Grade is a number in the range of [0, 100]

(define tree1 (make-node (make-node 3 0 1)
                         10
                         (make-node 8 100 (make-node 1 2 3))))
(define tree2 (make-node (make-node 1 0 10)
                         3
                         105))
(define tree3 (make-node (make-node 1 5 8)
                         10
                         (make-node (make-node 11 17 18)
                                    23
                                    87)))
                        
(check-expect (tree-ordered? grade1) #f)
(check-expect (tree-ordered? grade3) #t)
(check-expect (tree-ordered? grade2) #f)

; Tree -> Boolean
(define (tree-ordered? tree)
  (cond [(number? tree) #t]
        [(node2? tree)
         (and (tree-ordered? (node2-left tree))
              (tree-ordered? (node2-right tree))
              )]))

; Tree -> Number
(define (max-tree tree)
  (cond [(number? tree) tree]
        [(node2? tree)
         (max (node2-val tree)
              (max-tree (node2-left tree))
              (max-tree (node2-right tree)))
         