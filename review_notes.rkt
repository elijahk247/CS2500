;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname review_notes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; 2

(define (flatten n)
  (cond [(number? n) (list n)]
        [else
         (append (flatten (node-left n))
                 (flatten (node-right n)))]))

(define-struct node (left right))

(define tree1 (make-node (make-node 10 9) (make-node 3 (make-node 1 5))))
(define tree2 5)
(check-expect (flatten tree1) '(10 9 3 1 5))
(check-expect (flatten tree2) '(5))


;; problem 3
(define tree3 (make-node (make-node 9 10) (make-node 5 (make-node 0 -7))))

(define (same-shape? tree1 tree2)
  (cond [(and (number? tree1) (number? tree2)) #t]
        [(and (node? tree1) (node? tree2))
         (and (same-shape? (node-left tree1) (node-left tree2))
              (same-shape? (node-right tree1) (node-right tree2)))]
        [else #f]))

(check-expect (same-shape? tree3 tree1) #t)
(check-expect (same-shape? tree3 tree2) #f)

;; problem 4

(define-struct song (title artist length))

(define song1 (make-song "Hey, Jude" "The Beatles" 431))
(define song2 (make-song "U Smile" "Justin Bieber" 197))
(define song3 (make-song "Free Bird" "Lynyrd Skynyrd" 608))
(define songs (list song1 song2 song3))

(define (total-time songs)
  (foldr (lambda (song so-far) (+ so-far (song-length song))) 0 songs))

(check-expect (total-time '()) 0)
(check-expect (total-time songs) 1236)

(define (any-bieber? songs)
  (ormap (lambda (song) (string=? "Justin Bieber" (song-artist song))) songs))

(check-expect (any-bieber? '()) #f)
(check-expect (any-bieber? songs) #t)

(define (remove-artist songs artist)
  (filter (lambda (song) (not (string=? (song-artist song) artist))) songs))

(check-expect (remove-artist '() "No one") '())
(check-expect (remove-artist songs "Justin Bieber") (list song1 song3))


;; problem 4 again

; List-of-Song -> List-of-Song


;; problem 5 (extra credit)



(define-struct node2 (left val right))

(define grade1 (make-node2 (make-node2 3 0 1)
                           10
                           (make-node2 8 100 (make-node2 1 2 3))))
(define grade2 (make-node2 (make-node2 1 0 10)
                           3
                           105))
(define grade3 (make-node2 (make-node2 1 5 8)
                           10
                           (make-node2 (make-node2 11 17 18)
                                      23
                                      87)))
(define (max-min-value tree f )
  (local
    [(define (helper tree acc)
       (cond [(number? tree) (f acc tree)]
             [(node2? tree)
              (f (node2-val tree)
                 (helper (node2-left tree) acc)
                 (helper (node2-right tree) acc))]))]
    (if (number? tree)
        (helper tree tree)
        (helper tree (node2-val tree)))))

(check-expect (max-min-value grade2 max) 105)
(check-expect (max-min-value grade2 min) 0)
(check-expect (max-min-value grade3 max) 87)
(check-expect (max-min-value grade3 min) 1)
                                       
(define (tree-ordered? tree)
  (cond [(number? tree) #t]
        [(node2? tree)
         (and (< (max-min-value (node2-left tree) min)
                 (node2-val tree)
                 (max-min-value (node2-right tree) max))
              (tree-ordered? (node2-left tree))
              (tree-ordered? (node2-right tree)))]))

(check-expect (tree-ordered? (make-node2 1 0 10)) #f)
(check-expect (tree-ordered? grade1) #f)
(check-expect (tree-ordered? grade2) #f)
(check-expect (tree-ordered? grade3) #t)