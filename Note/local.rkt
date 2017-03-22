;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname local) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

;; Number Number Number Number -> Number
;; computes the distance between two points
;(check-expect (dist 3 4 0 0) 5)
(define (dist x1 y1 x2 y2)
  (local [(define deltax (- x1 x2))
          (define deltay (- y1 y2))
          (define sqrx (sqr deltax))
          (define sqry (sqr deltay))]
    (sqrt (+ sqrx sqry))))

;; A [NEList-of X] is one of:
;; - (cons X empty)
;; - (cons X [NEList-of X])

;; largest: [NEList-of Number] -> Number
;; get the largest number in the list
;(check-expect (largest '(4)) 4)
;(check-expect (largest '(1 2 3 4)) 4)
;(check-expect (largest '(4 3 2 1)) 4)
;(check-expect (largest '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)) 20)
;(check-expect (largest '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21)) 21)
(define (largest alon)
  (cond [(empty? (rest alon)) (first alon)]
        [else (if (< (first alon) (largest (rest alon)))
                  (largest (rest alon))
                  (first alon))]))

;; largest2: [NEList-of Number] -> Number
;; get the largest number in the list
;(check-expect (largest2 '(4)) 4)
(check-expect (largest2 '(1 2 3)) 3)
;(check-expect (largest2 '(4 3 2 1)) 4)
;(check-expect (largest2 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)) 20)
;(check-expect (largest2 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21)) 21)
(define (largest2 alon)
  (cond [(empty? (rest alon)) (first alon)]
        [else (local [(define frst (first alon))
                      (define max (largest2 (rest alon)))]
                (if (> max frst) max frst))]))



;; my-filter : [X -> Boolean] [List-of X] -> [List-of X]
;; filters the list based on the predicate
(check-expect (my-filter even? '(1 2 3 4)) '(2 4))
(check-expect (my-filter even?  '()) '())
(define (my-filter pred alox)
  ;; [X [List-of X] -> [List-of X]] [List-of X] [List-of X] -> [List-of X]
  (foldr (λ (x y) (if (pred x)
                        (cons x y)
                        y)) empty alox)) 

;; [List-of Posn] -> Number
;; add up the xs of the posns
(check-expect (sum-xs empty) 0)
(check-expect (sum-xs (list (make-posn 3 4) (make-posn 6 8))) 9)
(define (sum-xs alop)
  ;; [Posn Number -> Number] Number [List-of Posn] -> Number
  ;(foldr (λ(p y) (+ (posn-x p) y)) 0 alop)) 
  (foldr + 0 (map posn-x alop))) 


;; A Set is a [List-of Number]
;; order doesn't matter and no repitions
(define set1 '(1 2 3 4))

;; contains?: Set N -> Boolean
;; is the number in the set?
(check-expect (contains?  set1 3) true)
(check-expect (contains?  set1 5) false)
(check-expect (contains?  '() 3) false)
(define (contains? aset n)
  ;; [X -> Boolean] [List-of X] -> Boolean
  (ormap (λ(x) (= x n)) aset)) 

;; subset : Set Set -> Boolean
;; is set1 a subset of set2?
(check-expect (subset? set1 '(1 2)) false)
(check-expect (subset? '(1 2) set1) true)
(check-expect (subset? '() set1) true)
(define (subset? s1 s2)
  (andmap (λ (x) (contains? s2 x)) s1))

;; set-equal? : Set Set -> Boolean
;; are the two sets equal?
(check-expect (set-equal? set1 set1) true)
(check-expect (set-equal? set1 '(4 3 2 1)) true)
(check-expect (set-equal? set1 '()) false)
(define (set-equal? s1 s2)
  ;(and (subset? s1 s2) (subset? s2 s1)))
  (and (subset? s1 s2) (= (length s1) (length s2))))

;; intersect : Set Set -> Set
;; get items that are members of both sets
(check-expect (intersect '(1 2) '(2 3)) '(2))
(check-expect (intersect '(1 2) '(3 4)) '())
(check-expect (intersect '(1 2 3 5) '(2 3 1 4)) '(2 3 1))
(define (intersect s1 s2)
  (filter (λ (x) (contains? s1 x)) s2))

;; union : Set Set -> Set
;; get all of the items in both sets, no repeats
(check-expect (union set1 '(5 6 7 8)) '(1 2 3 4 5 6 7 8))
(check-expect (union set1 '(1 2 3 4 5 6 7 8)) '(1 2 3 4 5 6 7 8))
(define (union s1 s2)
  #;(foldr (λ (x rst) (if (contains? s2 x)
                        rst
                        (cons x rst))) s2 s1)
  (append (filter (λ (x) (not (contains? s2 x))) s1) s2)) 









