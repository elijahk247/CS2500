;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Note June 6|) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t quasiquote repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Number Number Number Number -> Number
;; computes the distance between two points
;(check-expect (dist 3 4 0 0) 5)

;;(define (dist x1 y1 x2 y2)
 ; (local [(define deltax (- x1 x2))
  ;        (define deltax (- y1 y2))
   ;       (sqrt (sqrx sqry))

(define (largest alon)
  (cond [(empty? (rest alon)) (first alon)]
        [else (if (< (first alon) (largest (rest alon)))
                  (largest (rest alon))
                  (first alon))]))
                 
                        

;; largest2: [NEList of Number] -> Number
;; get the largest number in the list
(check-expect (largest2 '(4)) 4)
(check-expect (largest2 '(1 2 3 4)) 4)
(check-expect (largest2 '(4 3 2 1)) 4)
(check-expect (largest2 '(1 2 3 4 5 6 7 8 9 10)) 10)

(define (largest2 alon)
  (cond [(empty? (rest alon)) (first alon)]
        [else (local [(define first (first alon))
                      (define max (largest2 (rest alon)))]
                (if (> max first) max first))]))

                      
;; my-filter : [X -> Boolean] [List of X] -> [List of X]
;; filters the list based on the predicate
(check-expect (my-filter even? '(1 2 3 4)) '(2 4))
(check-expect (my-filter even? '()) '())
(define (my-filter pred alox)
  （foldr (λ(x y) (if (pred x)
                      (cons x y)
                      y)) empty alox)

;; [List of Posn] -> Number
;; add up the xs of the posns
(check-expect (sum-xs empty) 0)
(check-expect (sum-xs (list (make-posn 3 4) (make-posn 6 8))) 9)
(define (sum-xs alop)
  ;; [Posn Number -> Number] Number [List of Posn
  (foldr (λ (p y) (+ (posn-x p) y)) 0 alop))
;; (foldr + 0 (map posn-x alop)))



;; subset : set1 set2 -> Boolean
;; is set1 a subset of set2
(check-expect (subset? set1 '(1 2)) false)
(check-expect (subset? '(1 2) set1) true)
(check-expect (subset? '() set1) true)
(define (subset? s1 s2)
  (filter (λ (x) (contains? s2 x)) s1))

;; set-equal? : Set Set -> Boolean
;; are the two sets equal?
(check-expect (set-equal? set1 set1) true)
(check-expect (set-equal? set1 '(4 3 2 1)) true)
(define (set-equal? s1 s2)
  ;; (and (sublet? s1 s2) (subset s2 s1)))
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
(define (union s1 s2)
  #; (foldr (λ (x rst) (if (contains? s2 x)
                           rst
                           (cons x rst))) s2 s1)
  (append (filter (λ (x) (not (contains? s2 x))) s1) s2))


;; A [Set X] is [X -> Boolean]
;; A [Set Number] is [Number -> Boolean]

(define evens (λ (x) (and (integer? x) (even? x))))

;; contains2 : [Set X] X -> Boolean
;; is the item in the set
(check-expect (contains2 evens 2) true)
(check-expect (contains2 evens 3) false)
(define (contains 2 aset an-item)
  (aset an-item))

;; intersect2 : [Set X] [Set X] -> [Set X]
;; items that are in both sets
;(check-expect (intersect2 evens odds)
(check-expect
(define (intersect2 s1 s2)
  (λ (x) (and (s1 x) (s2 x))))

