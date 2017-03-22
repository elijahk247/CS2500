;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mutual_recursion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A [Set X] is [X -> Boolean]

;; A [Set Number] is [Number -> Boolean]

(define evens (λ (x) (and (integer? x) (even? x))))
(define odds (λ (x) (and (integer? x) (odd? x))))
(define 5to10 (λ (x) (and (>= x 5) (<= x 10))))

;; contains2 : [Set X] X -> Boolean
;; is the item in the set?
(check-expect (contains2 evens 2) true)
(check-expect (contains2 evens 3) false)
(define (contains2 aset an-item)
  (aset an-item))

;; intersect2 : [Set X] [Set X] -> [Set X]
;; items that are in both sets
(check-expect ((intersect2 evens odds) 2) false)
(check-expect ((intersect2 evens 5to10) 8) true)
(define (intersect2 s1 s2)
  (λ (x) (and (s1 x) (s2 x))))

;; union : [Set X] [Set X] -> [Set X]
;; all items in set1 and all items in set2
(check-expect ((union2 evens odds) 2) true)
(define (union2 s1 s2)
  (λ (x) (or (s1 x) (s2 x))))


;; An Atom is one of:
;; - String
;; - Symbol
;; - Number

;; Any -> Boolean
;; is it an atom?
(check-expect (atom? 5) true)
(check-expect (atom? true) false)
(check-expect (atom? atom?) false)
(define (atom? x)
  (or (string? x) (number? x) (symbol? x)))

;; atom=? : Atom Atom -> Boolean
;; are the two atoms the same?
(check-expect (atom=? 5 '5x) false)
(define (atom=? a1 a2)
  (cond [(string? a1) (and (string? a2) (string=? a1 a2))]
        [(symbol? a1) (and (symbol? a2) (symbol=? a1 a2))]
        [(number? a1) (and (number? a2) (= a1 a2))]))

;; An S-exp is one of:
;; - Atom
;; - [List-of S-exp]

;; A [List-of S-exp] is one of
;; - empty
;; - (cons S-exp [List-of S-exp])

#;(define (s-exp-temp s)
  (cond [(atom? s) ...]
        [(list? s) ...(los-temp s)]))

#;(define (los-temp alos)
  (cond [(empty? alos) ...]
        [(cons? alos) ...(s-exp-temp (first alos))
                      ...(los-temp (rest alos))]))

5
"a"
'(1 2 3)
'(1 (2 3) "hello")
'(+ 2 3)
(define s1 '(+ 2 (- 3 4) (* 6 7)))

;; occurs: S-exp Atom -> Boolean
;; is the atom in the s-exp?
(check-expect (occurs s1 '*) true)
(check-expect (occurs s1 "p") false)
(define (occurs s a)
  (cond [(atom? s) (atom=? s a)]
        [(list? s) (occurs-in-list s a)]))

;; occurs-in-list : [List-of S-exp] Atom -> Boolean
;; is the atom in the list?
(define (occurs-in-list alos a)
  (ormap (λ (x) (occurs x a)) alos)
  #;(cond [(empty? alos) false]
        [(cons? alos) (or (occurs (first alos) a)
                          (occurs-in-list (rest alos) a))]))

;; A Person is a (make-person String Number [List-of Person])
(define-struct person (name yob children))
;; A [List-of Person] is one of:
;; - empty
;; - (cons Person [List-of Person])

(define sally (make-person "sally" 1960 
                           (list (make-person "alice" 1980 '())
                                 (make-person "bob" 1978 
                                              (list (make-person "bobby" 1999 '()))))))
;; Person Number -> Number
;; count the people born before the given year
(check-expect (count-p sally 1980) 2)
(define (count-p aper year)
  (if (< (person-yob aper) year)
      (+ 1 (count-children (person-children aper) year))
      (count-children (person-children aper) year)))

;; [List-of Person] Number -> Number
;; count the people in the list born before the given year
(define (count-children aloc year)
  ;; [Person Number -> Number] Number [List of Person] -> Number
  (foldr (lambda (p n) (+ (count-p p year) n)) 0 aloc)
  #;(cond [(empty? aloc) 0]
        [else (+ (count-p (first aloc) year)
                 (count-children (rest aloc) year))]))


;; Person -> Person
;; adds one to everyone's year of birth

(check-expect (add-to-years sally)
              (make-person "sally" 1960
                           (list (make-person "alice" 1980 '())
                                 (make-person "bob" 1979
                                              (list (make-person "bobby" 2000 '()))))))

(define (add-to-years aperson)
  (... (person-name aperson)
       (person-yob aperson)
       (add-to-years-list (person-children aperson))))

;; [List of Person] -> [List of Person]
;; add one to everyone's yob
(check-expect (add-to-years-list (person-children sally))
              (list (make-person "alice" 1981 '())
                    (make-person "bob" 1979
                                 (list (make-person "bobby" 2000 '())))))

(define (add-to-years-list aloc)
  (cond [(empty? aloc) empty]
        [else (cons (add-to-years (first aloc))
                    (add-to-years-list (rest aloc)))]))

(define (add-to-years aloc num)
  ;; [Person -> Person] [List of Person] -> [List of Person]
  (map (lambda





