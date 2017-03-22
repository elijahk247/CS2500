;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Note: some of the functions below would be better written with helpers. 


;; an AE is one of:
;; Number
;; (make-add AE AE)
;; (make-mult AE AE)
(define-struct add (left right))
(define-struct mult (left right))

(define ae1 3)
(define ae2 (make-add ae1 5))
(define ae3 (make-mult (make-mult ae2 ae1) (make-add ae1 ae2)))

;; ae-temp : AE -> ?
#;(define (ae-temp an-ae)
  (cond [(number? an-ae) ...]
        [(add? an-ae) ...(ae-temp (add-left an-ae))
                      ...(ae-temp (add-right an-ae))]
        [(mult? an-ae) ...(ae-temp (mult-left an-ae))
                       ...(ae-temp (mult-right an-ae))]))

;; AE -> Number
;; compute the value of the expression
(check-expect (evaluate ae1) 3)
(check-expect (evaluate ae2) 8)
(check-expect (evaluate ae3) 264)
(define (evaluate an-ae)
  (cond [(number? an-ae) an-ae]
        [(add? an-ae) (+ (evaluate (add-left an-ae))
                         (evaluate (add-right an-ae)))]
        [(mult? an-ae) (* (evaluate (mult-left an-ae))
                          (evaluate (mult-right an-ae)))]))

;; A BST is one of:
;; (make-leaf)
;; (make-branch Natural String BST BST)
;; constraint -
;;             1. every key on left of the root is less than the root
;;             2. every key on right of the root is greater than the root
;;             3. 1 and 2 are true for every subtree
(define-struct leaf ())
(define-struct branch (key info left right))

(define lf (make-leaf))
(define bst1 (make-branch 10 "a"
                          (make-branch 8 "b"
                                       (make-branch 7 "c" lf lf)
                                       lf)
                          (make-branch 12 "d"
                                       lf
                                       (make-branch 14 "e"
                                                    (make-branch 13 "f" lf lf)
                                                    lf))))
(define bad-bst (make-branch 10 "a"
                          (make-branch 12 "b"
                                       (make-branch 7 "c" lf lf)
                                       lf)
                          (make-branch 8 "d"
                                       lf
                                       (make-branch 14 "e"
                                                    (make-branch 13 "f" lf lf)
                                                    lf))))
(define bad-bst2 (make-branch 10 "a"
                          (make-branch 8 "b"
                                       (make-branch 7 "c" lf lf)
                                       lf)
                          (make-branch 12 "d"
                                       lf
                                       (make-branch 14 "e"
                                                    (make-branch 13 "f" lf lf)
                                                    (make-branch 11 "j" lf lf)))))

#;(define (bst-temp abst)
  (cond [(leaf? abst) ...]
        [(branch? abst) ...(branch-key abst)
                        ...(branch-info abst)
                        ...(bst-temp (branch-left abst))
                        ...(bst-temp (branch-right abst))]))

;; BST Natural -> String
;; retrieve the info at the given key
(check-error (retrieve lf 4) "key not found")
(check-expect (retrieve bst1 12) "d")
(check-expect (retrieve bst1 7) "c")
(check-error (retrieve bst1 15) "key not found")
(define (retrieve abst akey)
  (cond [(leaf? abst) (error "key not found")]
        [(branch? abst) (cond [(= akey (branch-key abst))
                               (branch-info abst)]
                              [(< akey (branch-key abst))
                               (retrieve (branch-left abst) akey)]
                              [else (retrieve (branch-right abst) akey)])]))

;; grow : BST Number String -> BST
(check-expect (grow bst1 9 "g")
              (make-branch 10 "a"
                           (make-branch 8 "b"
                                        (make-branch 7 "c" lf lf)
                                        (make-branch 9 "g" lf lf))
                           (make-branch 12 "d"
                                        lf
                                        (make-branch 14 "e"
                                                     (make-branch 13 "f" lf lf)
                                                     lf))))
(check-expect (grow lf 1 "b")
              (make-branch 1 "b" lf lf))
(check-expect (grow bst1 11 "s")
              (make-branch 10 "a"
                           (make-branch 8 "b"
                                        (make-branch 7 "c" lf lf)
                                        lf)
                           (make-branch 12 "d"
                                        (make-branch 11 "s" lf lf)
                                        (make-branch 14 "e"
                                                     (make-branch 13 "f" lf lf)
                                                     lf))))
(define (grow abst akey an-info)
  (cond [(leaf? abst) (make-branch akey an-info lf lf)]
        [(branch? abst) (cond [(= (branch-key abst) akey)
                               (make-branch akey an-info 
                                            (branch-left abst) (branch-right abst))]
                              [(> (branch-key abst) akey)
                               (make-branch (branch-key abst) (branch-info abst)
                                            (grow (branch-left abst) akey an-info)
                                            (branch-right abst))]
                              [else (make-branch (branch-key abst) (branch-info abst)
                                                 (branch-left abst)
                                                 (grow (branch-right abst) 
                                                       akey an-info))])])) 

;; BST -> [List-of Number]
;; list the keys in the tree
(check-expect (list-nums lf) '())
(check-expect (list-nums bst1) '(7 8 10 12 13 14))
(define (list-nums abst)
  (cond [(leaf? abst) empty]
        [(branch? abst) (append 
                         (list-nums (branch-left abst))
                         (list (branch-key abst))
                         (list-nums (branch-right abst)))]))

;; bst? : BST -> Boolean
;; is the given tree a valid bst?
(check-expect (bst? lf) true)
(check-expect (bst? bst1) true)
(check-expect (bst? bad-bst) false)
(check-expect (bst? bad-bst2) false)
(define (bst? abst)
  (cond [(leaf? abst) true]
        [(branch? abst) 
                        (and
                        (andmap (λ(x) (< x (branch-key abst))) 
                                (list-nums (branch-left abst)))
                        (andmap (λ(x) (> x (branch-key abst))) ;can this be done with foldr instead?
                                (list-nums (branch-right abst)))
                        (bst? (branch-left abst))
                        (bst? (branch-right abst)))])) 
                       
