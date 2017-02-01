;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Problem set 7|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/abstraction)

;; Problem 1

;; String -> String
;; to make a palindrome of a given string by mirroring around
;; the last letter
(define (make-palindrome s)
  (implode (append (explode s) (rest (reverse (explode s))))))

(check-expect (make-palindrome "fundies") "fundieseidnuf")

;; String -> Boolean
;; to determine whether the given string is a palindrome
(define (is-palindrome? s)
  (string=? s (implode (reverse (explode s)))))

(check-expect (is-palindrome? "abcd") false)
(check-expect (is-palindrome? "fundieseidnuf") true)


;; Problem 2

;; Number -> Boolean
;; to determine if the given natural number is prime
(define (prime? n)
  (cond [(or (zero? n) (zero? (sub1 n))) false]              
        [else (local [(define (prime1? m n)
                        (cond [(zero? (sub1 m)) true]
                              [(= (/ n m) (round (/ n m))) false]
                              [else (prime1? (sub1 m) n)]))]
                (prime1? (round (sqrt n)) n))]))

(check-expect (prime? 11) true)
(check-expect (prime? 13) true)
(check-expect (prime? 5) true)
(check-expect (prime? 6) false)
(check-expect (prime? 1) false)

;; Number -> [List of Number]
;; to produce a list of prime numbers up to n
(define (list-primes n)
  (filter prime? (build-list n add1)))

(check-expect (list-primes 8) (list 2 3 5 7))
(check-expect (list-primes 15) (list 2 3 5 7 11 13))


;; Problem 3

;; Number -> Number
;; to return the corresponding fibonacci number from a given number
(define (fibonacci n)
  (cond [(zero? n) 0]
        [(zero? (sub1 n)) 1]
        [else (+ (fibonacci (sub1 n)) (fibonacci (sub1 (sub1 n))))]))

(check-expect (fibonacci 1) 1) 
(check-expect (fibonacci 2) 1) 
(check-expect (fibonacci 11) 89)

;; Number -> Number
;; to return the corresponding fibonacci number from a given number
;; using an accumulator
(define (fibonacci1 n)
  (local [(define (f-helper a b n)
            (cond [(zero? n) a]
                  [else (f-helper b (+ a b) (sub1 n))]))]
    (f-helper 0 1 n)))

(check-expect (fibonacci1 1) 1)
(check-expect (fibonacci1 2) 1)
(check-expect (fibonacci1 11) 89)

;; Number -> [List of Number]
;; to create a list of fibonacci numbers from F0 to Fn
(define (list-fibonacci n)
  (build-list n fibonacci1))

(check-expect (list-fibonacci 6) '(0 1 1 2 3 5))


;; Problem 4

;; A Card is a (make-card value suit)
;; Interp.
;;   a value is one of:
;;   2-10
;;   JACK
;;   QUEEN
;;   KING
;;   ACE
;;   a suit is one of:
;;   CLUBS
;;   DIAMONDS
;;   HEARTS
;;   SPADES
(define-struct card [value suit])

(define JACK 11)
(define QUEEN 12)
(define KING 13)
(define ACE 14)

(define CLUBS 1)
(define DIAMONDS 2)
(define HEARTS 3)
(define SPADES 4)

;; A Player is a (make-player String Card)
;; interp. string is the player's name
(define-struct player [name card])

(define SETH (make-player "Seth" (make-card 2 DIAMONDS)))
(define SETH2 (make-player "Seth" (make-card ACE SPADES)))
(define KEVIN (make-player "Kevin" (make-card 1 DIAMONDS)))
(define KEVIN2 (make-player "Kevin" (make-card 2 CLUBS)))
(define BEN (make-player "Ben" (make-card 1 HEARTS)))
(define SARAH (make-player "Sarah" (make-card 2 SPADES)))
(define ABBY (make-player "Abby" (make-card ACE CLUBS)))

;; A Trick is a [List-of Player] with a length of 4
(define TRICK1 (list SETH KEVIN BEN SARAH))
(define TRICK2 (list ABBY SETH KEVIN BEN))
(define TRICK3 (list SETH KEVIN BEN ABBY))
(define TRICK4 (list SETH KEVIN BEN KEVIN2))
(define TRICK5 (list SETH2 KEVIN BEN SARAH))

;; A Game is a [List-of Trick]
(define GAME1 (list TRICK1 TRICK2 TRICK3))
(define GAME2 (list TRICK1 TRICK2))
(define GAME3 (list TRICK1 TRICK4 TRICK5))

;; high-card-victor : Trick -> Player
;; determines the winner in a trick,
;;   with the winner being the card of highest value

(check-expect (high-card-victor TRICK1)
              SARAH)
(check-expect (high-card-victor TRICK2)
              ABBY)
(check-expect (high-card-victor TRICK3)
              ABBY)
(check-expect (high-card-victor TRICK4)
              SETH)
(check-expect (high-card-victor TRICK5)
              SETH2)

(define (high-card-victor atrick)
  (local (;; Player Player -> Player
          ;; checks which player has the winning card
          (define (victor? aplayer1 aplayer2)
            (if (equal?
                 (better-card (player-card aplayer1) (player-card aplayer2))
                 (player-card aplayer1))
                aplayer1
                aplayer2)))
    (foldr victor? (first atrick) (rest atrick))))

;; better-card : Card Card -> Card
;; checks which card wins by having higher value/suit

(check-expect (better-card (make-card 10 SPADES)
                           (make-card 9 SPADES))
              (make-card 10 SPADES))
(check-expect (better-card (make-card 9 SPADES)
                           (make-card 10 SPADES))
              (make-card 10 SPADES))
(check-expect (better-card (make-card 10 CLUBS)
                           (make-card 10 SPADES))
              (make-card 10 SPADES))
 
(define (better-card acard1 acard2)
  (cond [(< (card-value acard1)
            (card-value acard2)) acard2]
        [(> (card-value acard1)
            (card-value acard2)) acard1]
        [(= (card-value acard1)
            (card-value acard2)) (better-suit acard1 acard2)]))

;; better-suit : Card Card -> [Maybe-Card]
;; checks which card has a better suit
;; if two cards have the same suit and number, error given

(check-expect (better-card (make-card 10 CLUBS)
                           (make-card 10 SPADES))
              (make-card 10 SPADES))
(check-expect (better-card (make-card 10 SPADES)
                           (make-card 10 CLUBS))
              (make-card 10 SPADES))

(define (better-suit acard1 acard2)
  (cond [(<= (card-suit acard1)
             (card-suit acard2)) acard2]
        [(> (card-suit acard1)
            (card-suit acard2)) acard1]))

;; game-winner : Game -> String
;; determines who wins a game by winning the most tricks,
;;   reports the winning player's name or reports a tie if no player wins.

(check-expect (game-winner GAME1) "Abby")
(check-expect (game-winner GAME2) "Tie Game")
(check-expect (game-winner GAME3) "Seth")

(define (game-winner agame)
  (if (empty? (the-winner (winner-list agame empty)))
      "Tie Game"
      (first (the-winner (winner-list agame empty)))))

;; the-winner : [List-of String] -> [List-of String]
;; outputs a list with the most frequent winner in a list of players
;;   and reports an empty list in the case of a tie

(check-expect (the-winner (list "Abby" "Sarah" "Abby" "Sarah" "Abby"))
              (list "Abby"))
(check-expect (the-winner (list "Abby" "Sarah")) '())

(define (the-winner alop)
   (cond [(and (= (length alop) 2) (not (string=? (first alop) (second alop))))
           '()]
          [(> (length alop) 1)
           (the-winner (repeat-winner? (sort alop string<?)))]
          [else alop]))

;; repeat-winner? [List-of String] -> [List-of String]
;; consolidates repeated wins in an alphabetized list of player names
;;   and removes players with no repeat wins

(check-expect (repeat-winner? (list "Abby" "Abby" "Abby" "Sarah" "Sarah"))
              (list "Abby" "Abby" "Sarah"))
(check-expect (repeat-winner? (list "Abby" "Abby" "Ben")) (list"Abby"))

(define (repeat-winner? alop)
  (cond [(empty? (rest alop)) empty]
        [else (if (string=? (first alop) (second alop))
                  (cons (first alop) (repeat-winner? (rest alop)))
                  (repeat-winner? (rest alop)))]))

;; winner-list : Game [List-of String] -> [List-of String]
;; adds the names of winners of tricks in a game to a list of names

(check-expect (winner-list '() (list "Seth")) (list "Seth"))
(check-expect (winner-list GAME1 '()) (list "Abby" "Abby" "Sarah"))
(check-expect (winner-list GAME3 '()) (list "Seth" "Seth" "Sarah"))
              
(define (winner-list agame alist)
  (cond [(empty? agame) alist]
        [else (winner-list (rest agame)
                           (cons (player-name (high-card-victor (first agame)))
                                 alist))]))


;; Problem 5

;; A Tweeps is one of:
;; - empty
;; - (list String [List of String])

;; An User is (make-user String [List of String])

(define-struct user (handle tweeps))

;; A Network is one of
;; - empty
;; - (list User [List-of Tweep])

(define network1
  (list (make-user "Kevin" '())
        (make-user "Jack" (list "John" "Kevin" "Ted"))
        (make-user "Ted" (list "Kevin" "John"))
        (make-user "Lucy" (list "John"))
        (make-user "John" (list "Jack" "Lucy"))))

;; Network -> [List of String]
;; creates a list of handles from all users in the network

(define (list-handles anw)
  (local [(define (names anw acc)
            (cond [(empty? anw) acc]
                  [else
                   (names (rest anw) (cons (user-handle (first anw)) acc))]))]
    (names anw '())))

(check-expect (list-handles empty) empty)
(check-expect (list-handles network1) (list "John" "Lucy" "Ted" "Jack" "Kevin"))

;; most-followers: Network -> String
;; produces the handle of the user with the most folowers in the network

(check-expect (most-followers network1) "Jack")

(define (most-followers anw)
  (local [; most-followers-helper : Network -> [List-of String]???????
          ; 
          (define (most-followers-helper anw acc)
            (cond [(empty? anw) (user-handle acc)]
                  [else (if (< (length (user-tweeps (first anw)))
                               (length (user-tweeps acc)))
                            (most-followers-helper (rest anw) acc)
                            (most-followers-helper (rest anw) (first anw)))]))]
    (most-followers-helper (rest anw) (first anw))))
      

;; friends?: Network -> Boolean
;; checks if a network has two mutual followers

(check-expect (friends? '()) false)
(check-expect (friends? (list (make-user "Kevin" (list "Jack")))) false)
(check-expect (friends? network1) true)

(define (friends? anw)
  (cond
    [(empty? anw) false]
    [(empty? (rest anw)) false]
    [(2-friends? (first anw) (second anw)) true]
    [else (friends? (rest anw))]))

;; 2-friends? : User User -> Boolean
;; checks if two users are in each other's list of tweeps

(define (2-friends? user1 user2)
  (and (member? (user-handle user1) (user-tweeps user2))
       (member? (user-handle user2) (user-tweeps user1))))


(check-expect (2-friends? (make-user "Kevin" (list "Jack" "Ted"))
                          (make-user "Jack" (list "John" "Kevin" "Ted"))) true)