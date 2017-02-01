;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Problem Set 3|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; Problem Set 3 - Changzong Liu

;; Problem 1
(define-struct movie (type year price))

(define (pay amovie)
  (cond [(string=? (movie-type amovie) "classic") (movie-price amovie)]
        [(string=? (movie-type amovie) "regular") (discount amovie)]
        [else "error"]))

(define (discount amovie)
  (if (<= (* (movie-price amovie) (expt (- 1 0.035) (- 2016 (movie-year amovie)))) 2)
      2
      (* (movie-price amovie) (expt (- 1 0.035) (- 2016 (movie-year amovie))))
      ))



(check-expect (pay (make-movie "regular" 2016 1)) 2)
(check-expect (pay (make-movie "regular" 2015 10)) 9.65)
(check-expect (pay (make-movie "classic" 1978 15)) 15)

 
;; Problem 3
(define (check-pass-6-10? lop)
  (cond [(empty? lop) false]
        [(cons? lop)
         (or (<= 6 (string-length (first lop)) 10)
                    (check-pass-6-10? (rest lop)))]))
            
          
                                                    
(check-expect (check-pass-6-10? (list "123456" "1235424")) true)
(check-expect (check-pass-6-10? (list "456" "12354632334")) false)
(check-expect (check-pass-6-10? (list "45321312131231236" "123")) false)

;;Problem 4

;; A list-of-symbol is one of:
;; - empty
;; - (cons symbol list-of-symbol)

(define list1 (cons 'wurst (cons 'huevos (cons 'pizza (cons 'pants empty)))))
(define list2 empty)

;; Cesarify: list-of-symbol->list-of-symbol
;; Given a list of symbols and returns the same list but 
;; with every instance of 'pizza doubled
  
(define (cesarify a)
  (cond [(empty? a) empty]
        [(cons? a) 
         (cond [(symbol=? (first a) 'pizza)
                (cons 'pizza (cons 'pizza (rest a)))]
               [else (cons (first a)
                           (cesarify (rest a)))])]))


(check-expect (cesarify ex4) (cons 'wurst 
                                   (cons 'huevos 
                                                (cons 'pizza (cons 'pizza (cons 'pants empty))))))
(check-expect (cesarify ex5) empty)
