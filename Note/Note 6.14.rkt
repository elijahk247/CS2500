;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Note 6.14|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define (pow m n)
  (cond [(zero? n) 1]
        [else (* m (pow m (sub1 n)))]))


(define (pow-gen m n)
  (cond [(zero? n) 1]
        [(even? n) (pow-gen (sqr m) (/ n 2))]
        [(odd? n) (* m (pow-gen (sqr m) (/ (sub1 n) 2)))]))


;; finds the number close to the root of given function

(define (find-root f lo hi delta)
  (local [(define width (- hi lo))
          (define num-windows (floor (/ width delta)))
          (define (mk-windows i) (+ lo (* i delta)))
          (define intervals (build-list num-windows mk-windows))
          (define (good window)
            (<= (* (f window) (f (+ window delta))) 0))]
    (first (filter good intervals))))

;; find a number close to the root of given function
;; Method: binary search

(define (find-root-gen f lo hi delta)
  (cond [(<= (- hi lo) delta) lo]
        [else (local [(define mid (/ (+ hi lo) 2))]
                (if (<= (* (f lo) (f mid)) 0)
                    (find-root-gen f lo mid delta)
                    (find-root-gen f mid hi delta)))]))

;; find the greatest common divisor of the two numbers

(define (gcd-struct m n)
  (local [(define (find-divisor i)
            (cond [(= 1 i) 1]
                  [else (if (and (= (remainder n i) 0)
                                 (= (remainder m i) 0))
                            i
                            (find-divisor (sub1 i)))]))]
  (find-divisor (min m n))))
              
          
            
                                     