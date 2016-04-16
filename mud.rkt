#lang racket

(include "assoc.rkt")
(include "directions.rkt")
(include "objects.rkt")

(require srfi/1)
(require srfi/13)
(require srfi/48)

;;START OF OBJECTS FUNCTIONS

(define objectdb (make-hash)) 
(define inventorydb (make-hash))

(add-objects objectdb)




;;refactored functions assq-ref and assv-ref into only one ass-ref
;; we pass what we want as parameter (assq or assv)
(define (ass-ref assqlist id x)
  (cdr (x id assqlist)))

(define (get-response id)
  (car (ass-ref descriptions id assq)))

(define (get-keywords id)
  (let ((keys (ass-ref decisiontable id assq)))
    (map (lambda (key) (car key)) keys)))


;; outputs a list in the form: (0 0 0 2 0 0)
(define (list-of-lengths keylist tokens)
  (map 
   (lambda (x)
     (let ((set (lset-intersection eq? tokens x)))
       ;; apply some weighting to the result
       (* (/ (length set) (length x)) (length set))))
   keylist))

(define (index-of-largest-number list-of-numbers)
  (let ((n (car (sort list-of-numbers >))))
    (if (zero? n)
      #f
      (list-index (lambda (x) (eq? x n)) list-of-numbers))))


(define (lookup id tokens)
  (let* ((record (ass-ref decisiontable id assv))
         (keylist (get-keywords id))
         (index (index-of-largest-number (list-of-lengths keylist tokens))))
    (if index 
      (cadr (list-ref record index))
      #f)))


(define (startgame initial-id)
  (let loop ((id initial-id) (description #t))
    (if description
        (printf "~a\n> " (get-response id))
        (printf "> "))
    (let* ((input (read-line))
           (string-tokens (string-tokenize input))
           (tokens (map string->symbol string-tokens)))
      (let ((response (lookup id tokens)))
        (cond ((number? response)
               (loop response #t))
              
              ((eq? #f response)
               (format #t "huh? I didn't understand that!\n")
               (loop id #f))
              
              ((eq? response 'look)
               (get-directions id)
               (display-objects objectdb id)
               (loop id #f))
              
              ((eq? response 'pick)
               (pick-item id input)
               (loop id #f))
              
              ((eq? response 'inventory)
               (display-inventory)
               (loop id #f))
              
              ((eq? response 'drop)
               (put-item id input)
               (loop id #f))
              
              ((eq? response 'quit)
               (format #t "So Long, and Thanks for All the Fish...\n")
               (exit)))))))


(startgame 1)
