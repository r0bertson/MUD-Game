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

(define (add-object db id object)
   (if (hash-has-key? db id)
        (let ((record (hash-ref db id)))
           (hash-set! db id (cons object record)))
        (hash-set! db id (cons object empty ))))

(define (add-objects db)
   (for-each
     (lambda (r)
        (add-object db (first r) (second r))) objects))

(add-objects objectdb)

(define (display-objects db id)
   (when (hash-has-key? db id)
      (let* ((record (hash-ref db id))
               (output (string-join record " and ")))
         (when (not (equal? output ""))
            (if (eq? id 'bag)
                 (printf "You are carrying ~a. \n" output)
                 (printf "You can see ~a. \n" output))))))


(define (remove-object-from-room db id str)
   (when (hash-has-key? db id)
      (let* ((record (hash-ref db id))
               (result (remove (lambda (x) (string-suffix-ci? str x)) record ))
               (item (lset-difference equal? record result )))
         (cond ((null? item)
                 (printf "I don't see that item in the room! \n"))
                (else
                  (printf "Added ~a to your bag.\n" (first item))
                  (add-object inventorydb 'bag (first item))
                  (hash-set! db id result))))))

(define (remove-object-from-inventory db id str)
   (when (hash-has-key? db 'bag)
      (let* ((record (hash-ref db 'bag))
               (result (remove (lambda (x) (string-suffix-ci? str x)) record))
               (item (lset-difference equal? record result)))
         (cond ((null? item)
                 (printf "You are not carrying that item ! \n"))
                ( else
                  (printf "Removed ~a from your bag . \n" (first item))
                  (add-object objectdb id (first item))
                  (hash-set! db 'bag result))))))

(define (pick-item id input)
   (let ((item (string-join (cdr (string-split input)))))
      (remove-object-from-room objectdb id item )))

(define (put-item id input)
   (let ((item (string-join (cdr (string-split input)))))
      (remove-object-from-inventory inventorydb id item)))

(define (display-inventory)
   (display-objects inventorydb 'bag))


;;END OF OBJECTS FUNCTIONS



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

;(startgame 1)
