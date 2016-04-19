#lang racket

(include "assoc.rkt")
(include "directions.rkt")
(include "objects.rkt")
(include "maze.rkt")

(require srfi/1)
(require srfi/13)
(require srfi/48)



;; MAZE THINGS


(define rooms (make-hash))
(define m (build-maze X Y))

(define (assq-ref assqlist id)
  (cadr (assq id assqlist)))

(define (room-allocator db types)
  (for ((j X))
    (for ((i Y))
      (hash-set! db (list j i) (assq-ref types (random (- (length types) 1)))))))

(room-allocator rooms room-type) ;;allocate names to the rooms

;;START OF OBJECTS FUNCTIONS

(define objectdb (make-hash))  ;;define object hash
(define inventorydb (make-hash)) ;;define bag hash

(add-objects objectdb) ;;insert objects in the object hash


;;refactored functions assq-ref and assv-ref into only one ass-ref
;; we pass what we want as parameter (assq or assv)
(define (ass-ref assqlist id x)
  (cdr (x id assqlist)))
;;old version


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


;;(define (lookup id tokens)
 ;; (let* ((record (ass-ref decisiontable id assv))
 ;;        (keylist (get-keywords id))
 ;;        (index (index-of-largest-number (list-of-lengths keylist tokens))))
  ;;  (if index 
  ;;    (cadr (list-ref record index))
   ;;   #f)))


(define (startgame-old initial-id)
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





(define (startgame-new room-id)
  (let loop ((rid room-id))
    (show-maze m rid)
    (printf "You are in the ~a \n>" (hash-ref rooms rid))
    (let ((input (read)))
      (cond [(eq? input 'quit) (exit)]) ;; help with paths
      (if (member input (paths rid))
          (let ((direction (lookup rid input)))
          (cond ((equal? rid direction) (loop rid))
                ((equal? direction (list (- X 1) (- Y 1)))
                 (show-maze m direction)
                 (displayln "You have reached the exit door.")
                 (exit))
                (else
                 (loop direction))))
      (begin
        (printf "huh? I did not understand ~a \n" input)
        (loop rid))))))
    


(startgame-new start)
