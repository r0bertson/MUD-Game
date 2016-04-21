#lang racket

(include "assoc.rkt")
(include "directions.rkt")
(include "objects.rkt")
(include "maze.rkt")

(require srfi/1)
(require srfi/13)
(require srfi/48)



;; MAZE THINGS

(define objectdb (make-hash))  ;;define object hash
(define inventorydb (make-hash)) ;;define bag hash
(define rooms (make-hash)) ;; define hash for carry the rooms names


(define (assq-ref assqlist id)
  (cadr (assq id assqlist)))

(define m (build-maze X Y))


(define (random-allocator db types rate)
  (for ((j X))
    (for ((i Y))
      (cond ((<= (random 100) rate)
             (cond((equal? db rooms)
                 (hash-set! db (list j i) (assq-ref types (random (- (length types) 1)))))
                  (else
                 (add-object db (list j i) (assq-ref types (random (- (length types) 1)))))))))))
 

(random-allocator rooms room-type 100)   ;;allocate names to the rooms
(random-allocator objectdb objects 50)  ;;allocate items to the rooms
;;START OF OBJECTS FUNCTIONS



;;insert objects in the object hash


;;refactored functions assq-ref and assv-ref into only one ass-ref
;; we pass what we want as parameter (assq or assv)
(define (ass-ref assqlist id x)
  (cdr (x id assqlist)))
;;old version


;;(define (get-response id)
;;  (car (ass-ref descriptions id assq)))

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

(define (lookup room direction)
  (cond [(eq? direction 'south)
         (move-x room +)]
        [(eq? direction 'north)
         (move-x room -)]
        [(eq? direction 'west)
         (move-y room -)]
        [(eq? direction 'east)
         (move-y room +)]
        [(call-actions room direction)]))


(define (call-actions id tokens)
 (let* ((record (ass-ref decisiontable 1 assv))
        (keylist (get-keywords 1))
        (index (index-of-largest-number (list-of-lengths keylist tokens))))
  (if index 
    (cadr (list-ref record index))
   #f)))

;;; ADVANCED COMMAND LINE PROCESSOR, WITHOUT MAZE
;;;(define (startgame-old initial-id)
;;;  (let loop ((id initial-id) (description #t))
;    (if description
;       ;; (printf "~a\n> " (get-response id))
;        (printf "> "))
;    (let* ((input (read-line))
;           (string-tokens (string-tokenize input))
;           (tokens (map string->symbol string-tokens)))
;      (let ((response (lookup id tokens)))
;        (cond ((number? response)
;               (loop response #t))
;              
;              ((eq? #f response)
;               (format #t "huh? I didn't understand that!\n")
;               (loop id #f))
;              
;              ((eq? response 'look)
;               (get-directions id)
;               (display-objects objectdb id)
;               (loop id #f))
;              
;              ((eq? response 'pick)
;               (pick-item id input)
;               (loop id #f))
;              
;              ((eq? response 'inventory)
;               (display-inventory)
;               (loop id #f))
;              
;              ((eq? response 'drop)
;               (put-item id input)
;               (loop id #f))
;              
;              ((eq? response 'quit)
;               (format #t "So Long, and Thanks for All the Fish...\n")
;               (exit)))))))



;; ADVANCED COMMAND LINE PROCESSOR WITH MAZE
(define (startgame-maze start)
  (let loop ((rid start))
    (printf "ROOM ID:  ~a \n>" rid)
       (show-maze m rid)
       (printf "You are in the ~a \n>" (hash-ref rooms rid))
    (let* ((input (read-line))
           (string-tokens (string-tokenize input))
           (tokens (map string->symbol string-tokens))
           (response (lookup rid tokens)))
        (cond ((member (car tokens) (paths rid))
              (let ((direction response))
                (cond ((equal? rid direction) (loop rid))
                      ((equal? direction (list (- X 1) (- Y 1)))
                       (show-maze m direction)
                       (displayln "You have reached the exit door.")
                       (exit))
                      (else
                       (loop direction)))))
              
              ((eq? #f response)
               (format #t "huh? I didn't understand that!\n")
               (loop rid))
              
              ((eq? response 'look)
               (display-objects objectdb rid)
               (loop rid))
              
              ((eq? response 'pick)
               (handle-item 'room rid input)
               (loop rid))
              
              ((eq? response 'inventory)
               (display-inventory)
               (loop rid))
              
              ((eq? response 'quit)
               (format #t "So Long, and Thanks for All the Fish...\n")
               (exit))
            
              ((eq? response 'drop)
               (handle-item 'bag rid input)
               (loop rid))))))



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
    


;;(startgame-new start)
(startgame-maze start)