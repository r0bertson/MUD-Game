#lang racket

(include "assoc.rkt")
(include "directions.rkt")
(include "objects.rkt")
(include "maze.rkt")

(require srfi/1)
(require srfi/13)
(require srfi/48)



;; DEFINITIONS OF DATABASES AND MAP

(define objectdb (make-hash))  ;;define object hash
(define inventorydb (make-hash)) ;;define bag hash
(define rooms (make-hash)) ;; define hash for carry the rooms names
(define m (build-maze X Y)) ;;build the maze
;; END OF DEFINITIONS




;; refactored functions assq-ref and assv-ref into only one ass-ref
;; we pass what we want as parameter (assq or assv)
(define (ass-ref assqlist id x)
  (cdr (x id assqlist)))

;(random-allocator)
;randomly allocates something to a position in the maze
;a rate can be applied to allocate only to some cells (rooms)
;for instance: if the rate is 50, a room will have 50%
;chance of have an also random item.
(define (random-allocator db types rate)
  (for ((j X))
    (for ((i Y))
      (cond ((<= (random 100) rate)
             (cond((equal? db rooms) ; add the name to the room
                   (hash-set! db (list j i) (car( ass-ref types (random (- (length types) 1)) assq))))
                  (else ;add to objectdb
                   (add-object db (list j i) (car (ass-ref types (random (- (length types) 1)) assq))))))))))




(random-allocator rooms room-type 100)   ;;allocate names to the rooms
(random-allocator objectdb objects 50)   ;;allocate items to the rooms


;;get the keywords on a association table
(define (get-keywords id)
  (let ((keys (ass-ref decisiontable id assq)))
    (map (lambda (key) (car key)) keys)))


;; outputs a list in the form: (0 0 0 2 0 0) based on some weightening
(define (list-of-lengths keylist tokens)
  (map 
   (lambda (x)
     (let ((set (lset-intersection eq? tokens x)))
       ;; apply some weighting to the result
       (* (/ (length set) (length x)) (length set))))
   keylist))


;return the index of the highest number on the list provided by the function
;(list-of-lenghts)
(define (index-of-largest-number list-of-numbers)
  (let ((n (car (sort list-of-numbers >))))
    (if (zero? n)
        #f
        (list-index (lambda (x) (eq? x n)) list-of-numbers))))

;deal with room changes
;the maze is build from top left (0 0)
(define (move-room room input)
               (cond [(eq? input 'south)
                      (move-x room +)]
                     [(eq? input 'north)
                      (move-x room -)]
                     [(eq? input 'west)
                      (move-y room -)]
                     [(eq? input 'east)
                      (move-y room +)]))
             

;;changed to receive a function as parameter to reuse to get keys
;;this function can both get the actions but also the words attached to it
;;depending on the function passed
(define (call-actions id tokens func)
  (let* ((record (ass-ref decisiontable 1 assv)) ;;get the references
         (keylist (get-keywords 1)) ;;get the keywords
         ;;description in the functions
         (index (index-of-largest-number (list-of-lengths keylist tokens)))) 
    (if index 
        (func (list-ref record index)) ;;return result
        #f)))


;; ADVANCED COMMAND LINE PROCESSOR WITH MAZE
(define (startgame-maze start)
  (let loop ((rid start))
    (show-maze m rid)
    (printf "You are in the ~a \n>" (hash-ref rooms rid))
    (let* ((input (read-line))
           (string-tokens (string-tokenize input))
           (tokens (map string->symbol string-tokens))
           (response (call-actions rid tokens cadr))) ;;get action
      (cond ((eq? response 'direction)
             (let* ((direction (call-actions rid tokens caar)) ;;get direction typed
                   (newlocation (move-room rid direction)))  ;;get future location after move
               (cond((member direction (paths rid)) ;check if direction is in path
                     (cond ((equal? rid newlocation) (loop rid))
                           ((equal? newlocation (list (- X 1) (- Y 1))) ;;end of game condition
                            (show-maze m newlocation) ;;show maze once again
                            (displayln "You have reached the exit door.")
                            (exit)) ; kill
                           (else
                            (loop newlocation)))) ;;loop in the new location
                    (else
                     (printf "You can not go that way!\n")
                     (loop rid)))))
            
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

;;(startgame-new start)
;(startgame-maze start)