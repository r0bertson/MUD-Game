#lang racket

(include "assoc.rkt")
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
(define gatekey "")
;; END OF DEFINITIONS



; THIS FUNCTION WILL DEFINE THE START POINT
(define (startpoint)
  (let*((start_x (random X))
        (start_y (random Y)))
  (list start_x start_y)))

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


;will place one unit of type of key randomly on the maze
(define (random-key-location db types)
  (for ((i (length types)))
    (add-object db (list (random X) (random Y)) (car (ass-ref types i assq)))))





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


             

;;changed to receive a function as parameter to reuse to get keys
;;this function can both get the actions but also the words attached to it
;;depending on the function passed
(define (call-actions id tokens func)
  (let* ((record (ass-ref decisiontable 1 assv)) ;;get the references
         (keylist (get-keywords 1)) ;;get the keywords
         ;;description in the functions
         (index (index-of-largest-number (list-of-lengths keylist tokens)))) 
    (if index 
        (func (list-ref record index)) ;;return result if match, return false if dont
        #f)))


;;THIS FUNCTION WILL EVALUATE IF THE USER HAVE THE KEY NECESSARY TO OPEN THE GATE
(define (door-handle gatekey)
  (printf "You can see the exit gate, but it is locked. \n")
  (cond ((hash-has-key? inventorydb 'bag)
         (let* ((record (hash-ref inventorydb 'bag)) ;;get items list in bag
                (result (remove (lambda (x) (string-suffix-ci? gatekey x)) record)) ;;result = record - bag
                (item (lset-difference equal? record result))) ;; compare them
           (cond ((null? item) ;;if there is no difference, the key was removed, return true
               #t))))
        (else
         #f)))



;;START OF ALLOCATION OF ITENS AND ROOM NAMES
(random-allocator rooms room-type 100)       ;;allocate names to the rooms
(random-allocator objectdb objects 50)       ;;allocate items to the rooms
(random-key-location objectdb key_objects)   ;;allocate keys to the rooms
;;END OF ALLOCATION

;; ADVANCED COMMAND LINE PROCESSOR WITH MAZE
(define (startgame-maze)
  (let* ((gatekey (car (ass-ref key_objects (random(length key_objects)) assq)))
         (gate_x (random X))
         (gate_y (random Y))
         (start (startpoint)))
    (printf "~a \n" gate_x)
    (printf "~a \n" gate_y)
    (printf "~a \n" gatekey)
    (printf "~a \n " start)
    (let loop ((rid start))    
      (printf "You are in the ~a \n>" (hash-ref rooms rid))
      (let* ((input (read-line))
             (string-tokens (string-tokenize input))
             (tokens (map string->symbol string-tokens))
             (response (call-actions rid tokens cadr))) ;;get action

      
        (cond ((eq? response 'direction)
               (let* ((direction (call-actions rid tokens caar)) ;get direction typed
                      (newlocation (move-room rid direction)))  ;get future location after move
                 (cond((member direction (paths rid)) ;check if direction is in path
                       (cond ((equal? newlocation (list gate_x gate_y)) ;end of game condition
                              (cond ((not (door-handle gatekey))
                                     (printf "It seems that you don't have the key to open the gate. \n")
                                     (loop newlocation))
                                    (else
                                     (printf "You used the key to open the gate. You are free! \n")
                                     (exit))))
                         (else
                          (loop newlocation))));;not in the gate
   
                      (else ;;direction not in path
                       (printf "You can not go that way!\n")
                       (loop rid)))))
            
              ((eq? #f response)
               (format #t "huh? I didn't understand that!\n")
               (loop rid))
            
              ((eq? response 'look)
             ;(show-maze m rid)
               (display-objects objectdb rid)
               (loop rid))
              ((eq? response 'mazemap)
               (show-maze m rid)
             ;(display-objects objectdb rid)
               (loop rid))
            
              ((eq? response 'pick)
             ;remove item from room and put into inventory
               (handle-item 'room rid input)
               (loop rid))
            
              ((eq? response 'inventory)
               (display-inventory) ;;show inventorydb
               (loop rid))
            
              ((eq? response 'quit)
               (format #t "So Long, and Thanks for All the Fish...\n")
               (exit))
            
              ((eq? response 'drop)
               ;remove item from inventory and drop on the current room
               (handle-item 'bag rid input)
               (loop rid)))))))

;;(startgame-new start)
(startgame-maze)