


(define room-type '((0 "Entrance")
                    (1 "hall")
                    (2 "hallway")
                    (3 "corridor")
                    (4 "lobby" )
                    (5 "hallway")
                    (6 "court" )
                    (7 "pass" )))


(define objects '((0 "a silver dagger")
                  (1 "a gold coin")
                  (2 "a long sword")
                  (3 "a rope")
                  (4 "a key")
                  (5 "a key")
                  (6 "a key")))

(define key_objects '((0 "a red key")
                      (1 "a blue key")
                      (2 "a white key")
                      (3 "a black key")))

(define look '(((directions) look) ((look) look) ((examine room) look)))

(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))

(define pick '(((get) pick ) ((pickup) pick) ((pick) pick)))

(define directions '(((south) direction) ((north) direction) ((west) direction) ((east) direction)))

;;the put name in here is just the name of the list
;;the name of the action is drop, so we can change the list name to drop
;;to avoid confusion. i'll leave it this way to remember in the future
(define put '(((put) drop) ((drop) drop) ((place) drop) ((remove) drop)))

(define inventory '(((inventory) inventory) ((bag) inventory)))

(define mazemap '(((map) mazemap) ((show map) mazemap)((see map) mazemap) ((kook map) mazemap)))

(define actions `(,@look ,@quit ,@pick ,@put ,@inventory,@directions,@mazemap))

(define decisiontable `((1 ,@actions)
                        (2 ((south) 1) ,@actions )
                        (3 ,@actions)))



;; new functions to the maze

