(define descriptions '((1 "You are in the lobby")
                       (2 "You are in the hallway")
                       (3 "You are in a swamp")))

(define look '(((directions) look) ((look) look) ((examine room) look)))

(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))

(define pick '(((get) pick ) ((pickup) pick) ((pick) pick)))

(define drop '(((put) drop) ((drop) drop) ((place) drop) ((remove) drop)))

(define inventory '(((inventory) inventory) ((bag) inventory)))

(define actions `(,@look ,@quit ,@pick ,@drop ,@inventory))

(define decisiontable `((1 ((north) 2) ((north west) 3) ,@actions)
                         (2 ((south) 1) ,@actions )
                         (3 ,@actions)))
