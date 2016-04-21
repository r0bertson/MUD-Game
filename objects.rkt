;;this file will deal with the objects manipulations


(define (add-object db id object)
  (if (hash-has-key? db id)
      (let ((record (hash-ref db id)))
        (hash-set! db id (cons object record)))
      (hash-set! db id (cons object empty))))

(define (add-objects db)
  (for-each
   (lambda (r)
     (add-object db (first r) (second r))) objects))


(define (display-objects db id)
  (cond ((hash-has-key? db id)
    (let* ((record (hash-ref db id))
           (output (string-join record " and ")))
      (cond ((equal? output "")
              (printf "You can see a single thing in this room. \n"))
            (else
       (if (eq? id 'bag)
                 (printf "You are carrying ~a. \n" output)
                 (printf "You can see ~a. \n" output))))))
        (else
         (if (eq? id 'bag)
             (printf "Your bag is empty!")
             (printf "The room is empty!")))))



;;ask if this is better this way or should i put inside the remove-object2 function
(define (evaluate a b id)
  (cond ((eq? a b)
       'bag)
        (else
         id)))

;;ask antonio if this function is better than the other
(define (remove-object db id from input)
  (let*((str (string-join (cdr (string-split input))))
        (newid (evaluate from 'bag id)))
    (when (hash-has-key? db newid)
      (let* ((record (hash-ref db newid))
                 (result (remove (lambda (x) (string-suffix-ci? str x)) record))
                 (item (lset-difference equal? record result)))
        (cond ((null? item)
               (printf "I don't see that item in the ~a! \n" from))
              (else
               (cond((eq? from 'room)
                     (printf "Added ~a to your bag.\n" (first item))
                     (add-object inventorydb 'bag (first item))
                     (hash-set! db id result))
                    (else
                     (printf "Removed ~a from your bag . \n" (first item))
                     (add-object objectdb id (first item))
                     (hash-set! db 'bag result)))))))))


;;should i remove these two functions??
(define (handle-item from id input)
  (if(eq? from 'bag)
    (remove-object inventorydb id 'bag input)
    (remove-object objectdb id 'room input)))




(define (display-inventory)
  (display-objects inventorydb 'bag))


;;END OF OBJECTS FUNCTIONS

