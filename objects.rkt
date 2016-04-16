;;this file will deal with the objects manipulations


(define (add-object db id object)
  (if (hash-has-key? db id)
      (let ((record (hash-ref db id)))
        (hash-set! db id (cons object record)))
      (hash-set! db id (cons object empty ))))

(define (add-objects db)
  (for-each
   (lambda (r)
     (add-object db (first r) (second r))) objects))



(define (display-objects db id)
  (when (hash-has-key? db id)
    (let* ((record (hash-ref db id))
           (output (string-join record " and ")))
      (when (not (equal? output ""))
        (if (eq? id 'bag)
            (printf "You are carrying ~a. \n" output)
            (printf "You can see ~a. \n" output))))))

;;function that removes an object from both inventory and objectdb

(define (remove-object db id from str)
  (cond((eq? from 'bag) ;;verifies from where we should remove
        (when (hash-has-key? db 'bag)
          (let* ((record (hash-ref db 'bag))
                 (result (remove (lambda (x) (string-suffix-ci? str x)) record))
                 (item (lset-difference equal? record result)))
            (cond ((null? item)
                   (printf "You are not carrying that item ! \n"))
                  (else
                    (printf "Removed ~a from your bag . \n" (first item))
                    (add-object objectdb id (first item))
                    (hash-set! db 'bag result))))))
       (else
        (when (hash-has-key? db id)
          (let* ((record (hash-ref db id))
                 (result (remove (lambda (x) (string-suffix-ci? str x)) record ))
                 (item (lset-difference equal? record result)))
            (cond ((null? item)
                   (printf "I don't see that item in the room! \n"))
                  (else
                   (printf "Added ~a to your bag.\n" (first item))
                   (add-object inventorydb 'bag (first item))
                   (hash-set! db id result))))))))


(define (pick-item id input)
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object objectdb id 'room item)))
;;(remove-object-from-room objectdb id item )))

(define (put-item id input)
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object inventorydb id 'bag item)))
;;(remove-object-from-inventory inventorydb id item)))

(define (display-inventory)
  (display-objects inventorydb 'bag))


;;END OF OBJECTS FUNCTIONS


(define objects '((1 "a silver dagger")
                  (1 "a gold coin")))
