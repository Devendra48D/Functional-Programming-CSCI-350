;;;Function reverse-general takes one parameter, a list L which can contain any number
;;;of elements and can also contain nested lists; returns a new list which is a reversed version of L.
;;;All the sub lists in L are reversed as well. 

;;;Logic: -returns empty list if L is empty
;;;       -if the first element in the list is a list then reverses it and
;;; appends the result to the end of the rest of the list
;;;       - if L is a simple list then appends the first element to the end of the rest of the list until
;;;L is reversed

(DEFINE (reverse-general L)
    (COND
        ((NULL? L) L) ;;empty list
        ((LIST? (CAR L)) (APPEND (reverse-general (CDR L)) (LIST (reverse-general (CAR L))))) ;;first element is a list
        ((LIST? L) (APPEND (reverse-general (CDR L)) (LIST (CAR L)))) ;; reverses a simple list
    )
)

;;;Function sum-up-numbers-simple takes one parameter, a list L which may contain
;;;numbers and non-numbers as elements; returns the sum of the numbers not in
;;;nested lists in L. If there are no such numbers, the result is 0.

(DEFINE (sum-up-numbers-simple L)
    (COND
        ((NULL? L) 0) ;;empty list
        ((LIST? (CAR L)) (+ 0 (sum-up-numbers-simple (CDR L)))) ;;first element is a list
        ((NUMBER? (CAR L)) (+ (CAR L) (sum-up-numbers-simple (CDR L)))) ;;first element is a number
    )
)
