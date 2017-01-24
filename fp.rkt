;;; Function reverse-general takes one parameter, a list L which can contain any number
;;; of elements and can also contain nested lists; returns a new list which is a reversed version of L.
;;; All the sub lists in L are reversed as well. 

;;; Logic: -returns empty list if L is empty
;;;       - If the first element in the list is a list then reverses it and
;;; appends the result to the end of the rest of the list
;;;       - If L is a simple list then appends the first element to the end of the rest of the list until
;;; L is reversed

(DEFINE (reverse-general L)
    (COND
        ((NULL? L) L) ;; empty list
        ((LIST? (CAR L)) (APPEND (reverse-general (CDR L)) (LIST (reverse-general (CAR L))))) ;; first element is a list
        ((LIST? L) (APPEND (reverse-general (CDR L)) (LIST (CAR L)))) ;; reverses a simple list
    )
)

;;; Function sum-up-numbers-simple takes one parameter, a list L which may contain
;;; numbers and non-numbers as elements; returns the sum of the numbers not in
;;; nested lists in L. If there are no such numbers, the result is 0.

;;; Logic: - If the list is empty answer is 0
;;;        - If the first element is a list, answer is (0 + the sum of rest of the list)
;;;        - If the first element is not a number, answer is (0 + the sum of rest of the list)
;;;        - If the first element is a number, answer is (first element + the sum of the rest of the list)

(DEFINE (sum-up-numbers-simple L)
    (COND
        ((NULL? L) 0) ;; empty list
        ((LIST? (CAR L)) (+ 0 (sum-up-numbers-simple (CDR L)))) ;;first element is a list
        ((NOT (NUMBER? (CAR L))) (+ 0 (sum-up-numbers-simple (CDR L)))) ;; first element is not a number
        ((NUMBER? (CAR L)) (+ (CAR L) (sum-up-numbers-simple (CDR L)))) ;; first element is a number
    )
)


;;; Function sum-up-numbers-general takes one parameterl L, a list which may contain
;;; numbers and non-numbers as elements; returns the sum of all the numbers
;;; (including those in nested lists) in L. If there are no such numbers, the result is 0.

;;; Logic: - If the list is empty, result is 0
;;;        - If the first element is a list, answer is (sum for current list + the sum of rest of the list)
;;;        - If the first element is not a number, answer is (0 + the sum of rest of the list)
;;;        - If the first element is a number, answer is (first element + the sum of the rest of the list)

(DEFINE (sum-up-numbers-general L)
    (COND
        ((NULL? L) 0);; empty list
        ((LIST? (CAR L)) (+ (sum-up-numbers-general (CAR L)) (sum-up-numbers-general (CDR L)))) ;; first element is a list
        ((NOT (NUMBER? (CAR L))) (+ 0 (sum-up-numbers-general (CDR L)))) ;; first element is not a number
        ((NUMBER? (CAR L)) (+ (CAR L) (sum-up-numbers-general (CDR L)))) ;; first element is a number
    )
)


;;; Function min-above-min takes two parameters, L1 and L2 that are both lists which do not contain nested lists
;;; Both L1 and L2 may have non-numeric elements; returns the minimum of the numbers in L1 that are larger than
;;; the smallest number in L2.
;;; If there is no number in L2, the function returns the minimum of numbers in L1.
;;; If there is no number in L1 that is larger than the minimum of L1, the result is #F.

(DEFINE (min-above-min L1 L2)
    (COND
        ((NULL? L1) #F) ;; L1 is empty
        ((NOT (NUMBER? (min-list L1))) #F) ;; L1 does not have a number
    )
)
