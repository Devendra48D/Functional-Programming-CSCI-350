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

;;; Function min-list takes one parameter L, a list of numbers and non-numbers without any nested lists
;;; and returns the smallest number in L. If no such number is present, returns #F.

;;; Logic: - Answer is #F if list is empty
;;;        - If the list has only one number, the nuumber is the answer
;;;        - If the list has only one non-number and no numbers; the answer is #F
;;;        - If the first element is not a number, the answer is minimum from the rest of the list
;;;        - If the first element is a number and the minimum for the rest of the list is not a number,
;;;          the answer is the first number
;;;        - If the first element is a number and the minimum for the rest is also a number,
;;;          the answer is minimum of these two values.

(DEFINE (min-list L)
    (COND
        ((NULL? L) #F) ;; empty list
        ((AND (NUMBER? (CAR L)) (NULL? (CDR L))) (CAR L)) ;; list with only one number
        ((AND (NOT (NUMBER? (CAR L))) (NULL? (CDR L))) #F) ;; list with only one non-number
        ((NOT (NUMBER? (CAR L))) (min-list (CDR L))) ;; first element is not a number
        ((AND (NUMBER? (CAR L)) (NOT (NUMBER? (min-list (CDR L))))) (CAR L)) ;; first element is a number
        ;; and min of rest of the list is not a number
        ((AND (NUMBER? (CAR L)) (NUMBER? (min-list (CDR L)))) (MIN (CAR L) (min-list (CDR L))))
        ;; first element is a number and min of rest of the list also a number
    )
)

;;; Function next-big takes two parameters, L a list containing numbers and non-numbers but no nested lists
;;; and num a number. next-big returns the smallest of the numbers that are larger than num in L.
;;; If there are no such numbers in L, the result is #F.

;;; Logic: - If num is not a number, the result is #F.
;;;        - If L is empty, the result i #F.
;;;        - If there is only one element in L that is smaller than or equal to num, the result is #F
;;;        - If there is only one element in L that is larger than num, the result is the only number
;;;        - If there is only one element in L and it is a non number, the result is #F.
;;;        - If the first element is not a number, the rest of the list is used to find the result
;;;        - If the first number is larger than num and there is a result from the rest of the list,
;;;          the minimum of these two is the answer.
;;;        - If the first number is larger than num and there is no result from the rest of the list,
;;;          the result is the first number
;;;        - If the first number is less than or equal to num or the first element is not a number,
;;;        - the rest of the list is used to find the result

(DEFINE (next-big L num)
    (COND
         ((NOT (NUMBER? num)) #F) ;; num is not a number
         ((NULL? L) #F) ;; L is empty
         ((AND (NUMBER? (CAR L)) (AND (NULL? (CDR L)) (<= (CAR L) num))) #F) ;; only one element in L
         ;; the element is a number that is smaller than or equal to num
         ((AND (NUMBER? (CAR L)) (AND (NULL? (CDR L)) (> (CAR L) num))) (CAR L)) ;; only one element in L
         ;; the element is a number that is larger than num
         ((AND (NOT (NUMBER? (CAR L))) (NULL? (CDR L))) #F) ;; only element in L and it is not a number
         ((AND (NOT (NUMBER? (CAR L))) (NOT (NULL? (CDR L)))) (next-big (CDR L) num)) ;; first element is not a
         ;; number and there are more elements in the list
         ((AND (> (CAR L) num) (NUMBER? (next-big (CDR L) num))) (MIN (CAR L) (next-big (CDR L) num)))
         ;; first number is larger than num and there is a result from the rest of the list
         ((AND (> (CAR L) num) (NOT (NUMBER? (next-big (CDR L) num)))) (CAR L))
         ;; first number is larger than num and there is no result from the rest of the list
         ((OR (<= (CAR L) num) (NOT (NUMBER? (CAR L)))) (next-big (CDR L) num))
         ;; first number is less than or equal to num or first element is not a number
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
