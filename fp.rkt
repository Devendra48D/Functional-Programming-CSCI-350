;;;Function reverse-general takes one parameter, a list L which can contain any number
;;;of elements and can also contain nested lists; returns a new list which is a reversed version of L.
;;;All the sub lists in L are reversed as well. 

(DEFINE (reverse-general L)
    (COND
        ((NULL? L) L) ;;empty list
        ((LIST? (CAR L)) (APPEND (reverse-general (CDR L)) (LIST (reverse-general (CAR L))))) ;;first element is a list
        ((LIST? L) (APPEND (reverse-general (CDR L)) (LIST (CAR L)))) ;; reverses a simple list
    )
)
