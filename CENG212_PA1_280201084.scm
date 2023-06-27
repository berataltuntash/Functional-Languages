(define lambda1 (lambda(x) ( * x x))) ; lambda 1
(define lambda2 (lambda() "I am a simple  function which only prints to screen :)")) ; lambda 2
(define pair19 (cons null lambda2))
(define pair14 (cons 1923 "Güliz"))
(define pair15 (cons "Eren" pair19))
(define pair16 (cons pair14 "Nuri"))
(define pair17 (cons pair16 pair15))
(define pair12 (cons "Hüseyin" "Samet"))
(define pair13 (cons "Serhat" pair17))
(define pair18 (cons "Büşra" pair13))
(define pair10 (cons "Adil" "Burak"))
(define list3 (list pair12 null "Dilek" "Altuğ"))
(define pair9 (cons "Ersin" 7 ))
(define list1 (list "Didem" lambda1 pair18))
(define pair8 (cons pair10 pair9))
(define pair11 (cons pair9 list3))
(define list2 (list "Cansu" "Ege" pair13 list1))
(define pair7 (cons pair8 "Didem"))
(define pair4 (cons pair11 list2))
(define pair6 (cons pair7 lambda1))
(define pair2 (cons 1992 pair4))
(define pair3 (cons 3 pair2))
(define pair1 (cons pair3 pair3))
(define pair5 (cons pair2 pair6))
(define headPair (cons pair1 pair5))

(define (traverser v1 v2 v3)
    (let ((checked '())) ;opening a list to keep track of visited variables
    
    (define (checker variable) 
        (cond ((member variable checked) (display "")) ; checking if v1 is on the list
            ((pair? variable) ; if v1 is a pair, firstly mark it as checked, using the our helper method twice, turn right, then to the left 
                (set! checked (cons variable checked))
                (checker(cdr variable))
                (checker(car variable))
            )

            ((null? variable) #f ; if v1 == null , return false
            ) 

            ((list? variable) ; if v1 is a list, firstly mark it as checked, then use our helper method to check each item in reverse order
                (set! checked (cons variable checked)) 
                (for-each checker (reverse variable))
            )

            ((or (number? variable) (string? variable)) ; if v1 is a string or number, check the predicate, if it is true, use v3 on v1. after that throw it to the checked list.
                (if (v2 variable) (v3 variable)
                    #f ;if predicate doesnt hold, return false
                ) 
                (set! checked (cons variable checked))
            )
            ((procedure? variable) ; if v1 is lambda, apply v3 to v1, if not, return false. Cant add lambda to chekced list
                 
                    (v3 variable)
                    (set! checked (cons variable checked)) 
            )

            (else #f) ; if none of our assumptions hold and something unusual happened, return false
        ) 
    ) 
    (checker v1)) ; invoke the checker method and give our item as an argument.
) 

(define (isPrime? n) 
    (cond ((<= n 1) #f)
        ((= n 2) #t)
        ((= (modulo n 2) 0) #f)
        (else (primeChecker n 3))
    )
) 

(define (primeChecker n divider)
    (cond ((>= (* divider divider) n) #t)
        ((= (modulo n divider) 0) #f)
        (else (primeChecker n (+ divider 2)))
    )
)

(display "*****************\n" )
(displayln " Output 1 \n")
(traverser headPair (lambda(x) (cond (( or (procedure? x) (number? x) (string? x)) #t)
          (else #f))) displayln)

(display "*****************\n" )
(displayln " Output 2 \n")
(traverser headPair (lambda(x) (cond ((and (number? x) (isPrime? x)) #t)
    (else #f))) 
    (lambda (x) (if (procedure? x) 
                (display "")
                (displayln x) 
                )
    )
)

(display "*****************\n" )
(displayln " Output 3 \n")
(traverser headPair (lambda(x) (cond ((and (string? x) (> (string-length x) 5)) #t)
    (else #f))) 
    (lambda (x) (if (procedure? x) 
                (display "") 
                (displayln x)
                )
    )
)

(display "*****************\n" )
(displayln " Output 4 \n")
(traverser headPair (lambda(x) (cond ((procedure? x) #t)
    (else #f))) 
    (lambda (x) (if(= (procedure-arity x) 1) 
                (displayln (x 17)) 
                (display "")
                )
    )
)

(display "****************\n" )
(displayln " Output 5 \n")
(traverser headPair (lambda(x) (cond ((procedure? x) #t)
    (else #f))) 
    (lambda (x) (if(= (procedure-arity x) 0) 
                (displayln (x))
                (display "")
                )
    )
)