
#| Rose Reiner CS230 Scheme Assignment |#

;;; Problem 1: absolute value ;;;
(define (absall1 lis)
  (cond
    ((null? lis) '())
    ((>= (car lis) 0) (cons (car lis) (absall1(cdr lis))))
    (else (cons (abs (car lis)) (absall1(cdr lis))))
    )
  )

(define (absall2 lis)
    (map abs lis)  
  )

;;; Problem 2: multiinsertL ;;;
(define (multiinsertL atm1 atm2 lis)
  (cond
    ((null? lis) '())
    ((eq? (car lis) atm2) (cons atm1 (cons atm2 (multiinsertL atm1 atm2 (cdr lis)))))
    (else (cons (car lis) (multiinsertL atm1 atm2 (cdr lis))))
     )
  )


;;; Problem 3: removeAllDeep ;;;
(define (removeAllDeep atm lis)
  (cond
    ((null? lis) '())
    ((list? (car lis)) (cons (removeAllDeep atm (car lis)) (removeAllDeep atm (cdr lis))))
    ((eq? (car lis) atm) (removeAllDeep atm (cdr lis)))
    (else (cons (car lis) (removeAllDeep atm (cdr lis))))
    )
  )

;;;Problem 4: join2Lists ;;;
(define (join2Lists lis1 lis2)
  (cond
    ((null? lis1) lis2)
    ((null? lis2) lis1)
    ((cons (car lis1) (join2Lists (cdr lis1) lis2)))
    )
  )

;;; Problem 5: enterAndSearch is the function to start ;;;
(define (enterAndSearch)
  (findkey(dottedPairs))
  
  )

(define (dottedPairs)
 
    (display "The program will ask if you want to exit after you enter two values. ")
    (display "Enter a value: ")
    (define firstvalue (read))
    (display "Enter a second value: ")
    (define secondvalue (read))
    (display "Do you want to exit the program, yes or no: ")
    (define userexit (read))
    (if (equal? userexit 'yes) (cons (cons firstvalue secondvalue) '()) (cons (cons firstvalue secondvalue) (dottedPairs)))
  )

(define listtosearch '((one . 1) (two . 2) (three . 3) (four . 4) (five . 5))) 


(define (findkey lis)
 
    (display "enter a key to search for: ")
    (define keyvalue (read))
    (cdr (assoc keyvalue lis))    
   )


;;; Problem 6: last ;;;
(define (last lis . num)
  (cond
    ((null? lis) '())
    ((null? num) (getlastatm lis))
    (else (getlastatm2 lis num))
    )
  )

(define (getlastatm lis)
  (cond
    ((null? lis) '())
    ((= 1 (length lis)) lis)
    (else (getlastatm (cdr lis)))))
   
 (define (getlastatm2 lis num)
   (cond
     ((null? lis) '())
     ((= 0 (car num)) '())
     ((= (car num) (length lis)) lis)
     (else (getlastatm2 (cdr lis) num))
     )
   )


;;; Problem 7: addingLists ;;;
(define (addingLists lis1 lis2)
  (cond
    ((null? lis1) lis2)
    ((null? lis2) lis1)
    (else (cons (+ (car lis1) (car lis2)) (addingLists (cdr lis1) (cdr lis2))))
   )
  )

;;; Problem 8 ;;;
#|
a. Another name for the function could be intergerDivision
b. If the first number is greater than the second, it adds one and
recurses with the new parameters of the first number subracted from the second and the second number
|#


;;; Problem 9 ;;;
#|
  a. cdr, cdr, cdr, car, cdr, car, car, car
  b. (car(cdr(car(cdr(car(cdr(cdr(cdr lis))))))))
|#

(define (gettingI lis)
  (car(cdr(car(cdr(car(cdr(cdr(cdr lis))))))))
  
  )


;;; Problem 10: odd-list ;;;
(define (odd-list atm)
  (reverse (oddlistHelper atm))
 
  )

(define (oddlistHelper atm)
  (cond
    ((< atm 2) '())
    ((odd? (truncate atm)) (cons (- atm 2) (oddlistHelper (- atm 2))))
    (else (cons (- atm 1) (oddlistHelper (- atm 1))))   
 )
  )


;;; Problem 11, 12, and 13: Blackjack ;;;

#|
 For Problem 11 start with the function blackjackHand
 For Problem 12 and 13 start with bjRead
|#

(define blackjackCards '((Ace . 1) (two . 2) (three . 3) (four . 4) (five . 5) (six . 6) (seven . 7) (eight . 8) (nine . 9) (ten . 10)
                                      (jack . 10) (queen . 10) (king . 10))) 
  

(define (bjRead)
  (Display "Enter a blackjack hand: ")
  (define firstCard (read))
  (define secondCard (read))
  (define hand (cons firstCard (cons secondCard '())))
  (addingCards hand)

)

(define (addingCards hand)
   (display (blackjackHand hand))
   (newline)
   (display hand)
   (newline)
   (suggestion hand)
   (newline)
  
  (cond   
    ((> (blackjackHand hand) 21) (display "Over 21!"))
    ((= (blackjackHand hand) 21) (display "Blackjack!"))
    (else (enterCards hand))
    )
  )

(define (enterCards hand)
   (Display "Enter a card or stay to quit:")
   (define userinput (read))

   (cond
     ((eq? userinput 'stay) (Display "Try again"))
     (else (addingCards (cons userinput hand)))
     )
  )

(define (blackjackHand hand . total)
  (cond
    ((null? total) (blackjackHand hand 0))
    ((null? hand) (car total))
    ((equal? (car (assoc (car hand) blackjackCards)) 'ace) (bjAce hand (car total)))
    (else (blackjackHand (cdr hand) (+ (car total) (cdr (assoc (car hand) blackjackCards)))))
    )
  )

(define (bjAce hand total)
  (cond
    ((> (blackjackHand (cdr hand) total) 10) (blackjackHand (cdr hand) (+ 1 total)))
    (else (blackjackHand (cdr hand) (+ 11 total)))
    
    )
  )

(define (suggestion hand)
  (cond
    ((>= (blackjackHand hand) 16) (display "It is suggested that you STAY"))
    (else (display "It is suggested that you HIT"))
    )
  )


  
     
     
  












                                                 

                                                 


  

  

  
