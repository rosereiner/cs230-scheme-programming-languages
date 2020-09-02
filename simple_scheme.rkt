(define (square x)
  (* x x)
  )

(define (hypotenuse s1 s2)
  (sqrt (+ (square s1) (square s2)))
  )

(define (factorial n)
  (if (= 0 n)
      1
      (* n (factorial (- n 1)))
   )
  )

(define (fibo n)
  (if (= n 0)
      0
      (if (= n 1)
          1
          (+ (fibo (- n 1)) (fibo (- n 2)))
          )
      )
  )

(define (fibo2 n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (else (+ (fibo2 (- n 1)) (fibo2 (- n 2))))
      )
  )


(member1 atm lis)
  (cond
    ((null? lis) #f)
    ((eq? atm (car lis)) #t)
    (else (member1 atm (cdr lis)))
    )
  
  )

(define (memberdeep atm lis)
  (cond
    ((null? lis) #f)
    ((list? (car lis))
     (if (memberdeep atm (car lis))
         #t
         (memberdeep atm (cdr lis))))
    ((eq? atm (car lis)) #t)
    (else (memberdeep atm (cdr lis)))
    )
  
  )


(define (atomsonly? lis)
  (cond
    ((null? lis) #t)
    ((list? (car lis)) #f)
    (else (atomsonly? (cdr lis)))
    )
)

(define (power base exp)
  (cond
    ((= exp 0) 1)
    (else (* base (power base (- exp 1))))
    )
  )