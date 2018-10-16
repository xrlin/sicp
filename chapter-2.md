Exercise 2.1

```scheme
(define (make-rat n d)
        (let ((g ((if (< d 0) - +) (abs (gcd n d))))) (let ((n (/ n g)) (d (/ d g))) (cons n d)))
)


(define (gcd a b)
        (if (= b 0) a (gcd b (remainder a b))
))

```

Exercise 2.5

```scheme
(define (exp base power)                                                                     
        (define (iter count result)                                                          
                (if (= count 0) result                                                       
                        (iter (- count 1) (* result base))                                   
                )                                                                            
        )                                                                                    
        (iter power 1)                                                                       
)                                                                                            
                                                                                             
(define (count-0-remainder num divider)                                                      
        (define (iter count)                                                                 
                (if (= (remainder num (exp divider count)) 0) (iter (+ count 1))             
                        (- count 1)                                                          
                )                                                                            
        )                                                                                    
        (iter 1)                                                                             
)                                                                                            
                                                                                             
                                                                                             
(define (my-cons a b)                                                                        
        (* (exp 2 a) (exp 3 b))                                                              
)                                                                                            
                                                                                             
(define (my-car num)                                                                         
        (count-0-remainder num 2)                                                            
)                                                                                            
                                                                                             
(define (my-cdr num)                                                                         
        (count-0-remainder num 3)                                                            
)                                                                                            
                                                                                             
```

Exercise 2.23

```scheme
(define (for-each fn items)                                                      
        (define (iter sub)                                                       
                (if (not (null? sub))                                            
                        (begin (fn (car sub)) (iter (cdr sub))))                 
        )                                                                        
        (iter items)                                                             
)                                                                                
```

Exercise 2.27

```scheme
(define (deep-reverse tree)
        (cond ((null? tree) nil)
              ((not (pair? tree)) tree)
              (else (list (deep-reverse (car (cdr tree))) (deep-reverse (car tree))))
        )
)

(define (cadr x)
  (if (null? (cdr x)) nil
    (car (cdr x)))
  )

```
