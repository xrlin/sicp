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
              (else (list (deep-reverse (cadr tree)) (deep-reverse (car tree))))
        )
)

(define (cadr x)
  (if (null? (cdr x)) nil
    (car (cdr x)))
  )

```

Exercise 2.28

```scheme
(define (fringe tree)
        (cond ((null? tree) nil)
                ((pair? tree) (append (fringe (car tree)) (fringe (cdr tree))))
                (else (list tree)))
)
```

Exercise 2.31

```schem
(define (tree-map fn tree)
        (map (lambda (sub-tree)
                (if (pair? sub-tree)
                        (tree-map fn sub-tree)
                (fn sub-tree))) tree)
)

(define square (lambda (x) (* x x)))
(define (square-tree tree) (tree-map square tree))
```

Exercise 2.32

```scheme
(define (subsets s)
        (if (null? s)
                (list nil)
                (let ((rest (subsets (cdr s))))
                        (append rest (map (lambda (l) (append (list (car s)) l)) rest))))
)
```

Exercise 2.33

```scheme
(define (accumulate p initial sequence)
        (if (null? sequence)
                initial
                (p (car sequence) (accumulate p initial (cdr sequence))))
)

(define (map p sequence)
        (accumulate (lambda (x y) (cons (p x) y)) nil sequence)
)

(define (append seq1 seq2)
        (accumulate cons seq2 seq1)
)

(define (length sequence)
        (accumulate (lambda (a b) (+ b 1)) 0 sequence)
)
```

Exercise 2.34

```scheme
(define (horner-eval x coefficient-sequence)
        (accumulate (lambda (this-coeff higher-terms) (+ (* x higher-terms) this-coeff))
         0 coefficient-sequence))
```

Exercise 2.36

```scheme
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map (lambda (lst) (car lst)) seqs))
            (accumulate-n op init (map (lambda (lst) (cdr lst)) seqs)))))
```




