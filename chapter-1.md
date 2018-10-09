Exercise 1.11

```scheme
(define (f-iter a b c i n)
    (if (= i n)
        c
        (f-iter (+ a (* b 2) (* c 3)) a b (+ i 1) n)))

(define (f n) (f-iter 2 1 0 0 n))
```

Exercise 1.12

```scheme
(define (pascal-triangle row col) 
    (cond ((> col row) 0) 
          ((< col 0) 0) 
          ((= col 1) 1) 
          ((+ (pascal-triangle (- row 1) (- col 1)) 
              (pascal-triangle (- row 1) col)))))
```
Exercise 1.16

```scheme
 (define (fast-expt b n) 
   (define (iter a b n) 
     (cond ((= n 0) a) 
           ((even? n) (iter a (square b) (/ n 2))) 
           (else (iter (* a b) b (- n 1))))) 
   (iter 1 b n)) 
  
 (define (square x) (* x x)) 
```

Exercise 1.17

```scheme
 (define (double x) (+ x x)) 
 (define (halve x) (/ x 2)) 
  
 (define (* a b) 
   (cond ((= b 0) 0) 
         ((even? b) (double (* a (halve b)))) 
         (else (+ a (* a (- b 1)))))) 
```

