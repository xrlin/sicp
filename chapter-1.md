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
