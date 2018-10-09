Exercise 2.1

```scheme
(define (make-rat n d)
        (let ((g ((if (< d 0) - +) (abs (gcd n d))))) (let ((n (/ n g)) (d (/ d g))) (cons n d)))
)


(define (gcd a b)
        (if (= b 0) a (gcd b (remainder a b))
))

```
