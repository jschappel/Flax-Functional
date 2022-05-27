## Desugarar In Depth
#### Define (function form)

Defines in the form `(define (x ...) ...)` are desugared into a lambda expression set to a definition. See Below.
```racket
(define (add x y) (+ x y))

; After desugaring becomes
(define add (lambda (x y) (+ x y)))
```


#### Cond
Cond expressions are broken down into nested if conditionals. If just a `else` case is provided then it just becomes an expression. If no case can be matched then `void` is returned.
```racket
; --- Case 1: ---
(cond
  ((eq? x 10) 10)
  ((eq? x 20) 20)
  (else 30))

; After desugaring becomes 
(if (eq? x 10)
    10
    (if (eq? x 20)
        20
        30))


; --- Case 2: ---
(cond (else 30))

; After desugaring becomes
30


; --- Case 3: ---
(cond
  ((eq? x 10) 10)
  ((eq? x 20) 20))

; After desugaring becomes
(if (eq? x 10)
    10
    (if (eq? x 20)
        20
        void))
```


#### List
List expressions are desugared into a nested core application expression where the inner most proc is `'emptylist` and the reset of the procs are `cons`. See Below,
```racket
(list 1 2 3)

; After desugaring becomes
(cons 1 (cons 2 (cons 3 emptylist)))
```


#### Vector
Vector expressions are desugared into a application expression where the proc is a lambda  expression and the argument is application expression that creates the vector using the proc `allocate array` and the arg is the size of the array. The body of the lambda expression is  a begin expression where the values of the array are set. See Below. 

```racket 
(vector 10 20 30)

; After desugaring becomes
((lambda ($V$) (begin
                 (array-set! $V$ 0 10)
                 (array-set! $V$ 1 20)
                 (array-set! $V$ 2 30)))
 ('allocate-array 3))
```


#### Let
Let expressions are desugared into an application expression where the operator is a lambda expression, whose body is the body of the let expression, and the operands are the values assigned to the let declarations. 

```racket 
(let ((x 10 )
      (y (- 20 30))
  (+ x y))

; After desugaring becomes
((lambda (x y) (+ x y))
 10
 (- 20 30))
```


#### Letrec
Letrec expressions are desugared into an application expression where the operator is a lambda expression whose params are the functions in the letrec and, whose body is a list of set expressions where each set expression sets the lambda param to the body of the letrec function. The params to the application expression are trash values and are reset inside the lambda. 


```racket 

(letrec ((define (is-even? n)
                 (or (zero? n)
                     (is-odd? (sub1 n))))
               (define (is-odd? n)
                 (and (not (zero? n))
                      (is-even? (sub1 n)))))
        (is-odd? 11))


; After desugaring becomes
((lambda (is-even? is-odd?)
       (begin
         (set! is-even? (lambda (n) (or (zero? n) (is-odd? (sub1 n)))))
         (set! is-odd? (lambda (n) (and (not (zero? n)) (is-even? (sub1 n)))))
         (is-odd? 11)))
     42
     42)
```