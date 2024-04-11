; Othón Berlanga Calderón - A01660663
; Wilfrido Tovar Andrade - A01664769
;Problemario: programación funcional parte 1

#lang racket


; Ejercicio 1
(displayln "EJERCICIO 1")
(define (fahrenheit-to-celsius f)
  (displayln (/ (*(- f 32) 5) 9)))

(fahrenheit-to-celsius 212.0)
(fahrenheit-to-celsius 32.0)
(fahrenheit-to-celsius -40)


;Ejercicio 2
(displayln "EJERCICIO 2")
(define (sign n)
  (cond [(> n 0)
            (displayln 1)]
        [(= n 0)
            (displayln 0)]
        [(< n 0)
            (displayln -1)]
        ))

(sign -5)
(sign 10)
(sign 0)

; Ejercicio 3
(displayln "EJERCICIO 3")
(define (roots a b c)
  (displayln (/ (+ (- b) (sqrt (- (expt b 2) (*(* a c) 4)))) (* 2 a))))

(roots 2 4 2)
(roots 1 0 0)
(roots 4 5 1)

; Ejercicio 4
(displayln "EJERCICIO 4")
(define (calc_bmi weight height)
  (/ weight (expt height 2)))

(define (description calc_bmi)
    (cond [( < calc_bmi 20)
           (displayln "underweight")]
          [(and (<= calc_bmi 20) (< calc_bmi 25))
           (displayln "normal")]
          [(and (<= calc_bmi 25) (< calc_bmi 30))
           (displayln "obese1")]
          [(and (<= calc_bmi 30) (< calc_bmi 40))
           (displayln "obese2")]
          [(<= calc_bmi 40)
           (displayln "obese3")]))

(define (bmi weight height)
  (let ((bmi-value (calc_bmi weight height)))
    (description bmi-value)))

(bmi 45 1.7)
(bmi 55 1.5)
(bmi 76 1.7)
(bmi 81 1.6)
(bmi 120 1.6)

; Ejercicio 5
(displayln "EJERCICIO 5")
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(factorial 0)
(factorial 5)
(factorial 40)

; Ejercicio 6
(displayln "EJERCICIO 6")
(define (duplicate lst)
  (cond [(null? lst) '()]
        [else (cons (car lst) (cons (car lst) (duplicate (cdr lst))))]))
(duplicate '())
(duplicate '(1 2 3 4 5))
(duplicate '(a b c d e f g h))

; Ejercicio 7
(displayln "EJERCICIO 7")
(define (pow a b)
  (if (= b 0)
      1
      (* a (pow a (- b 1)))))

(pow 5 0)
(pow -5 3)
(pow 15 12)

; Ejercicio 8
(displayln "EJERCICIO 8")
(define (fib n)
  (cond ((<= n 1) n)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(fib 6)
(map fib  (range 10))
(fib 42)

; Ejercicio 9
(displayln "EJERCICIO 9")
(define (enlist lst)
  (if (null? lst)
  '()
  (cons (list (car lst)) (enlist (cdr lst))))
)

(enlist '())
(enlist '(a b c))
(enlist '((1 2 3) 4 (5) 7 8))

; Ejercicio 10
(displayln "EJERCICIO 10")
(define (positives lst)
  (let ((result
         (cond [(null? lst) '()]
               [(positive? (car lst))
                (cons (car lst) (positives (cdr lst)))]
               [else (positives (cdr lst))])))  
    result))

(positives '())
(positives '(12 -4 3 -1 -10 -13 6 -5))
(positives '(-4 -1 -10 -13 -5))
  

; Ejercicio 11
(displayln "EJERCICIO 11")
(define (add-list lst)
  (let ((result
         (cond [(null? lst) 0]
               [else (+ (car lst)
                        (add-list (cdr lst)))])))
    result))


(add-list '())
(add-list '(2 4 1 3))
(add-list '(1 2 3 4 5 6 7 8 9 10))

 ; Ejercicio 12
 (displayln "EJERCICIO 12")
(define (invert-pairs lst)
  (if (null? lst)
  '()
  (cons (list (car (cdr (car lst))) (car (car lst))) (invert-pairs (cdr lst)))))

(invert-pairs '())
(invert-pairs '((a 1)(a 2)(b 1)(b 2)))
(invert-pairs '((January 1)(February 2)(March 3)))

; Ejercicio 13
(displayln "EJERCICIO 13")
(define (list-of-symbols? lst)
  (cond ((null? lst) #t)                       
        ((symbol? (car lst))                    
         (list-of-symbols? (cdr lst)))         
        (else #f))) 

(list-of-symbols? '())
(list-of-symbols? '(a b c d e))
(list-of-symbols? '(a b c d 42 e))

; Ejercicio 14
(displayln "EJERCICIO 14")
(define (swapper a b lst)
  (define (swap-helper x)
    (if (null? x)
        '()
        (let ((head (car x))
              (tail (cdr x)))
          (cons (if (eq? head a) b (if (eq? head b) a head))
                (swap-helper tail)))))
  (swap-helper lst))

(swapper 1 2 '())
(swapper 1 2 '(4 4 5 2 4 8 2 5 6 4 5 1 9 5 9 9 1 2 2 4))
(swapper 1 2 '(4 3 4 9 9 3 3 3 9 9 7 9 3 7 8 7 8 4 5 6))
(swapper 'purr 'kitty '(soft kitty warm kitty little ball of fur happy kitty sleepy kitty purr purr purr))

; Ejercicio 15
(displayln "EJERCICIO 15")
(define (dot-product a b)
  (define (dot-product-helper a b result)
    (if (or (null? a) (null? b))  ; Si alguna de las listas es vacía, terminamos
        result
        (dot-product-helper (cdr a) (cdr b) (+ (* (car a) (car b)) result))))

  (dot-product-helper a b 0))
(dot-product '() '())
(dot-product '(1 2 3) '(4 5 6))
(dot-product '(1.3 3.4 5.7 9.5 10.4) '(-4.5 3.0 1.5 0.9 0.0))

; Ejercicio 16 
(displayln "EJERCICIO 16")
(define (sum lst)
  (if (null? lst)
      0
      (+ (car lst) (sum (cdr lst)))))

(define (average lst)
  (if (null? lst)
      0
      (/ (sum lst) (length lst))))

(average '())
(average '(4))
(average '(5 6 1 6 0 1 2))
(average '(1.7 4.5 0 2.0 3.4 5 2.5 2.2 1.2))

; Ejercicio 17 
(displayln "EJERCICIO 17")
(define (summation lst avg)
  (if (null? lst)
    0
    (+  
       (pow (- (car lst) avg) 2)
    (summation (cdr lst) avg))))

(define (standard-deviation lst)
(if (null? lst)
  0
    (sqrt
        (/  
          (summation lst (average lst))
          (length lst)))))

(standard-deviation '())
(standard-deviation '(4 8 15 16 23 42))
(standard-deviation '(110 105 90 100 95))
(standard-deviation '(9 2 5 4 12 7 8 11 9 3 7 4 12 5 4 10 9 6 9 4))

; Ejercicio 18
(displayln "EJERCICIO 18")
(define (replic n lst)
  (if (null? lst)
      '()
      (append (make-list n (car lst)) (replic n (cdr lst)))
      )
  )

(replic 7 '())
(replic 0 '(a b c))
(replic 3 '(a))
(replic 4 '(1 2 3 4))

; Ejercicio 19
(displayln "EJERCICIO 19")
(define (expand-helper lst n)
  (if(null? lst)
  '()
  (append (make-list n (car lst))
  (expand-helper (cdr lst) (+ n 1)))
  )
)
(define (expand lst)
(expand-helper lst 1))

(expand '(a b c d e))

; Ejercicio 20
(displayln "EJERCICIO 20")
(define (binary n)
  (if (< n 1)
      `()
      (append (binary (floor (/ n 2))) (list(remainder n 2)))
      )
  )

(binary 0)
(binary 30)
(binary 45123)
