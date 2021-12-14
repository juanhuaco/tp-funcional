

;(define (ambos pred?)
 ; (lambda (a b)
;    (and (pred? a) (pred? b))))

;============Ejercicio 1===============

(define (al-menos-uno pred)
  (lambda (a b)
    (not (and (not (pred a)) (not (pred b))))))

;============Ejercicio 2===============

;(define (ni-uno-ni-otro pred)
;  (lambda (a b)
;    (and (not (pred a)) (not (pred b)))))

;============Ejercicio 3===============

(define (ni-uno-ni-otro pred)
  (lambda (a b)
    (not ((al-menos-uno pred) a b))))

;============Ejercicio 4===============

(define (ambos pred)
  (lambda (a b)
    (and ((al-menos-uno pred) a a) ((al-menos-uno pred) b b))))