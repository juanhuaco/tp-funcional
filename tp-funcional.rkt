(#%require (only racket/base random))


;=============PARTE UNO==============


;============funcion ambas===========
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


;=============PARTE DOS==============

;=============FUNCIONES DADAS==============
(define conj-etiqueta "conjunto")

(define conj-vacio (cons conj-etiqueta '()))

(define (conj-vacio? c)
  (eq? c conj-vacio))

(define (conj? arg)
  (and
   (pair? arg)
   (eq? (car arg) conj-etiqueta)))

(define (elige c)
  (let ((ls (cdr c)))
    (if (null? ls)
        (/ 1 0) ; en el conjunto vacio no se elige elemento: ERROR
        (list-ref ls (random (length ls))))))

(define (conj-ampliado elem c)
  (cons conj-etiqueta
        (cons elem (cdr c))))

(define (residuo elem)
  (lambda (c)
    (let ((ls (borra-elem-de-lista elem (cdr c))))
      (if (null? ls)
          conj-vacio
          (cons conj-etiqueta ls)))))
(define (borra-elem-de-lista elem lst)
  (if (null? lst) '()
      (if (equal? (car lst) elem)
          (borra-elem-de-lista elem (cdr lst))
          (cons (car lst) (borra-elem-de-lista elem (cdr lst))))))


;============Ejercicio 5===========

(define (list->conj lista)
  (if (null? lista)
      conj-vacio
      (if (member (car lista) (list->conj (cdr lista)))
          (list->conj (cdr lista))
          (conj-ampliado (car lista) (list->conj (cdr lista))))))

;============Ejercicio 6===========


(define (para-ninguno pred)
  (lambda (conj)
      (if (conj-vacio? conj)
          #t
          (let ((elem (elige conj)))
          (if (pred elem)
              #f
              ((para-ninguno pred) ((residuo elem) conj)))))))
    
;============Ejercicio 7===========

(define (existe-al-menos-uno pred)
  (lambda (conj)
      (if (conj-vacio? conj)
          #f
          (let ((elem (elige conj)))
          (if (pred elem)
              #t
              ((existe-al-menos-uno pred) ((residuo elem) conj)))))))

;============Ejercicio 8===========


(define (para-todos pred)
  (lambda (conj)
      (if (conj-vacio? conj)
          #t
          (let ((elem (elige conj)))
          (if (pred elem)
              ((para-todos pred) ((residuo elem) conj))
              #f)))))


;============PARTE 3===========


;============Ejercicio 9===========

(define (pertenece obj)
  (lambda (conj)
    (if (member obj (cdr conj))
        #t
        #f)))

;============Ejercicio 10===========

 (define (contiene conj)
   (lambda (obj)
     ((pertenece obj) conj)))

;============Ejercicio 11===========

(define (subconj c1)
  (lambda (c2)
    ((para-todos (contiene c2)) c1)))

;============Ejercicio 12===========

(define (cardinalidad conj)
  (if (conj-vacio? conj)
      0
      (+ 1 (cardinalidad ((residuo (elige conj)) conj)))))
 
;============Ejercicio 13===========

 (define (interseccion c1 c2)
   (if (or (conj-vacio? c1) (conj-vacio? c2))
       conj-vacio
       (let ((elem (elige c1))) 
         (if ((contiene c2) elem)
            (conj-ampliado elem (interseccion ((residuo elem) c1) c2))
            (interseccion ((residuo elem) c1) c2)))))

;============Ejercicio 14===========
(define (map-conj proc conj)
  (if (conj-vacio? conj)
      conj-vacio
      (let ((elem (elige conj)))
        (conj-ampliado (proc elem) (map-conj proc ((residuo elem) conj))))))








