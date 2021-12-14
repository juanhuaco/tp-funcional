(#%require (only racket/base random))

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


















