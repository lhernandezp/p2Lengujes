#lang plai
;;Data type FWAEL
(define-type FWAEL 
  [num (n number?)]; num-n
  [booleano (b boolean?)]
  [id (v symbol?)]
  ;[with (w listof Bind?) (body FWAEL?)]
  [binop (f symbol?)(l FWAEL?)(r FWAEL?)]
  [lempty]
  [lcons (e FWAEL?)(c FWAEL?)]; lcons-e -> cabeza, lcons-c -> cola
           (id exp))]
      [(list? exp)
       (cond
         [(symbol=? (car exp) 'cons) 
          (lcons (parser (cadr exp)) (parser (caddr exp)))]
         [(symbol=? (car exp) 'car)
          (lcar (parser (cadr exp) ))]
              (symbol=? '* (car exp))
              (symbol=? '/ (car exp)))
          
          (binop (car exp)(parser(cadr exp))(parser(caddr exp)))])])))
      [lempty () (lista-vacia)]
;;Function auxiliary for in the interp
(define func
  (lambda (s l r)
