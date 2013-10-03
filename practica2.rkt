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
  [lcar (l FWAEL?)])

;;Data type of listas
(define-type Listas
  [lista-vacia]
  [lista (e FWAEL?) (l Listas?)];lista-e
  )
  
 ;;Parser
(define parser
  (lambda (exp)
    (cond
      [(number? exp)(num exp)]
      [(boolean? exp)(booleano exp)]
      [(symbol? exp)
       (if (symbol=? 'empty exp)
           (lempty)
           (id exp))]
      [(list? exp)
       (cond
         [(symbol=? (car exp) 'cons) 
          (lcons (parser (cadr exp)) (parser (caddr exp)))]
         [(symbol=? (car exp) 'car)
          (lcar (parser (cadr exp) ))]
         
         [(or (symbol=? '+ (car exp)) 
              (symbol=? '- (car exp))
              (symbol=? '* (car exp))
              (symbol=? '/ (car exp)))
          
          (binop (car exp)(parser(cadr exp))(parser(caddr exp)))])])))
;; Interp
(define interp
  (lambda (exp)
    (type-case FWAEL exp
      [num (n) exp]
      [booleano (b) exp]
      [id (x) (error 'interp "error free id")]
      [binop (f l r) (func f (interp l)(interp r) )]
      [lempty () (lista-vacia)]
      [lcons (e c) (lista (interp e)(interp c))]
      [lcar (l) (lista-e (lista (interp (lcons-e l)) (interp (lcons-c l))))]
      )))

;;Function auxiliary for in the interp
(define func
  (lambda (s l r)
    (case s
      [(+) (num (+ (num-n l) (num-n r)))]
      [(-) (num (- (num-n l) (num-n r)))]
      [(*) (num (* (num-n l) (num-n r)))]
      [(/) (num (/ (num-n l) (num-n r)))])))
  
      