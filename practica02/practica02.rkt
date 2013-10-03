#lang plai
;; Interprete de CFWAE, con ambientes y closures
;; que implementa alcance estatico.

;; Data-type que representa la sintaxis abstracta de CFWAE. 
(define-type CFWAE
  [num (n number?)]
  [id (v symbol?)]
  [binop (fun procedure?)
         (lexp CFWAE?)
         (rexp CFWAE?)]
  [with (vars (listof bind?))
        (body CFWAE?)]
  [if0 (cond0 CFWAE?)
       (then0 CFWAE?)
       (else0 CFWAE?)]
  [fun (par-for (listof symbol?))
       (fun-body CFWAE?)]
  [app (fun-exp CFWAE?)
       (real-params (listof CFWAE?))])



;pruebas
(define prueba '{with* {{x 5} 
                 {w {+ x 1}}
                 {z {with* {{x 10}
                           {f {fun {a} {+ x a}}}}
                          {f 10}}}}
                {+ x z}})

(define prueba2 '{with* {{x 1}
                         {y 1}
                         {z {with* {{x 2}
                                    {y 2}
                                    {f {fun {a} {+ a {- x y}}}}} 
                                   {f 10}}}}
                        {with* {(f {fun {g} {g {- z {+ x y}}}})
                                (x 3)
                                (y 3)} 
                               {f {with* {(x 4)
                                          (y 4)} 
                                         {fun {k} {- k {+ x y}}}}}}})

(define prueba3 '{with* {(x 1)
                         (y 1)} 
                        {with* {(f {fun {k} {+ k {+ x y}}})} 
                               {with* {(x 2)
                                       (y 2)} 
                                      {f 10}}}})


;; Toma una lista de numeros, simbolos o listas
;; y las transforma a un arbol de sintaxis abstractade CFWAE.
;; parser: A -> CFWAE
;; (define (parser expresion))
(define parser
  (lambda (exp)
    (cond
      [(number? exp) (num exp)]
      [(symbol? exp) (id exp)]
      [(list? exp)
       (case (car exp)
         [(+ - * /) (binop (select-fun (car exp)) 
                           (parser (cadr exp))
                           (parser (caddr exp)))]
         [(if0) (if0 (parser (cadr exp))
                     (parser (caddr exp))
                     (parser (cadddr exp)))]
         [(with) (parser (append (list (list 'fun (extrae-variables (cadr exp)) (caddr exp)))
                                 (extrae-valores (cadr exp))))]
         [(with*) (with (parsea-binding (cadr exp))
                        (parser (caddr exp)))]
         [(fun) (fun (cadr exp)
                     (parser (caddr exp)))]
         [else (app (parser (car exp))
                    (map parser (cdr exp)))]
         )])))

;; Toma un arbol de sintaxis abstracta CFWAE y lo 
;; reduce a su minima expresion.
;; interp: CFWAE Env -> CFWAE-value
;; (define (interp expresion ambiente))
(define interp
  (lambda (expresion amb)
    (type-case CFWAE expresion
      [num (n) (numV n)]
      [id (v) (lookup v amb)]
      [binop (fun lexp rexp)
             (opera-numV fun (interp lexp amb)
                  (interp rexp amb))]
      [if0 (condicional then else)
           (let ([es-cero (interp condicional amb)])
             (if (es-num-cero? es-cero)
                 (interp then amb)
                 (interp else amb)))]
      [with (variables-w cuerpo-w)
            (interp cuerpo-w
                    (foldl interpreta-bind
                           amb
                           variables-w))]
      [fun (params cuerpo)
           (closureV params cuerpo amb)]
      [app (funcion argumentos)
           (let ([valor-funcion (interp funcion amb)]
                 [valor-argumentos (map (lambda (valor) (interp valor amb)) argumentos)])
             (type-case CFWAE-value valor-funcion
               [closureV (parm-closu cuerpo-closu amb-closu)
                         (interp cuerpo-closu
                                 (empareja-args parm-closu valor-argumentos amb))]
               [else (error 'app "Las aplicaciones esperan una funcion y sus parametros")]))]
      )))



;; Data-type que representa un valor ligado a un identificador.
(define-type Binding
  [bind (name symbol?) (value CFWAE?)])


;; Data-type que representa el valor que toma nuestro interprete.
(define-type CFWAE-value
  [numV (n number?)]
  [closureV (parametros (listof symbol?))
           (cuerpo CFWAE?)
           (ambiente Env?)])

;; Parsea la lista de bindings en sintaxis concreta.
;; parsea-binding: listof(B) -> listof(bind?)
;; (define (parsea-binding lista))
(define parsea-binding
  (lambda (lista-bindings)
    (map (lambda (bin) (bind (car bin)
                             (parser (cadr bin)))) 
         lista-bindings)
    ))

;; Interpreta un binding mientras lo agrega
;; al ambiente para extenderlo.
;; interpreta-bind: Binding Env -> Env
;; (define (interpreta-bind binding ambiente))
(define interpreta-bind
  (lambda (binding amb)
    (aSub (bind-name binding)
          (interp (bind-value binding) amb)
          amb)
    ))

;; Dada una lista de simbolos y una de CFWAE-value se encarga de 
;; asociar un simbolo con un valor, las listas deben de tener el mismo tamano,
;; asi se pueden agregar al ambiente para extenderlo.
;; empareja-args: listof(symbol?) listof(CFWAE-value?) Env -> Env
(define empareja-args
  (lambda (cl-params args-val amb)
    (if (empty? cl-params)
        amb
        (empareja-args (cdr cl-params)
                       (cdr args-val)
                       (aSub (car cl-params) (car args-val) amb)))))

;; Data-type que representa los ambientes de sustitucion.
(define-type Env
  [mtSub]
  [aSub (name symbol?)
        (value CFWAE-value?)
        (env Env?)])

;; Se encarga de buscar una variable dentro de un ambiente
;; y si lo encuentra regresa el valor correspondiente o un 
;; error si no lo encuentra.
;; lookup: symbol Env -> CFWAE
;; (define (lookup variable ambiente))
(define lookup
  (lambda (var amb)
    (type-case Env amb
      [mtSub () (error 'lookup
                       (string-append "no hay identificador para: '"
                                      (symbol->string var)))]
      [aSub (bound-name bound-value amb-rest)
            (if (symbol=? var bound-name)
                bound-value
                (lookup var amb-rest))]
      )))

;; Dada una lista de listas mas pequenas nos permite
;; obtener las variables de esta.
;; extrae-variables: listof(list?) -> listof(symbol?)
(define extrae-variables
  (lambda (lista)
    (if (empty? lista)
        empty
        (cons (caar lista) (extrae-variables (cdr lista))))))

;; Dada una lista de listas mas pequenas nos permite 
;; obtener los valores relacionados con esta.
;; extrae-valores: listof(list?) -> listof(number?)
(define extrae-valores
  (lambda (lista)
    (if (empty? lista)
        empty
        (cons (cadar lista) (extrae-valores (cdr lista))))))

;; Se encarga de aplicar una funcion aritmetica a los valores
;; de numV, en el caso de la division no se encarga de verificar que
;; el denominador sea diferente de 0.
;; opera-numV: procedure CFWAE-value CFWAE-value -> CFWAE-value
;; (define (opera-numV funcion ladoIzquierdo ladoDerecho))
(define opera-numV
  (lambda (fun lexp rexp)
    (numV (fun (numV-n lexp)
               (numV-n rexp)))))

;; Nos permite saber si un CFWAE-value es igual al numero 0.
;; es-num-cero?: CFWAE-value -> boolean
;; (define (es-num-cero? valor))
(define es-num-cero?
  (lambda (valor)
    (if (= 0 (numV-n valor))
        #t
        #f)))

;; Dado un simbolo de funcion nos regresa la funcion 
;; correspondiente.
;; select-fun: symbol -> procedure
(define select-fun
  (lambda (operacion)
    (case operacion
      [(+) +]
      [(-) -]
      [(*) *]
      [(/) /]
      )))