#lang plai

;; Tipo de datos que representa la sintaxis abstracta de FWAEL
(define-type FWAEL
  [num (n number?)]
  [bolean (b boolean?)]
  [id (v symbol?)]
  [binop (fun symbol?)
         (exp-izq FWAEL?)
         (exp-der FWAEL?)]
  [with (arterisco boolean?)
        (vars (listof bind?)) 
        (body FWAEL?)]
  [fun (param-for (listof symbol?)) 
       (fun-body FWAEL?)]
  [app (fun-exp FWAEL?) (param-reales (listof FWAEL?))]
  [lempty]
  [lcons (cabeza FWAEL?) (resto FWAEL?)]
  [lcar (lista FWAEL?)]
  [lcdr (lista FWAEL?)]
  )


;; Tipo de dato que representa un valor ligado a un identificador, 
;; un Binding consiste de un id de tipo symbol y un valor de tipo FWAEL
(define-type Binding
  [bind (nombre symbol?)
        (valor FWAEL?)])

;;
;;
(define-type Ambiente
  [mtSub]
  [aSub (id symbol?)
        (valor FWAEL-Value?)
        (env Ambiente?)])

;;
;;
(define-type FWAEL-Value
  [numV (n number?)]
  [boolV (b boolean?)]
  [closureV (parametros (listof symbol?))
            (cuerpo FWAEL?)
            (env Ambiente?)]
  [lemptyV]
  [lconsV (cabeza FWAEL-Value?)
          (resto FWAEL-Value?)])

;; Parsea Bindings
;; parsea-bindings: listof(B) -> listof(bind?)
(define parsea-bindings
  (lambda (lista-binds)
    (map (lambda (pareja)
           (bind (car pareja)
                 (parser (cadr pareja))))
         lista-binds)))

   
;;
;;
(define lookup
  (lambda (var amb)
    (type-case Ambiente amb
      [mtSub () (error 'interp (string-append "No se encontro el id:" 
                                              (symbol->string var) 
                                              " en el ambiente"))]
      [aSub (i v a) (if (symbol=? i var)
                        v
                        (lookup var a))]
      )))

;;
;;
(define parser
  (lambda (expresion)
    (cond
      [(number? expresion) (num expresion)]
      [(symbol? expresion) (if (symbol=? expresion 'lempty)
                               (lempty)
                               (id expresion))]
      [(boolean? expresion) (bolean expresion)]
      [(list? expresion)
       (case (car expresion)
         [(+ - * /) (binop (car expresion)
                           (parser (cadr expresion))
                           (parser (caddr expresion)))]
         [(fun) (fun (cadr expresion)
                     (parser (caddr expresion)))]
         [(with) (with #f 
                       (parsea-bindings (cadr expresion))
                       (parser (caddr expresion)))]
         [(with*) (with #t
                       (parsea-bindings (cadr expresion))
                       (parser (caddr expresion)))]
         [(lempty) (lempty)]
         [(lcons) (lcons (parser (cadr expresion))
                         (parser (caddr expresion)))]
         [(lcar) (lcar (parser (cadr expresion)))]
         [(lcdr) (lcdr (parser (cadr expresion)))]
         [else (app (parser (car expresion))
                    (map parser (cdr expresion)))]
         )]
      )))

;;
;; interp
(define interp
  (lambda (expr amb)
    (type-case FWAEL expr
      [num (n) (numV n)]
      [bolean (b) (boolV b)]
      [id (v) (lookup v amb)]
      [binop (fun lizq lder) (opera-fwael fun 
                                          (interp lizq amb) 
                                          (interp lder amb))]
      [lempty () (lemptyV)]
      [lcons (cabeza resto) (lconsV (interp cabeza amb)
                                    (interp resto amb))]
      [lcar (lista) (lconsV-cabeza (interp lista amb))]
      [lcdr (lista) (lconsV-resto (interp lista amb))]
      [with (ast params body) (if ast
                                  1
                                  (interp body (foldl (lambda (b env)
                                                        (aSub (bind-nombre b)
                                                              (interp (bind-valor b) env)
                                                              env))
                                                      amb
                                                      params)))]
      [fun (lst-params cuerpo) (closureV lst-params 
                                         cuerpo 
                                         amb)]
      [app (fun-expr arg-expr) (let ([val-fun (interp fun-expr amb)]
                                     [val-arg (map (lambda (a)
                                                     (interp a amb)) arg-expr)])
                                 (type-case FWAEL-Value val-fun
                                   [closureV (params-cl cuerpo-cl amb-cl)
                                             (interp cuerpo-cl
                                                     (foldl (lambda (ls la env)
                                                              (aSub ls la env))
                                                            amb-cl
                                                            params-cl
                                                            val-arg))]
                                   [else (error 'app "no se puede interpretar")]
                                   ))]
      )))

;;
;;
(define opera-fwael
  (lambda (f i d)
    (case f
      [(+) (numV (+ (numV-n i)
                    (numV-n d)))]
      [(-) (numV (- (numV-n i)
                    (numV-n d)))]
      [(*) (numV (* (numV-n i)
                    (numV-n d)))]
      [(/) (numV (/ (numV-n i)
                    (numV-n d)))]
    )))

;;
(define prueba
  (lambda (exp)
    (interp (parser exp) (mtSub))))

;;Funciones de ejemplo de prueba que vienen en el pdf
(define p1
  (prueba '(with {[cubo {fun {n} {* n {* n n}}}]}
                 {lcons {cubo 1} {lcons {cubo 2} {lcons {cubo 3} lempty}}})))
  
(define p2
  (prueba '(with {[lista {lcons 1 {lcons 2 {lcons 3 lempty}}}]
                  [doble {fun{x} {+ x x}}]}
                 {doble {lcar {lcdr lista}}})))
(define p3
  (prueba '(with {[lista {lcons {fun {x} x} {lcons {fun {x} {/ 1 x}} {lcons 3 lempty}}}]}
                 {{lcar lista} {{lcar {lcdr lista}} {lcar {lcdr {lcdr lista}}}}}))
  )