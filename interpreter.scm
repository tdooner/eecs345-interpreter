(load "verySimpleParser.scm") (define interpret
  (lambda (filename)
    (get-environment 'return 
      (interpret-statement-list (parser filename) '((true #t) (false #f) (return None))))))

(define interpret-statement-list
  (lambda (parsetree env)
    (cond
      ((null? parsetree) env)
      (else
        (interpret-statement-list (cdr parsetree) (interpret-stmt (car parsetree) env))))))

(define interpret-stmt
  (lambda (stmt env)
    (cond
      ((number? stmt) env)
      ((eq? (car stmt) 'var) (interpret-declare stmt env))
      ((eq? (car stmt) '=) (interpret-assign stmt env))
      ((eq? (car stmt) 'if) (interpret-branch stmt env))
      ((eq? (car stmt) 'return) (interpret-ret stmt env))

      ;
      ; todo: create a function called something like interpret-stmt-truth that
      ; determines the truthiness of a parsetree root and wire it in here when
      ; we see an "if"
      ;((eq? (car stmt) '<) (interpret-assign (stmt env)))
      ;((eq? (car stmt) '>) (interpret-assign (stmt env)))
      ;((eq? (car stmt) '>=) (interpret-assign (stmt env)))
      ;((eq? (car stmt) '<=) (interpret-assign (stmt env)))
      ;((eq? (car stmt) '!=) (interpret-assign (stmt env)))
      ;((eq? (car stmt) '==) (interpret-assign (stmt env)))
      ;
      ;((eq? (car stmt) '&&) (interpret-assign (stmt env)))
      ;((eq? (car stmt) '||) (interpret-assign (stmt env)))
      ;((eq? (car stmt) '!) (interpret-assign (stmt env)))  ;unary !
)))

(define interpret-stmt-value
  (lambda (stmt env)
    (cond
      ((null? stmt) 'None)
      ((number? stmt) stmt)
      ((atom? stmt) (get-environment stmt env))
      ((eq? (car stmt) '=) (interpret-assign-value stmt env))
      ((eq? (car stmt) '+) ((interpret-binary +) stmt env))
      ((eq? (car stmt) '-) (interpret-negative stmt env))
      ((eq? (car stmt) '*) ((interpret-binary *) stmt env))
      ((eq? (car stmt) '/) ((interpret-binary /) stmt env))
      ((eq? (car stmt) '%) ((interpret-binary remainder) stmt env))
)))

(define interpret-ret
  (lambda (stmt env)
    (update-environment 'return (interpret-stmt-value (cadr stmt) env) env)))

(define interpret-branch
  (lambda (stmt env)
    (if (interpret-bool (cadr stmt) env)
      (interpret-stmt (caddr stmt) env)
      (interpret-stmt (cadddr stmt) env))))

(define interpret-bool
  (lambda (stmt env)
    (cond
      ((atom? stmt) (get-environment stmt env))
      ((eq? (car stmt) '==) ((interpret-binary eq?) stmt env))
      ((eq? (car stmt) '!=) ((interpret-binary (lambda (x y) (not (eq? x y)))) stmt env))
      ((eq? (car stmt) '<) ((interpret-binary <) stmt env))
      ((eq? (car stmt) '>) ((interpret-binary >) stmt env))
      ((eq? (car stmt) '<=) ((interpret-binary <=) stmt env))
      ((eq? (car stmt) '>=) ((interpret-binary >=) stmt env))
      ((eq? (car stmt) '&&) ((interpret-binary (lambda (x y) (and x y))) stmt env))
      ((eq? (car stmt) '||) ((interpret-binary (lambda (x y) (or x y))) stmt env))
      ((eq? (car stmt) '!) (interpret-not stmt env))
)))

(define interpret-declare
  (lambda (stmt env)
    (cond
      ((null? (cddr stmt)) (add-to-environment (cadr stmt) '(None) env))
      (else (add-to-environment (cadr stmt) (cons (interpret-stmt-value (caddr stmt) env) '()) env)))))

(define interpret-assign
  (lambda (stmt env) 
    (update-environment (cadr stmt) (interpret-stmt-value (caddr stmt) env) env)))

(define interpret-assign-value
  (lambda (stmt env)
    (cond
      ((number? (caddr stmt)) (caddr stmt))
      (else (interpret-stmt-value stmt env)))))

(define interpret-binary
  (lambda (op)
    (lambda (stmt env)
      (op (interpret-stmt-value (cadr stmt) env) (interpret-stmt-value (caddr stmt) env)))))

(define interpret-negative
  (lambda (stmt env)
    (cond
      ((eq? (length stmt) 3) ((interpret-binary -) stmt env))
      (else (- 0 (interpret-stmt-value (cadr stmt) env))))))

(define interpret-not
  (lambda (stmt env)
    (not (interpret-stmt-value (cadr stmt) env))))

; declares a variable and adds it to the environment with the value 'None
; for "var x;"
(define add-to-environment
  (lambda (binding value env)
    (if (not (declared? binding env))
      (cons (cons binding value) env)
      (error "Error: You have already declared this variable!"))))

; updates a variable that is already in the environment with a new value
; for "x = 5;"
(define update-environment
  (lambda (binding value env)
    (cond
      ((null? env) (error "Error: Trying to assign to an undeclared variable!"))
      ((eq? (car (car env)) binding)
       (cons (cons binding (cons value '())) (cdr env)))
      (else
        (cons (car env) (update-environment binding value (cdr env))))
      )))

(define get-environment
  (lambda (binding env)
    (cond
      ((null? env) (error "Error: This variable has not been declared yet!"))
      ((eq? (car (car env)) binding) (cadr (car env)))
      (else (get-environment binding (cdr env))))))

; #t, if binding is in env
; #f, else
(define declared?
  (lambda (binding env)
    (cond
      ((null? env) #f)
      ((eq? (car (car env)) binding) #t)
      (else (declared? binding (cdr env))))))

(define atom?
  (lambda (x)
    (not (or (pair? x) (null? x)))))
