; Interpreter Project, Part 1
; Tom Dooner (ted27)
; Brian Stack (bis12)

(load "loopSimpleParser.scm")

; The heart of it all. Creates a default environment that is used, and at the
; end of interpreting the list of things, returns whatever value is put in the
; "return" variable in the environment
(define interpret
  (lambda (filename)
    (get-environment 'return
      (interpret-statement-list (parser filename) '((true #t) (false #f) (return None))))))

(define interpret-statement-list
  (lambda (parsetree env)
    (cond
      ((null? parsetree) env)
      (else
        ; This passes the updated environment into the next statement to
        ; be interpreted
        (interpret-statement-list (cdr parsetree) (interpret-stmt (car parsetree) env))))))

; Returns the new environment after the stmt is interpreted.
(define interpret-stmt
  (lambda (stmt env)
    (cond
      ((number? stmt) env)
      ((atom? stmt) env)
      ((member? (car stmt) '(+ - * / %)) env)     ; Environment is not changed
      ((eq? (car stmt) 'var) (interpret-declare stmt env))
      ((eq? (car stmt) '=) (interpret-assign stmt env))
      ((eq? (car stmt) 'if) (interpret-branch stmt env))
      ((eq? (car stmt) 'return) (interpret-ret stmt env)))
))

; Returns the value of the stmt based on the environment that is passed in.
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

; Handles '(return x)
; Returns updated environment
(define interpret-ret
  (lambda (stmt env)
    (update-environment 'return (interpret-stmt-value (cadr stmt) env) env)))

; Handles '(if (...) (...) (...))
; Returns updated environment
(define interpret-branch
  (lambda (stmt env)
    (if (interpret-bool-value (cadr stmt) (interpret-bool-env (cadr stmt) env))
      (interpret-stmt (caddr stmt) (interpret-bool-env (cadr stmt) env))
      (interpret-stmt (cadddr stmt) (interpret-bool-env (cadr stmt) env)))))

; Handles '(> (= x (+ x 1)) y)
; Returns updated environment
(define interpret-bool-env
  (lambda (stmt env)
    (interpret-stmt (cadr stmt) (interpret-stmt (caddr stmt) env))))

; Handles '(> (= x (+ x 1)) y)
; Returns #t or #f based on the environment and the variables
(define interpret-bool-value
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

; Handles '(var x)
; Returns updated environment
(define interpret-declare
  (lambda (stmt env)
    (cond
      ((null? (cddr stmt)) (add-to-environment (cadr stmt) '(None) env))
      (else (add-to-environment (cadr stmt)
              (cons (interpret-stmt-value (caddr stmt) env) '())
              (interpret-stmt (caddr stmt) env))))))

; Handles '(= x (+ x 1))
; Returns updated environment
(define interpret-assign
  (lambda (stmt env)
    (update-environment (cadr stmt) (interpret-stmt-value (caddr stmt) env) (interpret-stmt (caddr stmt) env))))

; Handles '(= x (+ x 1))
; Returns value, which is just the value of the caddr of it.
(define interpret-assign-value
  (lambda (stmt env)
    (cond
      ((number? (caddr stmt)) (caddr stmt))
      (else (interpret-stmt-value (caddr stmt) env)))))

; Helper function to abstract out binary operations like < > + - && ||, etc.
(define interpret-binary
  (lambda (op)
    (lambda (stmt env)
      (op (interpret-stmt-value (cadr stmt) env) (interpret-stmt-value (caddr stmt) env)))))

; Helper function for unary or binary subtraction
(define interpret-negative
  (lambda (stmt env)
    (cond
      ((eq? (length stmt) 3) ((interpret-binary -) stmt env))
      (else (- 0 (interpret-stmt-value (cadr stmt) env))))))

; Helper function for unary !
(define interpret-not
  (lambda (stmt env)
    (not (interpret-stmt-value (cadr stmt) env))))

; Helper function to add to the environment with the value 'None and return the
; new environment
(define add-to-environment
  (lambda (binding value env)
    (if (not (declared? binding env))
      (cons (cons binding value) env)
      (error "Error: You have already declared this variable!"))))

; Helper function to updates the value of a variable in the environment and
; return the new environment
(define update-environment
  (lambda (binding value env)
    (cond
      ((null? env) (error "Error: Trying to assign to an undeclared variable!"))
      ((eq? (car (car env)) binding)
       (cons (cons binding (cons value '())) (cdr env)))
      (else
        (cons (car env) (update-environment binding value (cdr env))))
      )))

; Helper function to retrieve the value of a variable from the environment
(define get-environment
  (lambda (binding env)
    (cond
      ((null? env) (error "Error: This variable has not been declared yet!"))
      ((eq? (car (car env)) binding) (cadr (car env)))
      (else (get-environment binding (cdr env))))))

; Helper function to return #t if binding is in the environment and #f if not.
(define declared?
  (lambda (binding env)
    (cond
      ((null? env) #f)
      ((eq? (car (car env)) binding) #t)
      (else (declared? binding (cdr env))))))

(define member?
  (lambda (item l)
    (cond
      ((null? l) #f)
      ((eq? (car l) item) #t)
      (else (member? item (cdr l))))))

(define atom?
  (lambda (x)
    (not (or (pair? x) (null? x)))))
