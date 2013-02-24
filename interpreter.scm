(load "verySimpleParser.scm")

(define interpret
  (lambda (filename)
    (let ((env '()))
      (interpret-statement-list (parser filename) env))))

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

      ; and so forth, for all the stuff we need...
      ;((eq? (car stmt) '-) (interpret-assign (stmt env)))  ;unary -
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
      ((eq? (car stmt) '=) (interpret-assign-value stmt env))
      ((eq? (car stmt) '+) (interpret-add stmt env))
      ((eq? (car stmt) '-) (interpret-sub stmt env))
      ((eq? (car stmt) '*) (interpret-mul stmt env))
      ((eq? (car stmt) '/) (interpret-div stmt env))
      ((eq? (car stmt) '%) (interpret-mod stmt env))
)))

(define interpret-declare
  (lambda (stmt env)
    (cond
      ((null? (cddr stmt)) (add-to-environment (cadr stmt) '(None) env))
      (else (add-to-environment (cadr stmt) (cddr stmt) env)))))

(define interpret-assign
  (lambda (stmt env) 
      (update-environment (cadr stmt) (interpret-stmt-value (caddr stmt) env) (interpret-stmt (caddr stmt) env))))

(define interpret-assign-value
  (lambda (stmt env)
    (cond
      ((number? (caddr stmt)) (caddr stmt))
      (else (interpret-stmt-value stmt)))))

(define interpret-add
  (lambda (stmt env)
    (+ (cadr stmt) (caddr stmt))))

(define interpret-sub
  (lambda (stmt env)
    (- (cadr stmt) (caddr stmt))))

(define interpret-mul
  (lambda (stmt env)
    (* (cadr stmt) (caddr stmt))))

(define interpret-div
  (lambda (stmt env)
    (/ (cadr stmt) (caddr stmt))))

(define interpret-mod
  (lambda (stmt env)
    (remainder (cadr stmt) (caddr stmt))))

; declares a variable and adds it to the environment with the value 'None
; for "var x;"
(define add-to-environment
  (lambda (binding value env)
    (if (not (declared? binding env))
      (cons (cons binding value) env)
      (error "You have already declared this variable!"))))

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

; #t, if binding is in env
; #f, else
(define declared?
  (lambda (binding env)
    (cond
      ((null? env) #f)
      ((eq? (car (car env)) binding) #t)
      (else (declared? binding (cdr env))))))
