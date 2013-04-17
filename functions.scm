; Interpreter Project, Part 4
; Tom Dooner (ted27)
; Brian Stack (bis12)

; stmt is something like '(function other (x) ((return (+ x 1))))
;
; inserts the body of the function into the environment a tuple of (other 1)
; because this definition takes one formal parameter
(define interpret-declare-function
  (lambda (stmt env)
    ; the function closure:
    (add-to-environment
      (cons (cadr stmt) (list (number-of-parameters (caddr stmt))))
      (cddr stmt) env)))

(define call-function
  (lambda (function values-to-bind env)
    (get-environment 'return
      (call/cc (lambda (ret)
                 (let
                   ((funcenv (add-to-environment 'returnfunc ret (create-function-env (car (get-function function (number-of-parameters values-to-bind) env)) values-to-bind env))))
                 ;(begin (display "\nrunning the code for: ") (display function) (display "\n") (display (cadr (get-environment function env))) (display "\n") (display env) (display "\n")
                  (interpret-statement-list (cadr (get-function function (number-of-parameters values-to-bind) env)) funcenv)))))))

; gets the appropriately overloaded function from the environment
; returns the tuple containing first the formal parameters and second the code
(define get-function
  (lambda (function num-arguments env)
    (get-environment (cons function (list num-arguments)) env)))

(define number-of-parameters
  (lambda (values-to-bind)
    (cond
      ((null? values-to-bind) 0)
      ((eq? (car values-to-bind) '&) (number-of-parameters (cdr values-to-bind)))
      (else (+ 1 (number-of-parameters (cdr values-to-bind)))))))

; This function takes:
;   - a list of formal parameters from a function declaration, like:
;     '(x & y)
;   - a list of the values to bind from the calling environment, like:
;     '((+ z 3) h)
;   - the environment, the last layer of which is the global environment,
; and prepares a new environment that consists of the global environment with a
; new layer which contains only the call-by-value parameters bound.
(define create-function-env
  (lambda (formal-parameters values-to-bind env)
    (bind-formal-parameters
      formal-parameters
      values-to-bind
      env
      (add-layer (global-env-only env))))) ;(this could just be in call-function ?)

(define bind-formal-parameters
  (lambda (formal-parameter-list value-list env new-env)
    (cond
      ((null? formal-parameter-list) new-env)
      (else (if (eq? (car formal-parameter-list) '&)
              ; if calling by reference, store the original variable's box in the new environment
              (bind-formal-parameters (cddr formal-parameter-list) (cdr value-list) env (add-box-to-environment (cadr formal-parameter-list) (get-environment-box (car value-list) env) new-env))
              ; if calling by value, store the original variable's value in the new environment
              (bind-formal-parameters (cdr formal-parameter-list) (cdr value-list) env (add-to-environment (car formal-parameter-list) (interpret-stmt-value (car value-list) env) new-env)))))))

