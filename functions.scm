; Interpreter Project, Part 4
; Tom Dooner (ted27)
; Brian Stack (bis12)

; stmt is something like '(function other (x) ((return (+ x 1))))
(define interpret-declare-function
  (lambda (stmt env)
    ; the function closure:
    (add-to-environment (cadr stmt) (cddr stmt) env)))

(define call-function
  (lambda (function values-to-bind env)
    (get-environment 'return
      (call/cc (lambda (ret)
                 (let
                   ((funcenv (add-to-environment 'returnfunc ret (create-function-env (car (get-environment function env)) values-to-bind env))))
                   ;(begin (display function) (display "\n") (display (cadr (get-environment function env))) (display "\n") (display env) (display "\n")
                   (interpret-statement-list (cadr (get-environment function env)) funcenv)))))));)

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
    (if (= (length (filter (lambda (x) (not (eq? x '&))) formal-parameters)) (length values-to-bind))
      (bind-formal-parameters
        formal-parameters
        values-to-bind
        env
        (add-layer (global-env-only env))) ;(this could just be in call-function ?)
      (error "Incorrect number of arguments!"))))

(define bind-formal-parameters
  (lambda (formal-parameter-list value-list env new-env)
    (cond
      ((null? formal-parameter-list) new-env)
      (else (if (eq? (car formal-parameter-list) '&)
              ; if calling by reference, store the original variable's box in the new environment
              (bind-formal-parameters
                (cddr formal-parameter-list)
                (cdr value-list)
                env
                (add-box-to-environment (cadr formal-parameter-list) (get-environment-box (car value-list) env) new-env))
              ; if calling by value, store the original variable's value in the new environment
              (bind-formal-parameters
                (cdr formal-parameter-list)
                (cdr value-list)
                env
                (add-to-environment (car formal-parameter-list) (interpret-stmt-value (car value-list) env) new-env)))))))

