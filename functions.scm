; Interpreter Project, Part 4
; Tom Dooner (ted27)
; Brian Stack (bis12)

; stmt is something like '(function other (x) ((return (+ x 1))))
;
; inserts the body of the function into the environment a tuple of (other 1)
; because this definition takes one formal parameter
(define interpret-declare-function
  (lambda (stmt env class object)
    (let*
      (
       (function-signature (cons (cadr stmt) (list (number-of-parameters (caddr stmt))))) ; (funcname 3)
       (function-body (cddr stmt))
       (with-rest-of-class (lambda (v) (cons v (cdr (get-class class env)))))
      )
      (update-environment class (with-rest-of-class (add-to-environment function-signature function-body (get-class-parsetree class env))) env)
    ; the function closure:
    ;(add-to-environment
    ;  (cons (cadr stmt) (list (number-of-parameters (caddr stmt))))
    ;  (cddr stmt) env)
)))

(define call-function
  (lambda (function values-to-bind env class) ;todo next assignment: add "object" to this
    ;(print "Trying to call function ")
    ;(print function)
    ;(display " in class ") (display class) (display "\n")

    (let*
      (
       (prev-env (car env))
       (class-env (cons (car (get-class-parsetree class env)) (global-env-only env))) ; this is a magical "car" <- it makes things work
       ;(_ (pretty-print class-env))
       (function-signature (cons function (list (number-of-parameters values-to-bind))))
       (exists-in-class? (declared-in-environment? function-signature class-env))
      )

      ;(display "We will call that function with environment: ")

      (if exists-in-class?
        ; if the function signature exists in the current class, call it!
        (get-environment 'return
          (call/cc (lambda (ret)
                     (let*
                       (
                        (func (get-function function-signature class-env))
                        ;(_ (begin (display "Going to bind values: ") (pretty-print values-to-bind)))
                        (function-env (add-to-environment 'returnfunc ret (create-function-env (car func) values-to-bind (cons prev-env class-env) class)))
                        ;(_ (pretty-print function-env))
                       )
                       ; (display "Calling!\n")
                       (interpret-statement-list (cadr func) function-env class 'noobjecthere))))
        )
        ; if the function signature doesn't exist in the current class, try
        ; to call the method on its parent class, if a parent class exists
        ((if (eq? (get-class-parent class env) 'None)
          (error "Error: Could not find function definition for " function)
          (call-function function values-to-bind env (get-class-parent class env))))
))))

; gets the appropriately overloaded function from the environment
;   function-signature is something like: (main 0)
; returns the tuple containing first the formal parameters and second the code
(define get-function
  (lambda (function-signature env)
    (get-environment function-signature env)))

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
  (lambda (formal-parameters values-to-bind env class)
    (if (= (length (filter (lambda (x) (not (eq? x '&))) formal-parameters)) (length values-to-bind))
      (bind-formal-parameters
        formal-parameters
        values-to-bind
        env
        (add-layer (global-env-only env))
        class
      )
      (error "Incorrect number of arguments!"))))

(define bind-formal-parameters
  (lambda (formal-parameter-list value-list env new-env class)
    (cond
      ((null? formal-parameter-list) new-env)
      (else (if (eq? (car formal-parameter-list) '&)
              ; if calling by reference, store the original variable's box in the new environment
              (bind-formal-parameters
                (cddr formal-parameter-list)
                (cdr value-list)
                env
                (add-box-to-environment (cadr formal-parameter-list) (get-environment-box (car value-list) env) new-env)
                class
              )
              ; if calling by value, store the original variable's value in the new environment
              (bind-formal-parameters
                (cdr formal-parameter-list)
                (cdr value-list)
                env
                (add-to-environment (car formal-parameter-list) (interpret-stmt-value (car value-list) env class 'noobject) new-env)
              class
              )
)))))

