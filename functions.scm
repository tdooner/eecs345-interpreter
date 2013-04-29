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
  (lambda (function values-to-bind env class object)
    (get-environment 'return
      (call/cc (lambda (ret)
        (call-function-sub function values-to-bind (add-to-environment 'returnfunc ret (add-layer (global-env-only env))) class object))))))

(define call-function-sub
  (lambda (function values-to-bind env class object)
    (let*
      (
       (function-signature (cons function (list (number-of-parameters values-to-bind))))
       ;(_ (begin (display "Trying to call function ") (pretty-print function-signature)))
       (class-env (get-class-parsetree class env))
       ;(_ (begin (display " in env ") (pretty-print class-env)))
       (exists-in-object? #f) ;todo <----
       (exists-in-class? (declared-in-environment? function-signature class-env))
       ;(prev-env (car env))
       ;(class-env (cons (car (get-class-parsetree class env)) (global-env-only env))) ; this is a magical "car" <- it makes things work
       ;(_ (pretty-print class-env))
      )


      ;(print "Does it exist in the class?")
      ;(pretty-print exists-in-class?)
      ;(pretty-print class-env)

      (if exists-in-object?
        (display "then we should call the function in the object")
        ; if the method is not in the object, look in the parent class:
        (if exists-in-class?
          (let*
            (
             (func (get-function function-signature class-env))
             (function-env (create-function-env (car func) values-to-bind env class))
             ;(_ (begin
             ;     (display "Calling function")
             ;     (pretty-print func)
             ;     (display " with env: ")
             ;     (pretty-print function-env)))
            )
            ; (display "Calling!\n")
            (interpret-statement-list (cadr func) function-env class 'noobjecthere)
          )

          ; if not exists-in-class? then:
          ((if (eq? (get-class-parent class env) 'None)
                    (error "Error: Could not find function definition for " function)
                    (call-function-sub function values-to-bind env (get-class-parent class env) object))))

      ))))

      ;(display "We will call that function with environment: ")

;      (if exists-in-class?
;        ; if the function signature exists in the current class, call it!
;        ; if the function signature doesn't exist in the current class, try
;        ; to call the method on its parent class, if a parent class exists
;        )))


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
        (add-to-environment 'return 'None env)
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

