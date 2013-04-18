; Interpreter Project, Part 4
; Tom Dooner (ted27)
; Brian Stack (bis12)

; Check if something is in something
(define member?
  (lambda (item l)
    (cond
      ((null? l) #f)
      ((equal? (car l) item) #t)
      (else (member? item (cdr l))))))

; Helper function to return #t if binding is in some level of the environment
; and #f if not.
(define declared?
  (lambda (binding level)
    (member? binding (car level))))

(define declared-in-environment?
  (lambda (binding env)
    (cond
      ((null? env) #f)
      ((declared? binding (car env)) #t)
      (else (declared? binding (del-layer env))))))

; Helper function to add to the environment with the value 'None and return the
; new environment
(define add-to-environment
  (lambda (binding value env)
      (cons
        (add-to-layer binding value (car env))
        (cdr env))
))

(define add-box-to-environment
  (lambda (binding value env)
      (cons
        (add-box-to-layer binding value (car env))
        (cdr env))
))

; Adds a binding to a layer
(define add-to-layer
  (lambda (binding value layer)
    (if (declared? binding layer)
      (error "Error: You have already declared this variable in this scope!")
      (cons
        (cons binding (car layer))
        (cons (cons (box value) (cadr layer)) '())))))

; Adds a binding to a layer
(define add-box-to-layer
  (lambda (binding value layer)
    (if (declared? binding layer)
      (error "Error: You have already declared this variable in this scope!")
      (cons
        (cons binding (car layer))
        (cons (cons value (cadr layer)) '())))))

; Helper function to updates the value of a variable in the environment and
; return the new environment
(define update-environment
  (lambda (binding value env)
    (cond
      ((null? env) (error "Error: Trying to assign to an undeclared variable!"))
      ((member? binding (car (car env)))
       (cons (set-layer binding value (car env)) (cdr env)))
      (else
        (cons (car env) (update-environment binding value (cdr env))))
      )))

; Adds a layer to the environment
(define add-layer
  (lambda (env)
    (cons '(()()) env)))

; Removes a layer from the environment
(define del-layer
  (lambda (env)
    (cdr env)))

; Change the value of a variable in a layer
(define set-layer
  (lambda (binding value layer)
    (cond
      ((null? layer) layer)
      ((equal? (caar layer) binding) (let ((layer layer))
        (set-box! (caadr layer) value)
        layer))
      (else
        (let
          ((next-env (set-layer binding value (cons (cdar layer) (cons (cdadr layer) '())))))
          (if (declared? (caar layer) next-env)
            (error "Error: You have already declared this variable in this scope!")
            (cons
              (cons (caar layer) (car next-env))
              (cons (cons (caadr layer) (cadr next-env)) '()))))))))

; Helper function to retrieve the value of a variable from the environment
(define get-environment
  (lambda (binding env)
    ;(begin (display "\n  ")(display binding) (display "\n  ") (display env)
    (cond
      ((null? env) (error "Error: Variable " binding " has not been declared yet!"))
      ((member? binding (caar env)) (get-from-layer binding (car env)))
      (else (get-environment binding (cdr env))))));)

(define get-environment-box
  (lambda (binding env)
    (cond
      ((null? env) (error "Error: This variable has not been declared yet!"))
      ((member? binding (caar env)) (get-from-layer-box binding (car env)))
      (else (get-environment-box binding (cdr env))))))

(define get-from-layer-box
  (lambda (binding layer)
    (cond
      ((null? layer) layer)
      ((equal? (caar layer) binding) (caadr layer))
      (else (get-from-layer-box binding
              (cons (cdar layer) (cons (cdadr layer) '())))))))

; Given a layer in the environment like '((x y z) (1 2 3)) it will look up the
; value of a binding
(define get-from-layer
  (lambda (binding layer)
    (cond
      ((null? layer) layer)
      ((equal? (caar layer) binding) (unbox (caadr layer)))
      (else (get-from-layer binding
              (cons (cdar layer) (cons (cdadr layer) '())))))))

; Helper function to take a big environment and return only the last part,
; which is the global environment
(define global-env-only
  (lambda (env)
    (cond
      ((null? (cdr env)) env)
      ((null? (cddr env)) env)
      (else global-env-only (cdr env)))))
