; Interpreter Project, Part 3
; Tom Dooner (ted27)
; Brian Stack (bis12)

; Check if something is in something
(define member?
  (lambda (item l)
    (cond
      ((null? l) #f)
      ((eq? (car l) item) #t)
      (else (member? item (cdr l))))))

; Helper function to return #t if binding is in some level of the environment
; and #f if not.
(define declared?
  (lambda (binding level)
    (member? binding (car level))))

; Helper function to add to the environment with the value 'None and return the
; new environment
(define add-to-environment
  (lambda (binding value env)
      (cons
        (add-to-layer binding value (car env))
        (cdr env))
))

; Adds a binding to a layer
(define add-to-layer
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
      ((eq? (caar layer) binding)
       (cons (car layer) (cons (cons value (cdadr layer)) '())))
      (else
        (let
          ((next-env (set-layer binding value (cons (cdar layer) (cons (cdadr layer) '())))))
          ; note: here we're using add-to-layer as a utility to do the cons'ing
          (add-to-layer (caar layer) (caadr layer) next-env))
))))

; Helper function to retrieve the value of a variable from the environment
(define get-environment
  (lambda (binding env)
    (cond
      ((null? env) (error "Error: This variable has not been declared yet!"))
      ((member? binding (caar env)) (get-from-layer binding (car env)))
      (else (get-environment binding (cdr env))))))

; Given a layer in the environment like '((x y z) (1 2 3)) it will look up the
; value of a binding
(define get-from-layer
  (lambda (binding layer)
    (cond
      ((null? layer) layer)
      ((eq? (caar layer) binding) (caadr layer))
      (else (get-from-layer binding
              (cons (cdar layer) (cons (cdadr layer) '())))))))

; Helper function to take a big environment and return only the last part,
; which is the global environment
(define global-env-only
  (lambda (env)
    (cond
      ((null? (cdr env)) env)
      (else global-env-only (cdr env)))))
