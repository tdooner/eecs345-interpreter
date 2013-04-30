; Interpreter Project, Part 5
; Tom Dooner (ted27)
; Brian Stack (bis12)
;
; called with: (new A 1 2 ..)
; returns a class object, which is a tuple of:
;   [1]: the symbol 'object
;   [2]: the symbol of the class name, or 'None
;   [3]: the instance variables from the class definition
(define interpret-instantiate-value
  (lambda (stmt env class object)
    (let*
      (
       (class-name (cadr stmt))
       (constructor-args (cddr stmt)) ; nothing happens with these yet
       (class-def (get-class-instance-stuff class-name env))
       (object-def (cons 'object (cons class-name class-def)))
      )
      object-def
)))

(define is-object?
  (lambda (var)
    (cond
      ((not (list? var)) #f)
      ((eq? 'object (car var)) #t))))

; todo: add a branch in interpret-dot-value that detects if (is-object? class) is true and calls the method on the object accordingly. Otherwise, call the method on the class.
