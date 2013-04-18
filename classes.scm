; Interpreter Project, Part 4
; Tom Dooner (ted27)
; Brian Stack (bis12)

; interprets the class and returns an environment with the class added to it
; The class object contains:
;   [1] The environment of class (static) variables.
;   [2] The environment of instance variables, initialized to 'None
;   [3] Parent class name
;   (todo: also return "initial environment"?)
; e.g.
; class Main extends SomethingElse {
;   static var x = 5;
;   main() { return x; }
; }
; becomes...
; '((((main x true false return) (#&(() ((return x))) #&5 #&#t #&#f #&None)))
;   ()
;   SomethingElse)

(define interpret-class
  (lambda (name extends body env)
    (add-to-environment name
      (cons (interpret-class-body-list body env)
        (cons '()       ; no instance variables in this assignment!
          (cons (if (null? extends) 'None (cadr extends)) '())))
      env)
))

(define interpret-class-body-list
  (lambda (parsetree env)
    (cond
      ((null? parsetree) env)
      (else (interpret-class-body-list (cdr parsetree) (interpret-class-body (car parsetree) env))))))

(define interpret-class-body
  (lambda (stmt env)
    (cond
      ((eq? (car stmt) 'static-function) (interpret-declare-function stmt env))
      ((eq? (car stmt) 'static-var) (interpret-declare stmt env))
)))

(define get-class
  (lambda (classname env)
    (get-environment classname env)))

(define get-class-parsetree
  (lambda (classname env)
    (car (get-class classname env))))

(define interpret-dot-value
  (lambda (class binding env)
    (get-environment binding (get-class-parsetree class env))))

(define interpret-function-in-class
  (lambda (stmt env)
    (if (list? (cadr stmt))
      (call-function (caddr (cadr stmt)) (cddr stmt) (get-class-parsetree (cadr (cadr stmt)) env))
      (call-function (cadr stmt) (cddr stmt) env))))
