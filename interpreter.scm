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
        (begin
          (set! env (interpret-stmt (car parsetree) env))
          (interpret-statement-list (cdr parsetree) env))
        ))))

(define interpret-stmt
  (lambda (stmt env)
    (cond
      ((eq? (car stmt) '=) (interpret-assign stmt env))
      ((eq? (car stmt) '+) (interpret-add stmt env))
      ((eq? (car stmt) '-) (interpret-sub stmt env))
      ((eq? (car stmt) '*) (interpret-mul stmt env))
      ((eq? (car stmt) '/) (interpret-div stmt env))

      ; and so forth, for all the stuff we need...
      ;((eq? (car stmt) '%) (interpret-assign (stmt env)))
      ;((eq? (car stmt) '-) (interpret-assign (stmt env)))  ;unary -
      ;
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

(define interpret-assign
  (lambda (stmt env)
    (cond
      ((list? (caddr stmt)) (update-environment (cadr stmt) (interpret-stmt (caddr stmt) env) env))
      (else
        (update-environment (cadr stmt) (caddr stmt) env)))))

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

(define update-environment
  (lambda (binding value env)
    (cond
      ((null? env) (cons (cons binding (cons value '())) '()))
      ((eq? (car (car env)) binding) (cons (cons binding (cons value '())) (cdr env)))
      (else (cons (car env) (update-environment binding value (cdr env))))
      )))
