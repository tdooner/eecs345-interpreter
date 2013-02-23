(load "verySimpleParser.scm")

(define interpret
  (lambda (filename)
    (let ([environment '()])
      (interpret-statement-list (parse filename) environment))))

(define interpret-statement-list
  (lambda (parsetree environment)
    ((null? parsetree) environment)
    (else
      (interpret-stmt (car parsetree) environment))))

(define interpret-stmt
  (lambda (stmt environment)
    (cond
      ((eq? (car stmt) '=) (interpret-assign (stmt environment)))
      ; and so forth, for all the stuff we need...
      ;((eq? (car stmt) '+) (interpret-assign (stmt environment)))
      ;((eq? (car stmt) '-) (interpret-assign (stmt environment)))
      ;((eq? (car stmt) '*) (interpret-assign (stmt environment)))
      ;((eq? (car stmt) '/) (interpret-assign (stmt environment)))
      ;((eq? (car stmt) '%) (interpret-assign (stmt environment)))
      ;((eq? (car stmt) '-) (interpret-assign (stmt environment)))  ;unary -
      ;
      ;((eq? (car stmt) '<) (interpret-assign (stmt environment)))
      ;((eq? (car stmt) '>) (interpret-assign (stmt environment)))
      ;((eq? (car stmt) '>=) (interpret-assign (stmt environment)))
      ;((eq? (car stmt) '<=) (interpret-assign (stmt environment)))
      ;((eq? (car stmt) '!=) (interpret-assign (stmt environment)))
      ;((eq? (car stmt) '==) (interpret-assign (stmt environment)))
      ;
      ;((eq? (car stmt) '&&) (interpret-assign (stmt environment)))
      ;((eq? (car stmt) '||) (interpret-assign (stmt environment)))
      ;((eq? (car stmt) '!) (interpret-assign (stmt environment)))  ;unary !
)))
