; Library forms that need no interpreter support.

(define (not x) (if x nil t))
(define (cadr l) (car (cdr l)))

(define (foldl f acc l)
  (if (null? l) acc (foldl f (f acc (car l)) (cdr l))))
(define (reverse l) (foldl (lambda (acc x) (cons x acc)) nil l))
(define (length l) (foldl (lambda (n _) (+ n 1)) 0 l))
(define (map f l)
  (reverse (foldl (lambda (acc x) (cons (f x) acc)) nil l)))
(define (filter p l)
  (reverse (foldl (lambda (acc x) (if (p x) (cons x acc) acc)) nil l)))
(define (append a b)
  (foldl (lambda (acc x) (cons x acc)) b (reverse a)))

(defmacro let (bindings &rest body)
  `((lambda ,(map car bindings) ,@body) ,@(map cadr bindings)))

(defmacro cond (&rest clauses)
  (if (null? clauses)
      nil
      `(if ,(car (car clauses))
           (begin ,@(cdr (car clauses)))
           (cond ,@(cdr clauses)))))

(defmacro and (&rest xs)
  (cond ((null? xs) t)
        ((null? (cdr xs)) (car xs))
        (t `(if ,(car xs) (and ,@(cdr xs)) nil))))

; The gensym keeps the first value from being evaluated twice or captured by the rest.
(defmacro or (&rest xs)
  (cond ((null? xs) nil)
        ((null? (cdr xs)) (car xs))
        (t (let ((v (gensym)))
             `(let ((,v ,(car xs))) (if ,v ,v (or ,@(cdr xs))))))))
