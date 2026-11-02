(define (fizzbuzz n)
  (cond ((= (mod n 15) 0) "FizzBuzz")
        ((= (mod n 3) 0) "Fizz")
        ((= (mod n 5) 0) "Buzz")
        (t n)))

(define (range a b)
  (if (> a b) nil (cons a (range (+ a 1) b))))

(map (lambda (n) (print (fizzbuzz n))) (range 1 15))
