(define (zero-symb? x)
  (and (number? x) (zero? x)))

(define (unity-symb? x)
  (and (number? x) (=  1 x)))
