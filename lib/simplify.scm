(include "symbolic.scm")

;; (simplify-algebra '(+ 1 2 3 4 0 5)) -> '(+ 1 2 3 4 5)
;; (simplify-algebra '(+ (+ 0) (* 0) (+ 0 (* 1 (^ 3 4)) 0 (* 2 (^ 3 2)) 0 (* 1 (^ 3 0)))))
;; '(+ (^ 3 4) (* 2 (^ 3 2)) 1)
;; > (simplify-algebra '(+ (+ 0) (* 0) (+ 0 (* (* 1) (^ 3 4)) 0 (* 2 (^ 3 2)) 0 (* 1 (^ 3 0)))))
;; '(+ (^ 3 4) (* 2 (^ 3 2)) 1)
;; > (simplify-algebra '(+ (* (+ 0)) (* 0) (+ 0 (* (* 1) (^ 3 4)) 0 (* 2 (^ 3 2)) 0 (* 1 (^ 3 0)))))
;; '(+ (^ 3 4) (* 2 (^ 3 2)) 1)
;; > (simplify-algebra '(+ (* (+ 0)) (* 0) (+ 0 (* (* 1) (^ 3 4)) 0 (* (* 2 1) (^ 3 2)) 0 (* 1 (^ 3 0)))))
;; '(+ (^ 3 4) (* 2 (^ 3 2)) 1)
;; > (simplify-algebra '(* (+ (* (+ 0)) (* 0) (+ 0 (* (* 1) (^ 3 4)) 0 (* (* 2 1) (^ 3 2)) 0 (* 1 (^ 3 0))))))
;;'(+ (^ 3 4) (* 2 (^ 3 2)) 1)
(define (simplify-algebra expr)
  (cond
   ((number? expr) expr)
   ((symbol? expr) expr)
   ((boolean? expr) expr)
   (else
    (cond
     ((is+? expr) (simplify+ expr))
     ((is^? expr) (simplify^ expr))
     ((is*? expr) (simplify* expr))
     (else expr)))))

;; (simplify+ '(+ 1 2 3 4 0 5)) -> '(+ 1 2 3 4 5)
;;
;; (simplify+ '(+ 1 2 (/ 8 0) 3 4 0 5)) -> '(+ 1 2 (/ 8 0) 3 4 5)
;;
;; (simplify-algebra '(+ 0 (+ 0 (* 1 (^ 3 4)) 0 (* 2 (^ 3 2)) 0 (* 1 (^ 3 0)))))
;; '(+ (^ 3 4) (* 2 (^ 3 2)) 1)
;; > (simplify-algebra '(+ (+ 0) (+ 0 (* 1 (^ 3 4)) 0 (* 2 (^ 3 2)) 0 (* 1 (^ 3 0)))))
;; '(+ (^ 3 4) (* 2 (^ 3 2)) 1)
;;
(define (simplify+ expr)
  ;;(cond

   ;;((unary-operation? expr) (simplify-algebra (arg expr)))

   ;; ((binary-operation? expr)
   ;;  (let ((a1-simp (simplify-algebra (arg1 expr)))
   ;; 	  (a2-simp (simplify-algebra (arg2 expr))))
   ;;    (cond ((zero-symb? a1-simp) a2-simp)
   ;; 	    ((zero-symb? a2-simp) a1-simp)
   ;; 	    (else `(,(operator expr) ,a1-simp ,a2-simp)))))

   ;;(else ;; n-arity operation
    (let* ((a (args expr)) ;; arguments list
	   (a-simp (map simplify-algebra a)) ;; simplified arguments list
	   (a-not-null ;; not null arguments list
	    (filter
	     (lambda (x) (not (zero-symb? x))) ;; simplify .... + 0 + ... + 0 .....
	     a-simp)))
      ;;(dv a-not-null)
      (cond
       ((null? a-not-null) 0)
       ((null? (rest a-not-null)) (first a-not-null))
       (else `(,(operator expr) ,@a-not-null)))));;))


(define (simplify* expr)
  
  ;;(cond
   
   ;;((unary-operation? expr) (simplify-algebra (arg expr)))

   ;; ((binary-operation? expr)
   ;;  (let ((a1-simp (simplify-algebra (arg1 expr)))
   ;; 	  (a2-simp (simplify-algebra (arg2 expr))))
   ;;    (cond ((zero-symb? a1-simp) 0)
   ;; 	    ((zero-symb? a2-simp) 0)
   ;; 	    ((unity-symb? a1-simp) a2-simp)
   ;; 	    ((unity-symb? a2-simp) a1-simp)
   ;; 	    (else `(,(operator expr) ,a1-simp ,a2-simp)))))

   ;;(else ;; n-arity operation
    (let* ((a (args expr)) ;; arguments list
	   (a-simp (map simplify-algebra a)) ;; simplified arguments list
	   (a-not-one '()));; not one in  arguments list
	 
      ;;(dv a-not-one)
      (if (member 0 a-simp)
	  0 ;; zero multiply somewhere
	  (begin
	    (set! a-not-one
		  (filter
		   (lambda (x) (not (unity-symb? x))) ;; simplify .... * 1 * ... * 1.....
		   a-simp))
      
	    (cond
	     ((null? a-not-one) 1) ;; only ones
	     ((null? (rest a-not-one)) (first a-not-one)) ;; only one element
	     (else `(,(operator expr) ,@a-not-one)))))));;))
   


(define (simplify^ expr)
  
    (let ((a1-simp (simplify-algebra (arg1 expr)))
	  (a2-simp (simplify-algebra (arg2 expr))))
      (cond ((zero-symb? a1-simp) a1-simp)
	    ((zero-symb? a2-simp) 1)
	    ((eqv? a2-simp 1) a1-simp)
	    (else `(,(operator expr) ,a1-simp ,a2-simp)))))
   

;; todo: associate ok + ko ^, n-arity evec + ?
;; > (prefix->infix '(+ a (+ b c)))
;; '(a + (b + c))
;; > (prefix->infix '(+ a b c)))
;; '(a + b + c)
