;; useful binary and arithmetic functions



(include "simplify.scm")

;; binary list to true-false list
;; (binList2TrueFalseList '(1 1 0 1 0 0)) -> (#t #t #f #t #f #f)
(define (binList2TrueFalseList L)
  (if (null? L)
      L
      (if (= (car L) 1)
          (cons #t (binList2TrueFalseList (cdr L)))
          (cons #f (binList2TrueFalseList (cdr L))))))

;; true-false list to binary list
;; >  (trueFalseList2binList '(#t #t #f #f #t #t #t #t)) -> '(1 1 0 0 1 1 1 1)
(define (trueFalseList2binList L)
  (map boolean->binary L))



;; number2binlist : convert a number in a list containing its binary number conversion
;; (number2binlist #b10110) --> (1 0 1 1 0)
(define (number2binlist n)

  (cond 

   ((zero? n) '(0))

   ((= 1 n) '(1))

   ((= (modulo n 2) 1) (append (number2binlist (quotient n 2)) (list 1)))

   (else (append (number2binlist (quotient n 2)) (list 0)))))


;; (number->poly-base-2 #b10110) -> '(+ (^ 2 4) (^ 2 2) (^ 2 1))
;;
(define (number->poly-base-2 n)
  
  (let ((deg 0)  ;; polynom degree
	(poly-prefix '())
	(poly-prefix-simp '()))

    (letrec

	((number->poly-base-2-rec

	  (lambda (n) 

	    ;;(dv deg)
	    (cond 
	     
	     ((zero? n) '(0))
	     
	     ;;((= 1 n) (quasiquote #;((2 ^ ,deg)) ((^ 2 ,deg))))
	     
	     ((= (modulo n 2) 1)
	      (let ((monomial (quasiquote #;((2 ^ ,deg)) ((^ 2 ,deg)))))
		(set! deg (+ 1 deg))
		(append
		 (number->poly-base-2-rec (quotient n 2))
		 monomial)))
		
	     (else
		(set! deg (+ 1 deg))
		(append (number->poly-base-2-rec (quotient n 2)) (list 0)))))))
	 
      (set! poly-prefix (cons '+ (number->poly-base-2-rec n)))
      ;;(dv poly-prefix)
      (set! poly-prefix-simp (simplify-algebra poly-prefix))
      ;;(dv poly-prefix-simp)
           
      poly-prefix-simp)))


(define (number->poly-base-k n k)
  
  (let ((deg 0)  ;; polynom degree
	(poly-prefix '())
	(poly-prefix-simp '()))

    (letrec

	((number->poly-base-k-rec

	  (lambda (n k) 

	    (dv deg)
	    
	    (let ((r (modulo n k))) ;; remainder
	      
	      (cond 
	     
	       ((zero? n) '(0))
	       
	       ;;((= 1 r) (quasiquote  ((^ ,k ,deg))))
	     
	       ((not (zero? r))
		(let ((monomial (quasiquote  ((* ,r (^ ,k ,deg)))))
		      (q (quotient n k)))
		  (set! deg (+ 1 deg))
		  (append
		   (number->poly-base-k-rec q k)
		   monomial)))
	       
	       (else
		
		  (set! deg (+ 1 deg))
		  (append
		   (number->poly-base-k-rec (quotient n k) k)
		   (list 0))))))))
	 
      (set! poly-prefix (cons '+ (number->poly-base-k-rec n k)))
      (dv poly-prefix)
      (set! poly-prefix-simp (simplify-algebra poly-prefix))
      (dv poly-prefix-simp)
           
      poly-prefix-simp)))



(define (number->poly-base-k-expt n k)
  
  (let ((deg 0)  ;; polynom degree
	(poly-prefix '())
	(poly-prefix-simp '()))

    (letrec

	((number->poly-base-k-rec-expt

	  (lambda (n k) 

	    ;;(dv deg)
	    
	    (let ((r (modulo n k))) ;; remainder
	      
	      (cond 
	     
	       ((zero? n) '(0))
	       
	       ((not (zero? r))
		(let ((monomial (quasiquote  ((* ,r (expt ,k ,deg)))))
		      (q (quotient n k)))
		  (set! deg (+ 1 deg))
		  (append
		   (number->poly-base-k-rec-expt q k)
		   monomial)))
	       
	       (else
		
		  (set! deg (+ 1 deg))
		  (append
		   (number->poly-base-k-rec-expt (quotient n k) k)
		   (list 0))))))))
	 
      (set! poly-prefix (cons '+ (number->poly-base-k-rec-expt n k)))
      (dv poly-prefix)
      (set! poly-prefix-simp (simplify-algebra poly-prefix))
      (dv poly-prefix-simp)
           
      poly-prefix-simp)))





;; (number->poly-base-2-infix #b10110) -> '((2 ^ 4) + (2 ^ 2) + (2 ^ 1))
;;
(define (number->poly-base-2-infix n)
  (prefix->infix (number->poly-base-2 n)))

(define (number->poly-base-k-infix n k)
  (prefix->infix (number->poly-base-k n k)))



;; (number->hereditary-base-2 #b10110)
;; -> '(+ (^ 2 (^ 2 (^ 2 (^ 2 0)))) (^ 2 (^ 2 (^ 2 0))) (^ 2 (^ 2 0)))
;;
;; (number->hereditary-base-2 35) -> (+ (^ 2 (+ (^ 2 2) 1)) 2 1)
(define (number->hereditary-base-2 n)
  
  (let ((deg 0)  ;; polynom degree
	(poly-prefix '())
	(poly-prefix-simp '()))

    (letrec

	((number->hereditary-base-2-rec

	  (lambda (n) 

	    ;;(dv deg)
	    (cond 
	     
	     ((zero? n) '(0))
	     
	     ;;((= 1 n) (quasiquote  ((^ 2 ,(number->hereditary-base-2 deg)))))
	     
	     ((= (modulo n 2) 1)
	      (let ((monomial
		     (quasiquote ((^ 2 ,(number->hereditary-base-2 deg))))))

		(set! deg (+ 1 deg))
		(append
		 (number->hereditary-base-2-rec (quotient n 2))
		 monomial)))
		
	     (else
		(set! deg (+ 1 deg))
		(append (number->hereditary-base-2-rec (quotient n 2)) (list 0)))))))
	 
      (set! poly-prefix (cons '+ (number->hereditary-base-2-rec n)))
      (dv poly-prefix)
      (set! poly-prefix-simp (simplify-algebra poly-prefix))
      (dv poly-prefix-simp)
      
      poly-prefix-simp)))
      


;; (number->hereditary-base-k 100 3) -> '(+ (^ 3 (+ 3 1)) (* 2 (^ 3 2)) 1)
(define (number->hereditary-base-k n k)
  
  (let ((deg 0)  ;; polynom degree
	(poly-prefix '())
	(poly-prefix-simp '()))

    (letrec

	((number->hereditary-base-k-rec

	  (lambda (n k) 

	    (if (zero? n)
		'(0)
	    
	    (let ((r (modulo n k))) ;; remainder
	      
	      ;;(dv deg)

	      (if (not (zero? r))
		
		  (let ((monomial
			 (quasiquote ((* ,r (^ ,k ,(number->hereditary-base-k deg k))))))
			(q (quotient n k)))
		  
		    (set! deg (+ 1 deg))
		    (append
		     (number->hereditary-base-k-rec q k)
		     monomial))
		
		  (begin
		    
		    (set! deg (+ 1 deg))
		    (append
		     (number->hereditary-base-k-rec (quotient n k) k)
		     (list 0)))))))))
      
      (set! poly-prefix (cons '+ (number->hereditary-base-k-rec n k)))
      ;;(dv poly-prefix)
      (set! poly-prefix-simp (simplify-algebra poly-prefix))
      ;;(dv poly-prefix-simp)
      
      poly-prefix-simp)))


;; (number->hereditary-base-k-expt 100 3) -> '(+ (expt 3 (+ 3 1)) (* 2 (expt 3 2)) 1)
;;
;; (number->hereditary-base-k-expt  22876792454964 3) -> '(+ (expt 3 (+ (expt 3 3) 1)) 3)
;;
;; (number->hereditary-base-k-expt  7625597484990 3) -> '(+ (expt 3 (expt 3 3)) 3)
;;
;;  (number->hereditary-base-k-expt   173646739038364 5)
;;
;; '(+
;;   (expt 5 (* 4 5))
;;   (* 4 (expt 5 (+ (* 3 5) 4)))
;;   (* 2 (expt 5 (+ (* 3 5) 2)))
;;   (* 3 (expt 5 (+ (* 3 5) 1)))
;;   (expt 5 (+ (* 2 5) 3))
;;   (* 2 (expt 5 (+ (* 2 5) 2)))
;;   (expt 5 (* 2 5))
;;   (expt 5 (+ 5 3))
;;   (* 4 (expt 5 (+ 5 2)))
;;   (* 3 (expt 5 (+ 5 1)))
;;   (* 2 (expt 5 5))
;;   (expt 5 4)
;;   (expt 5 3)
;;   (* 4 (expt 5 2))
;;   (* 2 5)
;;   4)
(define (number->hereditary-base-k-expt n k)
  
  (let ((deg 0)  ;; polynom degree
	(poly-prefix '())
	(poly-prefix-simp '()))

    (letrec

	((number->hereditary-base-k-rec-expt

	  (lambda (n k) 

	    (if (zero? n)
		'(0)
	    
	    (let ((r (modulo n k))) ;; remainder
	      
	      ;;(dv deg)

	      (if (not (zero? r))
		
		  (let ((monomial
			 (quasiquote ((* ,r (expt ,k ,(number->hereditary-base-k-expt deg k))))))
			(q (quotient n k)))
		  
		    (set! deg (+ 1 deg))
		    (append
		     (number->hereditary-base-k-rec-expt q k)
		     monomial))
		
		  (begin
		    
		    (set! deg (+ 1 deg))
		    (append
		     (number->hereditary-base-k-rec-expt (quotient n k) k)
		     (list 0)))))))))
      
      (set! poly-prefix (cons '+ (number->hereditary-base-k-rec-expt n k)))
      ;;(dv poly-prefix)
      (set! poly-prefix-simp (simplify-algebra poly-prefix))
      ;;(dv poly-prefix-simp)
      
      poly-prefix-simp)))



;; (number->hereditary-base-2-infix #b10110)
;; -> '((2 ^ (2 ^ (2 ^ (2 ^ 0)))) + (2 ^ (2 ^ (2 ^ 0))) + (2 ^ (2 ^ 0)))
;;
;; (number->hereditary-base-2-infix 26)
;; -> '((2 ^ (2 ^ 2)) + (2 ^ (2 + 1)) + 2)
;; previous -> '((2 ^ (2 ^ (2 ^ (2 ^ 0)))) + (2 ^ ((2 ^ (2 ^ 0)) + (2 ^ 0))) + (2 ^ (2 ^ 0)))
;;
(define (number->hereditary-base-2-infix n)
  (prefix->infix (number->hereditary-base-2 n)))


;; (number->hereditary-base-k-infix  22876792454964 3)
;; -> '((3 ^ ((3 ^ 3) + 1)) + 3)
;;
;; (number->hereditary-base-k-infix  109 7) -> '((2 * (7 ^ 2)) + 7 + 4)
;;
;; (number->hereditary-base-k-infix  299 12) -> '((2 * (12 ^ 2)) + 11)
;;
(define (number->hereditary-base-k-infix n k)
  (prefix->infix (number->hereditary-base-k n k)))

;; binlist2number : convert a binary list to a number
;;>  (binlist2number '(1 1 0 0 1 1 1 1)) -> 207
(define (binlist2number L)
  (letrec ((revL (reverse L)) ; reversed list
	   (reverseBinList->number

	    (lambda (revBinLst expo) ; starting exposant 

	      (if-t debug-mode
		    (display "calling reverseBinList") (newline)
		    (display "... revBinLst : ") (display revBinLst) (newline)
		    (display "... expo : ") (display expo) (newline) (newline))

	      (if (null? revBinLst)

		  0 ;; zero is not only represented by an empty list but it will do the job of ending computation
		  
		  (let* ((Bk (first revBinLst))
			 (BkX2Pk (* Bk (expt 2 expo)))) ;; Bk * 2^k
		    (+ 
		     (begin
		       (if-t debug-mode
			   (display "...... Bk : ") (display Bk) (newline)
			   (display "...... Bk * (2 ^ expo) : ") (display BkX2Pk) (newline))
		       BkX2Pk)
		     (let ((restRBL2n (reverseBinList->number (rest revBinLst) (1+ expo))))
		       (begin
			 (if-t debug-mode
			       (display "...... restRBL2n : ") (display restRBL2n) (newline))
			 restRBL2n ))))))))

    (reverseBinList->number revL 0))) ; expo set to 0 at beginning


    
(define (boolean->binary b)
  (if b 1 0))

;; convert binary string number to carries string
;; (binary-string->carries "   101101110" ) -> "   1 11 111 "
(define (binary-string->carries s)
  (string-replace s "0" " "))

;; convert binary string number to carries string
(define (binary-string->carries-c s)
  (string-replace (string-replace s "0" " ") "1" "C"))

;; display binary numbers with padding
;;  (padding #b10110) -> "0000000000010110"
(define (padding x)
  (~r x #:base 2 #:min-width 24 #:pad-string "0"))

;; display binary numbers with padding
;;  (padding-spc #b10110) -> "                   10110"
;;
;; WARNING : use monospace font in DrRacket to get constant spacing
;;
(define (padding-spc x)
  (~r x #:base 2 #:min-width 24 #:pad-string " "))

;; (flag-set? #b10 #b11) -> #t
;; (flag-set? #b100 #b11) -> #f
(define (flag-set? f x)
  (= (bitwise-and f x) f))

;; logarithme binaire
(define (lb x)
  (/ (log x) (log 2)))

;; increment variable
;; nota: DrRacket Scheme has it own add1 function 
(define-syntax 1+
  (syntax-rules ()
    ((_ x)   (begin (set! x (+ x 1)) x))))


;; shift left a binary number
(define-syntax shift-left
  (syntax-rules ()
    ((_ x) (arithmetic-shift x 1))
    ((_ x n) (arithmetic-shift x n))))

;; shift right a binary number
(define-syntax shift-right
  (syntax-rules ()
    ((_ x) (arithmetic-shift x -1))
    ((_ x n) (arithmetic-shift x (- n)))))


;; non symbolic functions
;;

;; compute Cout, the 'carry out' of the result of Cin + A + B
(define (compute-carry Cin A B)
  ;; (compute-carry #t #f #t) -> #t
  (xor (and A B) (and Cin (xor A B))))
 
(define (compute-sum Cin A B)
  ;; (compute-sum #f #t #t) -> #f
  ;; (compute-sum #t #t #t) -> #t
  (xor Cin (xor A B)))


(define-syntax macro-function-compare-2-bits-with-continuation ;; continuation version of macro-compare-2-bits
  ;; i need a macro because of external function to the clozure
  (syntax-rules ()
    ((_) (let ((cnt 0)) ;; counter
	   (lambda (continuation b1 b2) (if (equal? b1 b2)
					  b1
					  (begin
					    (set! cnt (add1 cnt))
					    (when (> cnt 1) (continuation #f)) ;; escaping with the continuation
					    'x)))))))

(define-syntax macro-return-function-compare-2-bits-with-kontinuation ;; continuation version of macro-compare-2-bits
  ;; i need a macro because of external function to the clozure
  (syntax-rules (kontinuation)
    ((_) (let ((cnt 0)) ;; counter
			(lambda (b1 b2) (if (equal? b1 b2)
					  b1
					  (begin
					    (set! cnt (add1 cnt))
					    (when (> cnt 1) (kontinuation #f)) ;; escaping with the continuation
					    'x)))))))



(define-syntax macro-compare-2-bits ;; i need a macro because of external variable to the clozure
  (syntax-rules ()
    ((_ condition) (let ((cnt 0)) ;; counter
		     (lambda (b1 b2) (if (equal? b1 b2)
				       b1
				       (begin
					 (set! cnt (add1 cnt))
					 (when (> cnt 1) (set! condition #t))
					 'x)))))))


(define-syntax macro-compare-2-bits-with-continuation ;; continuation version of macro-compare-2-bits
  ;; i need a macro because of external function to the clozure
  (syntax-rules ()
    ((_ continuation) (let ((cnt 0)) ;; counter
			(lambda (b1 b2) (if (equal? b1 b2)
					  b1
					  (begin
					    (set! cnt (add1 cnt))
					    (when (> cnt 1) (continuation #f)) ;; escaping with the continuation
					    'x)))))))



(define-syntax macro-compare-2-bits-with-kontinuation ;; continuation version of macro-compare-2-bits
  ;; i need a macro because of external function to the clozure
  (syntax-rules (#;kontinuation)
    ((_) (let ((cnt 0)) ;; counter
			(lambda (b1 b2) (if (equal? b1 b2)
					  b1
					  (begin
					    (set! cnt (add1 cnt))
					    (when (> cnt 1) (kontinuation #f)) ;; escaping with the continuation
					    'x)))))))




;; function  version of macro-compare-2-bits-with-continuation
;;
;; > (call/cc (lambda (k) (function-compare-2-bits-with-continuation k 1 0)))
;; cnt = 1

;; 'x
;; > (call/cc (lambda (k) (function-compare-2-bits-with-continuation k 1 0)))
;; cnt = 2

;; #f
(define function-compare-2-bits-with-continuation 
 
  (let ((cnt 0)) ;; counter
    (lambda (continuation x y) (if (equal? x y)
				   x
				   (begin
				     (set! cnt (add1 cnt))
				     ;;(dv cnt)
				     ;;(newline)
				     (when (> cnt 1) (continuation #f)) ;; escaping with the continuation
				     'x)))))


;; exclusive or
(define (xor p q) 
  ;; (xor #f #f) -> #f
  ;; (xor #t #f) -> #t
  (or (and p (not q)) (and (not p) q)))

;; (xor 0 0) -> 0
;; (xor 1 0) -> 1
;;
;; for DrRacket Scheme
;;(bitwise-xor p q))
;; (if (and (equal? p 1) (equal? q 1))
;;     0
;;     1))

  
;; symbolic exclusive or
(define (symb-xor p q) 
  ;; (symb-xor 'p 'q) -> '(or (and p (not q)) (and (not p) q))
  `(or (and ,p (not ,q)) (and (not ,p) ,q)))



;; adder circuit functions

;; symbolic functions
;;

;; compute Sum symbolically
;; return result of Cin + A + B (Cin being 'carry in')
(define (symb-compute-sum  Cin A B)
  ;; (symb-compute-sum 'Ci 'a 'b) -> '(or (and Ci (not (or (and a (not b)) (and (not a) b)))) (and (not Ci) (or (and a (not b)) (and (not a) b))))
  ;; (enlight-dnf (symb-compute-sum 'Ci 'a 'b)) -> (a^b^Ci)v(!a^!b^Ci)v(!a^b^!Ci)v(a^!b^!Ci)
  (symb-xor Cin (symb-xor A B)))

(define (symb-compute-carry Cin A B)
  ;; (symb-compute-carry 'Ci 'a 'b)
  ;; -> '(or (and (and a b) (not (and Ci (or (and a (not b)) (and (not a) b)))))
  ;;   (and (not (and a b)) (and Ci (or (and a (not b)) (and (not a) b)))))
  ;;
  ;; (enlight-dnf (symb-compute-carry 'Ci 'a 'b)) -> (a^b)v(a^b^!Ci)v(a^!b^Ci)v(!a^b^Ci)
  ;; (prefix->infix (simplify (n-arity (simplify-OR (simplify-AND (dnf (symb-compute-carry 'C 'a 'b)))))))
  ;;  -> '((a and !b and C) or (!a and b and C) or (a and b) or (a and b and !C))
  ;; todo: mettre sous forme disjunctive minimale
  (symb-xor `(and ,A ,B) `(and ,Cin  ,(symb-xor A B))))

;; > (goodstein 3)
;; b = 2
;; hi = (2 + 1)
;; n = 3

;; b = 3
;; hi = 3
;; n = 3

;; b = 4
;; hi = 3
;; n = 2

;; b = 5
;; hi = 2
;; n = 1

;; b = 6
;; hi = 1
;; n = 0
;;
;;
;; > (goodstein 266)
;; b = 2
;; hi = ((2 ^ (2 ^ (2 + 1))) + (2 ^ (2 + 1)) + 2)

;; b = 3
;; hi = ((3 ^ (3 ^ (3 + 1))) + (3 ^ (3 + 1)) + 2)

;; b = 4
;; hi = ((4 ^ (4 ^ (4 + 1))) + (4 ^ (4 + 1)) + 1)

;; b = 5
;; hi = ((5 ^ (5 ^ (5 + 1))) + (5 ^ (5 + 1)))

;; b = 6
;; . . ../git/LOGIKI/lib/binary_arithmetic.scm:330:21: user break
;;
;; G(13)(280)=(+ (expt 281 (+ 281 1)) (* 3 (expt 281 3)) (* 2 (expt 281 2)) (* 61 281) 230)
;;
;; test with 257
(define (goodstein n)

  (let ((n-start n)
	(h '()) ;; hereditary base b
	(hi '()) ;; infix
	(hs '()) ;; hereditary base b+1
	(omega (string->symbol (string #\u03C9))) ;; omega symbol
	
	(hi-omega '()) ;; hereditary infix omega 
	(b 2))
    
    (while (not (= n 0))

	   ;; dv : display variable
	   ;;(display "G(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
	   (set! h (number->hereditary-base-k-expt n b))
	   ;;(display-nl h)
	   ;; convertir ,ne pas recalculer
	   (set! hi (prefix->infix h))
	   (set! hi
		 (replace hi 'expt '^)) ;; expt))
	   ;;(set! hi (number->hereditary-base-k-infix n b))
	   (display "G(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
	   (display-nl hi)
	   (set! hi-omega 
		 (replace hi b omega))
	   (display "P(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
	   (display-nl hi-omega)
	   (set! hs ;; bump the base
		 (replace h b (+ 1 b)))
	   ;;(dv hs)
	   (set! b (+ 1 b))
	   (set! n (- (eval hs) 1)) ;; substract 1
	   ;;(dv n)
	   (newline))
    
    (display "G(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
    (display-nl n)
    (display "P(") (display n-start) (display ")(") (display (- b 1)) (display ")=0")))

    

;; G(13)(280)=(+ (expt 281 (+ 281 1)) (* 3 (expt 281 3)) (* 2 (expt 281 2)) (* 61 281) 230)
(define (goodstein-optim n)

  (let ((n-start n)
	(h '()) ;; hereditary base b expression
	(hi '()) ;; infix expression
	(hs '()) ;; hereditary base b+1 expression
	(omega (string->symbol (string #\u03C9))) ;; omega symbol
	
	(hi-omega '()) ;; hereditary infix omega 
	(b 2)
	(hs-rev '()) ;; reverse of base b+1 hereditary expression
	(monomial '())
	(monomial-1 '())
	(hs-rev-rest '())
	(hs-rev-result '())
	)
    
    (while (not 
	    (and (number? h)
		 (= h 0)))

	   ;; dv : display variable
	   ;;(display "G(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
	   
	   (when (= 2 b) ;; only at the beginning
		 (set! h (number->hereditary-base-k-expt n b)))

	   ;;(display-nl h)
	   
	   ;; convertir ,ne pas recalculer
	   (set! hi (prefix->infix h))
	   (set! hi
		 (replace hi 'expt '^)) ;; expt))
	   ;;(set! hi (number->hereditary-base-k-infix n b))
	   
	   (display "G(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
	   
	   (display-nl hi)

	   (set! hi-omega 
		 (replace hi b omega))
	   (display "P(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
	   (display-nl hi-omega)

	   (set! hs ;; bump the base
		 (replace h b (+ 1 b)))
	   ;;(dv hs)

	   (set! b (+ 1 b))

	   ;;(set! n (- (eval hs) 1)) ;; substract 1
	   
	   (if (number? hs) ;; sometimes it's a number , not a list
	       (set! h (- hs 1))
	       (begin
		 (set! hs-rev (reverse hs))
		 ;;(dv hs-rev)
		 (set! monomial (first hs-rev))
		 ;;(dv monomial)
		 (set! hs-rev-rest (rest hs-rev))
		 ;;(dv hs-rev-rest)
		 (set! monomial-1
		       (number->hereditary-base-k-expt 
			(- (eval monomial) 1)
			b))
		 ;;(dv monomial-1)
		 (cond ((number? monomial-1)
			(if (zero? monomial-1)
			    (if (pair-list? hs-rev-rest)
				(set! hs-rev-result (first hs-rev-rest))
				(set! hs-rev-result hs-rev-rest))
			    (set! hs-rev-result (insert monomial-1 hs-rev-rest))))
		       ((is*? monomial-1)
			(set! hs-rev-result (insert monomial-1 hs-rev-rest)))
		       (else
			(set! hs-rev-result
			      (append
			       (reverse (args monomial-1))
			       hs-rev-rest))))
		 
		 (if (not (number? hs-rev-result))
		     (set! h (reverse hs-rev-result))
		     (set! h hs-rev-result))))
	   
	   ;;(dv n)
	   (newline))

    (display "G(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
    (display-nl h)
    (display "P(") (display n-start) (display ")(") (display (- b 1)) (display ")=0")
    
    )) 
