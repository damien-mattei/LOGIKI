;; goodstein.scm
;;
;; Copyright (C) 2016-2017  Damien MATTEI
;;
;; damien.mattei@orange.fr
;;
;;
;;
;;
;;
;;    This program is free software: you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation, either version 3 of the License, or
;;    (at your option) any later version.

;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.

;;    You should have received a copy of the GNU General Public License
;;    along with this program.  If not, see <http://www.gnu.org/licenses/>

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
;; > (goodstein 266)
;; G(266)(1)=((2 ^ (2 ^ (2 + 1))) + (2 ^ (2 + 1)) + 2)
;; P(266)(1)=((ω ^ (ω ^ (ω + 1))) + (ω ^ (ω + 1)) + ω)

;; G(266)(2)=((3 ^ (3 ^ (3 + 1))) + (3 ^ (3 + 1)) + 2)
;; P(266)(2)=((ω ^ (ω ^ (ω + 1))) + (ω ^ (ω + 1)) + 2)

;; G(266)(3)=((4 ^ (4 ^ (4 + 1))) + (4 ^ (4 + 1)) + 1)
;; P(266)(3)=((ω ^ (ω ^ (ω + 1))) + (ω ^ (ω + 1)) + 1)

;; G(266)(4)=((5 ^ (5 ^ (5 + 1))) + (5 ^ (5 + 1)))
;; P(266)(4)=((ω ^ (ω ^ (ω + 1))) + (ω ^ (ω + 1)))

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
	   ;;(display-nl (eval h))

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

    
;; > (goodstein-optim 266)
;; G(266)(1)=((2 ^ (2 ^ (2 + 1))) + (2 ^ (2 + 1)) + 2)
;; P(266)(1)=((ω ^ (ω ^ (ω + 1))) + (ω ^ (ω + 1)) + ω)
;; 3 - 1 = 2

;; G(266)(2)=((3 ^ (3 ^ (3 + 1))) + (3 ^ (3 + 1)) + 2)
;; P(266)(2)=((ω ^ (ω ^ (ω + 1))) + (ω ^ (ω + 1)) + 2)
;; 2 - 1 = 1

;; G(266)(3)=((4 ^ (4 ^ (4 + 1))) + (4 ^ (4 + 1)) + 1)
;; P(266)(3)=((ω ^ (ω ^ (ω + 1))) + (ω ^ (ω + 1)) + 1)
;; 1 - 1 = 0

;; G(266)(4)=((5 ^ (5 ^ (5 + 1))) + (5 ^ (5 + 1)))
;; P(266)(4)=((ω ^ (ω ^ (ω + 1))) + (ω ^ (ω + 1)))
;; (6 ^ (6 + 1)) - 1 = ((5 * (6 ^ 6)) + (5 * (6 ^ 5)) + (5 * (6 ^ 4)) + (5 * (6 ^ 3)) + (5 * (6 ^ 2)) + (5 * 6) + 5)

;; G(266)(5)=((6 ^ (6 ^ (6 + 1))) + (5 * (6 ^ 6)) + (5 * (6 ^ 5)) + (5 * (6 ^ 4)) + (5 * (6 ^ 3)) + (5 * (6 ^ 2)) + (5 * 6) + 5)
;; P(266)(5)=((ω ^ (ω ^ (ω + 1))) + (5 * (ω ^ ω)) + (5 * (ω ^ 5)) + (5 * (ω ^ 4)) + (5 * (ω ^ 3)) + (5 * (ω ^ 2)) + (5 * ω) + 5)
;; 5 - 1 = 4

;; G(266)(6)=((7 ^ (7 ^ (7 + 1))) + (5 * (7 ^ 7)) + (5 * (7 ^ 5)) + (5 * (7 ^ 4)) + (5 * (7 ^ 3)) + (5 * (7 ^ 2)) + (5 * 7) + 4)
;; P(266)(6)=((ω ^ (ω ^ (ω + 1))) + (5 * (ω ^ ω)) + (5 * (ω ^ 5)) + (5 * (ω ^ 4)) + (5 * (ω ^ 3)) + (5 * (ω ^ 2)) + (5 * ω) + 4)
;; 4 - 1 = 3
;;
(define (goodstein-optim n)

  (let ((n-start n) ;; TODO : remove n-start
	(h '()) ;; hereditary base b expression
	(hi '()) ;; infix expression
	(hs '()) ;; hereditary base b+1 expression
	(hsi '()) ;; infix expression
	(omega (string->symbol (string #\u03C9))) ;; omega symbol
	
	(hi-omega '()) ;; hereditary infix omega 
	(b 2)
	(hs-rev '()) ;; reverse of base b+1 hereditary expression
	(monomial '())
	(monomial-1 '())
	(hs-rev-rest '())
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
	   ;;(display-nl (eval h))

	   (set! hi-omega 
		 (replace hi b omega))
	   (display "P(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
	   (display-nl hi-omega)

	   (set! hs ;; bump the base
		 (replace h b (+ 1 b)))
	   ;; convertir ,ne pas recalculer
	   ;; (set! hsi (prefix->infix hs))
	   ;; (set! hsi
	   ;; 	 (replace hsi 'expt '^)) ;; expt))
	   ;; (dv hsi)

	   (set! b (+ 1 b))

	   ;;(set! n (- (eval hs) 1)) ;; substract 1
	   
	   (cond ((number? hs) ;; sometimes it's a number , not a list
		  (set! h (- hs 1)))
	       
		 ((is+? hs)
		  (set! hs-rev (reverse hs)) ;; reverse to get the monomial first (low polynomial degrees firsts)
		  ;;(dv hs-rev)
		  (set! monomial (first hs-rev)) ;; get lower degree monomial
		  ;;(dv monomial)
		  (display (prefix->infix (expt->^ monomial)))
		  (display " - 1 = ")
		  (set! hs-rev-rest (rest hs-rev)) ;; keep the rest
		  ;;(dv hs-rev-rest)
		  (set! monomial-1
			(number->hereditary-base-k-expt 
			 (- (eval monomial) 1) ;; substract one
			 b))
		  ;;(dv monomial-1)
		  (display (prefix->infix (expt->^ monomial-1)))
		  (newline)
		  (cond ((number? monomial-1)
			 (if (zero? monomial-1)
			     (if (pair-list? hs-rev-rest) ;; ex : hs-rev-rest = ( e1 + )
				 (set! h (first hs-rev-rest)) ;; remove the no more usefull operator
				 (set! h (reverse hs-rev-rest))) ;; hs-rev-rest = (e1 e2 ... +)
			     (set! h 
				   (reverse (insert monomial-1 hs-rev-rest)))))
			((is*? monomial-1) ;; monomial-1 = ( c * b^s)
			 (set! h
			       (reverse (insert monomial-1 hs-rev-rest))))
			(else ;; monomial-1 = (+ e1 e2 ....)
			 (set! h
			       (reverse
				(append
				 (reverse (args monomial-1)) ;; lower to higher monomial degrees
				 hs-rev-rest))))))
		 (else ;; e
		  (set! monomial hs)
		  ;;(dv monomial)
		  (display (prefix->infix (expt->^ monomial)))
		  (display " - 1 = ")
		  (set! monomial-1
			(number->hereditary-base-k-expt 
			 (- (eval monomial) 1) ;; substract one
			 b))
		  ;;(dv monomial-1)
		  (display (prefix->infix (expt->^ monomial-1)))
		  (newline)
		  (set! h monomial-1)))
	   ;;(dv h)
	   (newline)) ;; end WHILE
    
    (display "G(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
    (display-nl h)
    (display "P(") (display n-start) (display ")(") (display (- b 1)) (display ")=0")
    
    ))


;; take a monomial c*b^n and substract 1 

;;(hereditary-base-monomial-1 '(* 4 (expt 6 7))) -> 
(define (hereditary-base-monomial-1-simp P) ;; f = hereditary-base-monomial-1
  
  (cond ((number? P)
	 (- P 1))
	((is*? P) ;; c*b^n = (c-1)*b^n + b^n -> (c-1)*b^n + f(b^n)
	 (let* ((c (simplify (arg1 P)))
		(c-1 (simplify (- c 1)))
		;;(hereditary-base-monomial-1 c))) ;; just a hack : we do the -1 by calling again (hereditary-base-monomial-1 but it will return a NUMBER, nothing else ,i could have returned (- (arg1 P) 1)
		(b^n (simplify (arg2 P))))

	   (list (quote +)
		 (list (quote *) c-1 b^n)
		 (hereditary-base-monomial-1-simp b^n))))

	(else ;; b^n = b*b^(n-1) = (b-1)*b^(n-1) + b^(n-1) -> (b-1)*b^(n-1) + f(b^(n-1))
	 (let* ((b (simplify (arg1 P)))
		(n (simplify (arg2 P)))
		(b-1 (simplify (- b 1)))
		(n-1 (simplify (- n 1))))

	   (if (= (- n 1) 1)
	       `(+ (* ,b-1 (expt ,b ,n-1)) ,(hereditary-base-monomial-1-simp b))
	       `(+ (* ,b-1 (expt ,b ,n-1)) ,(hereditary-base-monomial-1-simp `(expt ,b ,n-1))))))))


;; take a monomial c*b^n and substract 1 

;; (prefix->infix (expt->^ (simplify (hereditary-base-monomial-1 '(expt 4 5)))))
;;   ->  '((3 * (4 ^ 4)) + ((3 * (4 ^ 3)) + ((3 * (4 ^ 2)) + ((3 * 4) + 3))))
;;
(define (hereditary-base-monomial-1 P) ;; f = hereditary-base-monomial-1
  
  (cond ((number? P)
	 (- P 1))
	
	((is*? P) ;; c*b^n = (c-1)*b^n + b^n -> (c-1)*b^n + f(b^n)
	 (let* ((c (arg1 P))
		(c-1 (- c 1))
		(b^n (arg2 P)))

	   (list (quote +)
		 (list (quote *) c-1 b^n)
		 (hereditary-base-monomial-1 b^n))))

	(else ;; b^n = b*b^(n-1) = (b-1)*b^(n-1) + b^(n-1) -> (b-1)*b^(n-1) + f(b^(n-1))
	 (let* ((b (arg1 P))
		(n (arg2 P))
		(b-1 (- b 1))
		(n-1 (- n 1)))

	   (if (= (- n 1) 1)
	       `(+ (* ,b-1 (expt ,b ,n-1)) ,(hereditary-base-monomial-1 b))
	       `(+ (* ,b-1 (expt ,b ,n-1)) ,(hereditary-base-monomial-1 `(expt ,b ,n-1))))))))


  
;; (2 * (24 ^ 2)) - 1 = ((24 ^ 2) + (23 * 24) + 23)
