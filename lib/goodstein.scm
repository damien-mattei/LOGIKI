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
	   (display "G(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
	   (set! h (number->hereditary-base-k-expt n b))
	   ;;(display-nl h)
	   (display-nl (eval h))

	   ;; convertir ,ne pas recalculer
	   (set! hi (prefix->infix h))
	   (set! hi
		 (replace hi 'expt '^)) ;; expt))
	   ;;(set! hi (number->hereditary-base-k-infix n b))
	   ;;(display "G(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
	   ;;(display-nl hi)
	   (set! hi-omega 
		 (replace hi b omega))
	   ;;(display "P(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
	   ;;(display-nl hi-omega)
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
	   (set! hsi (prefix->infix hs))
	   (set! hsi
		 (replace hsi 'expt '^)) ;; expt))
	   (dv hsi)

	   (set! b (+ 1 b))

	   ;;(set! n (- (eval hs) 1)) ;; substract 1
	   
	   (cond ((number? hs) ;; sometimes it's a number , not a list
		  (set! h (- hs 1)))
	       
		 ((is+? hs)
		  (set! hs-rev (reverse hs)) ;; reverse to get the monomial first (low polynomial degrees firsts)
		  ;;(dv hs-rev)
		  (set! monomial (first hs-rev)) ;; get lower degree monomial
		  ;;(dv monomial)
		  (set! hs-rev-rest (rest hs-rev)) ;; keep the rest
		  ;;(dv hs-rev-rest)
		  (set! monomial-1
			(number->hereditary-base-k-expt 
			 (- (eval monomial) 1) ;; substract one
			 b))
		  ;;(dv monomial-1)
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
		  (set! monomial-1
			(number->hereditary-base-k-expt 
			 (- (eval monomial) 1) ;; substract one
			 b))
		  ;;(dv monomial-1)
		  (set! h monomial-1)))
	   ;;(dv h)
	   (newline)) ;; end WHILE
    
    (display "G(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
    (display-nl h)
    (display "P(") (display n-start) (display ")(") (display (- b 1)) (display ")=0")
    
    ))
