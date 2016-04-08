;PAD takes one argument n, representing the nth padovan number to find
(defun PAD (n)
	(cond
		((= n 0) 1)
		((= n 1) 1)
		((= n 2) 1)
		(t (+ (PAD (- n 2)) (PAD (- n 3))))
	)
)
; The PAD function works by first checking if n is one of the 3 base cases (0,1, or 2) if so it returns 1,
; otherwise the function computes the nth number by summing the (n-2) number and the (n-3) number.
; the (n-2) and (n-3) numbers are computed by recursively calling PAD.

(format t "PAD 0 = 1 ~S~%" (= (PAD 0) 1))
(format t "PAD 1 = 1 ~S~%" (= (PAD 1) 1))
(format t "PAD 2 = 1 ~S~%" (= (PAD 2) 1))
(format t "PAD 3 = 2 ~S~%" (= (PAD 3) 2))
(format t "PAD 4 = 2 ~S~%" (= (PAD 4) 2))
(format t "PAD 5 = 3 ~S~%" (= (PAD 5) 3))
(format t "PAD 6 = 4 ~S~%" (= (PAD 6) 4))
(format t "PAD 7 = 5 ~S~%" (= (PAD 7) 5))
(format t "PAD 8 = 7 ~S~%" (= (PAD 8) 7))
(format t "PAD 9 = 9 ~S~%" (= (PAD 9) 9))
(format t "PAD 10 = 12 ~S~%" (= (PAD 10) 12))
(format t "PAD 11 = 16 ~S~%" (= (PAD 11) 16))
(format t "PAD 12 = 21 ~S~%" (= (PAD 12) 21))

;SUMS takes one argument n, representing nth padovan number to find sums up to.
(defun SUMS (n)
	(cond
		((= n 0) 0)
		((= n 1) 0)
		((= n 2) 0)
		(t (+ 1 (+ (SUMS (- n 2)) (SUMS (- n 3)))))
	)
)
; The SUMS function works similar to pad by first checking for the 3 base cases (0,1,2), however it returns 0 for these.
; If n isn't a base case the function computes the number of SUMS by adding 1 to the sum of the number of SUMS for the 
; (n - 2) number and the number of SUMS for the (n-3) number. 
; The number of SUMS for the (n-2) and (n-3) numbers are computed by recursively calling SUMS.

(format t "SUMS 0 = 0 ~S~%" (= (SUMS 0) 0))
(format t "SUMS 1 = 0 ~S~%" (= (SUMS 1) 0))
(format t "SUMS 2 = 0 ~S~%" (= (SUMS 2) 0))
(format t "SUMS 3 = 2 ~S~%" (= (SUMS 3) 1))
(format t "SUMS 4 = 2 ~S~%" (= (SUMS 4) 1))
(format t "SUMS 5 = 3 ~S~%" (= (SUMS 5) 2))
(format t "SUMS 6 = 4 ~S~%" (= (SUMS 6) 3))
(format t "SUMS 7 = 5 ~S~%" (= (SUMS 7) 4))
(format t "SUMS 8 = 7 ~S~%" (= (SUMS 8) 6))
(format t "SUMS 9 = 9 ~S~%" (= (SUMS 9) 8))
(format t "SUMS 10 = 12 ~S~%" (= (SUMS 10) 11))
(format t "SUMS 11 = 16 ~S~%" (= (SUMS 11) 15))
(format t "SUMS 12 = 21 ~S~%" (= (SUMS 12) 20))


;takes one argument tree, representing a list based tree to ANONymize
(defun ANON (tree)
	(cond
		((null tree) nil)
		((not (listp tree)) '?)
		(t (cons (ANON (car tree)) (ANON (cdr tree))))
	)
)
; The ANON function works by decomposing the tree and rebuilding it with ?'s instead of the original value.
; To do this it first checks if the input tree is nil, if so it returns nil.
; Next it checks if the tree is an atom, if so it replaces it with a ?.
; Finally if it is a list it cons' the anonymized car of the tree with the anonymized cdr of the tree.
; Doing this follows the original tree structure down to its leaves and replaces them with ?'s and then builds a new tree back up
; in the same structure of the original tree

(format t "(ANON '42) = '? ~S~%" (equal (ANON '42) '?))
(format t "(ANON 'FOO) = '? ~S~%" (equal (ANON 'FOO) '?))
(format t "(ANON '(((L E) F) T))) = '(((? ?) ?) ?)) ~S~%" (equal (ANON '(((L E) F) T)) '(((? ?) ?) ?)))
(format t "(ANON '(5 FOO 3.1 -0.2)) = '(? ? ? ?) ~S~%" (equal (ANON '(5 FOO 3.1 -0.2)) '(? ? ? ?)))
(format t "(ANON '(1 (FOO 3.1) -0.2)) = '(? (? ?) ?) ~S~%" (equal (ANON '(1 (FOO 3.1) -0.2)) '(? (? ?) ?)))
(format t "(ANON '(((1 2) (FOO 3.1)) (BAR -0.2))) = '(((? ?) (? ?)) (? ?)) ~S~%" (equal (ANON '(((1 2) (FOO 3.1)) (BAR -0.2))) '(((? ?) (? ?)) (? ?))))
(format t "(ANON '(R (I (G (H T))))) = '(? (? (? (? ?)))) ~S~%" (equal (ANON '(R (I (G (H T))))) '(? (? (? (? ?))))))