;takes one argument n, representing the nth padovan number to find
(defun pad (n)
	(cond
		((= n 0) 1)
		((= n 1) 1)
		((= n 2) 1)
		(t (+ (pad (- n 2)) (pad (- n 3))))
	)
)
; The pad function works by first checking if n is one of the 3 base cases (0,1, or 2) if so it returns 1,
; otherwise the function computes the nth number by summing the (n-2) number and the (n-3) number.
; the (n-2) and (n-3) numbers are computed by recursively calling pad.

(format t "pad 0 = 1 ~S~%" (= (pad 0) 1))
(format t "pad 1 = 1 ~S~%" (= (pad 1) 1))
(format t "pad 2 = 1 ~S~%" (= (pad 2) 1))
(format t "pad 3 = 2 ~S~%" (= (pad 3) 2))
(format t "pad 4 = 2 ~S~%" (= (pad 4) 2))
(format t "pad 5 = 3 ~S~%" (= (pad 5) 3))
(format t "pad 6 = 4 ~S~%" (= (pad 6) 4))
(format t "pad 7 = 5 ~S~%" (= (pad 7) 5))
(format t "pad 8 = 7 ~S~%" (= (pad 8) 7))
(format t "pad 9 = 9 ~S~%" (= (pad 9) 9))
(format t "pad 10 = 12 ~S~%" (= (pad 10) 12))
(format t "pad 11 = 16 ~S~%" (= (pad 11) 16))
(format t "pad 12 = 21 ~S~%" (= (pad 12) 21))

;takes one argument n, representing nth padovan number to find sums up to.
(defun sums (n)
	(cond
		((= n 0) 0)
		((= n 1) 0)
		((= n 2) 0)
		(t (+ 1 (+ (sums (- n 2)) (sums (- n 3)))))
	)
)
; The sums function works similar to pad by first checking for the 3 base cases (0,1,2), however it returns 0 for these.
; If n isn't a base case the function computes the number of sums by adding 1 to the sum of the number of sums for the 
; (n - 2) number and the number of sums for the (n-3) number. 
; The number of sums for the (n-2) and (n-3) numbers are computed by recursively calling sums.

(format t "sums 0 = 0 ~S~%" (= (sums 0) 0))
(format t "sums 1 = 0 ~S~%" (= (sums 1) 0))
(format t "sums 2 = 0 ~S~%" (= (sums 2) 0))
(format t "sums 3 = 2 ~S~%" (= (sums 3) 1))
(format t "sums 4 = 2 ~S~%" (= (sums 4) 1))
(format t "sums 5 = 3 ~S~%" (= (sums 5) 2))
(format t "sums 6 = 4 ~S~%" (= (sums 6) 3))
(format t "sums 7 = 5 ~S~%" (= (sums 7) 4))
(format t "sums 8 = 7 ~S~%" (= (sums 8) 6))
(format t "sums 9 = 9 ~S~%" (= (sums 9) 8))
(format t "sums 10 = 12 ~S~%" (= (sums 10) 11))
(format t "sums 11 = 16 ~S~%" (= (sums 11) 15))
(format t "sums 12 = 21 ~S~%" (= (sums 12) 20))


;takes one argument tree, representing a list based tree to anonymize
(defun anon (tree)
	(cond
		((null tree) nil)
		((not (listp tree)) '?)
		(t (cons (anon (car tree)) (anon (cdr tree))))
	)
)
; The anon function works by decomposing the tree and rebuilding it with ?'s instead of the original value.
; To do this it first checks if the input tree is nil, if so it returns nil.
; Next it checks if the tree is an atom, if so it replaces it with a ?.
; Finally if it is a list it cons' the anonymized car of the tree with the anonymized cdr of the tree.
; Doing this follows the original tree structure down to its leaves and replaces them with ?'s and then builds a new tree back up
; in the same structure of the original tree

(format t "(anon '42) = '? ~S~%" (equal (anon '42) '?))
(format t "(anon 'FOO) = '? ~S~%" (equal (anon 'FOO) '?))
(format t "(anon '(((L E) F) T))) = '(((? ?) ?) ?)) ~S~%" (equal (anon '(((L E) F) T)) '(((? ?) ?) ?)))
(format t "(anon '(5 FOO 3.1 -0.2)) = '(? ? ? ?) ~S~%" (equal (anon '(5 FOO 3.1 -0.2)) '(? ? ? ?)))
(format t "(anon '(1 (FOO 3.1) -0.2)) = '(? (? ?) ?) ~S~%" (equal (anon '(1 (FOO 3.1) -0.2)) '(? (? ?) ?)))
(format t "(anon '(((1 2) (FOO 3.1)) (BAR -0.2))) = '(((? ?) (? ?)) (? ?)) ~S~%" (equal (anon '(((1 2) (FOO 3.1)) (BAR -0.2))) '(((? ?) (? ?)) (? ?))))
(format t "(anon '(R (I (G (H T))))) = '(? (? (? (? ?)))) ~S~%" (equal (anon '(R (I (G (H T))))) '(? (? (? (? ?))))))