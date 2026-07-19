; collatz.lsp — Collatz conjecture
; Even -> n/2,  Odd -> 3n+1.  Conjecture: always reaches 1.

(load lib)

(defun next (n) (if (= (mod n 2) 0) (/ n 2) (+ (* 3 n) 1)))

(defun collatz (1) (list 1))
(defun collatz (n) (:: n (collatz (next n))))

(printf "collatz 6:\n")
(collatz 6)
(printf "collatz 27:\n")
(collatz 27)
(printf "length:\n")
(length (collatz 27))

; find the number with the longest sequence in 1..30
(defun longer (a b) (if (> (length a) (length b)) a b))

(defun longest (()) ())
(defun longest ((:: x xs)) (longer (collatz x) (longest xs)))

(printf "longest in 1..30:\n")
(longest (range 1 30))
(printf "length:\n")
(length (longest (range 1 30)))
