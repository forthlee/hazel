; hamming.lsp — Hamming numbers
; Numbers whose only prime factors are 2, 3, and 5:
;   1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, ...

(load lib)

(defun double (n) (* 2 n))
(defun triple (n) (* 3 n))
(defun times5 (n) (* 5 n))

(defun merge ((:: x xs) (:: y ys))
  (if (< x y) (:: x (merge xs (:: y ys)))
  (if (> x y) (:: y (merge (:: x xs) ys))
  (:: x (merge xs ys)))))

(printf "First 20 Hamming numbers:\n")
(letrec (h (:: 1 (merge (map double h) (merge (map triple h) (map times5 h)))))
  (take 20 h))
