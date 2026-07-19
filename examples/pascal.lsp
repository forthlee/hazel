; pascal.lsp — Pascal's triangle
; Each row = pairwise sums of previous row padded with 0s.

(load lib)

(defun nextrow (row) (map + (:: 0 row) (append row (list 0))))

(printf "Pascal's triangle (first 8 rows):\n")
(letrec (rows (:: (list 1) (map nextrow rows)))
  (take 8 rows))
