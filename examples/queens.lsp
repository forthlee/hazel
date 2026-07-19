; queens.lsp — N-Queens problem
;
; Usage: (queens 4)   — all solutions for 4-queens
;        (queens 8)   — all 92 solutions for 8-queens
;        (length (queens 8))

(load lib)

; ---- N-Queens solver ----

(defun queens (n) (solve n n))

(defun solve (0 size) (list ()))
(defun solve (m size) (addAll size (solve (- m 1) size)))

; for each partial solution, try extending with columns 1..size
(defun addAll (size ()) ())
(defun addAll (size (:: p ps)) (append (tryCol 1 size p) (addAll size ps)))

(defun tryCol (col size p)
  (if (> col size) ()
  (if (safe p col) (:: (append p (list col)) (tryCol (+ col 1) size p))
  (tryCol (+ col 1) size p))))

; ---- Safety check ----
; Check that placing a queen at column col in the next row
; does not conflict with any existing queen.

(defun safe (p col) (noAttack 1 p (+ (length p) 1) col))

(defun noAttack (row () newRow col) 1)
(defun noAttack (row (:: j rest) newRow col)
  (if (= j col) 0
  (if (= (+ row j) (+ newRow col)) 0
  (if (= (- row j) (- newRow col)) 0
  (noAttack (+ row 1) rest newRow col)))))

; ---- Board display ----

(defun showRow (col n size)
  (if (> col size) "\n"
  (append (if (= col n) "Q " ". ") (showRow (+ col 1) n size))))

(defun showBoard (size ()) "")
(defun showBoard (size (:: q qs)) (append (showRow 1 q size) (showBoard size qs)))

(defun showSolutions (size ()) "")
(defun showSolutions (size (:: s ss))
  (append (showBoard size s) (append "\n" (showSolutions size ss))))

; ===== Demo =====

(printf "=== N-Queens Problem ===\n")

(printf "8-Queens - number of solutions: %d\n\n" (length (queens 8)))

(printf "First 8-Queens solution:\n")
(printf "%s\n" (showBoard 8 (car (queens 8))))
