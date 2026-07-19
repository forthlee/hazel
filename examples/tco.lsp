; tco.lsp — Tail Call Optimisation demo
;
; Without TCO, deep tail recursion overflows the C stack (~8MB, ~10,000 frames).
; With TCO, tail calls become a C goto loop — no stack growth.

(load lib)

; ---- simple counter ----
(defun loop (0 acc) acc)
(defun loop (n acc) (loop (- n 1) (+ acc 1)))
(loop 50000 0)

; ---- tail-recursive sum ----
(defun sumTo (0 acc) acc)
(defun sumTo (n acc) (sumTo (- n 1) (+ acc n)))
(sumTo 10000 0)

; ---- accumulator pattern (reverse) ----
(reverse (range 1 1000))

; ---- foldl: tail-recursive fold ----
(foldl + 0 (range 1 10000))
(foldl * 1 (range 1 10))

; ---- mutual tail recursion ----
(defun isEven (0) 1)
(defun isEven (n) (isOdd (- n 1)))
(defun isOdd (0) 0)
(defun isOdd (n) (isEven (- n 1)))
(isEven 10000)
(isOdd 10001)

; ---- non-tail recursion (for contrast) ----
(defun fact (0) 1)
(defun fact (n) (* n (fact (- n 1))))
(fact 20)
