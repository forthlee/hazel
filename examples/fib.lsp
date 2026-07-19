; fib.lsp — Fibonacci via lazy infinite stream 
;
; fib = [0, 1, 1, 2, 3, 5, 8, 13, ...]
; Each element is a thunk, computed once on first access.

(load lib)

(defun add (a b) (+ a b))

(define fib (:: 0 (:: 1 (map add fib (cdr fib)))))

(define n 40)

(printf "fib(%d) = %d\n" n (nth n fib))
