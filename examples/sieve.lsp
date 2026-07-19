; sieve.lsp — Sieve of Eratosthenes, lazy

(load lib)

(defun remove (p ()) ())
(defun remove (p (:: x xs))
  (if (= (mod x p) 0) (remove p xs) (:: x (remove p xs))))

(defun sieve (()) ())
(defun sieve ((:: p xs)) (:: p (sieve (remove p xs))))

(printf "First 30 primes:\n")
(take 30 (sieve (from 2)))
