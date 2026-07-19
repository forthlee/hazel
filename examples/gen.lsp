; gen.lsp — Genetic Algorithm
;
; Finds binary string [b0..b4] minimizing (16*b0 + 8*b1 + 4*b2 + 2*b3 + b4)
; Optimal: 00000 = 0

(load lib)

; ---- PRNG (linear congruential generator) ----

(defun rng (seed max)
  (let (next (mod (+ (* seed 1103) 13849) 65536))
    (cons (mod next max) next)))

; ---- Gene operations ----

(defun weighted (() ()) 0)
(defun weighted ((:: x xs) (:: w ws)) (+ (* x w) (weighted xs ws)))

(defun decode (gene) (weighted gene (list 16 8 4 2 1)))

(defun score (gene) (let (v (decode gene)) (- 1 (* v v))))

; ---- Create random population ----

(defun mkGene (seed 0) (cons () seed))
(defun mkGene (seed n)
  (let ((cons bit s1) (rng seed 2))
  (let ((cons rest s2) (mkGene s1 (- n 1)))
    (cons (:: bit rest) s2))))

(defun mkPop (seed 0) (cons () seed))
(defun mkPop (seed n)
  (let ((cons gene s1) (mkGene seed 5))
  (let ((cons rest s2) (mkPop s1 (- n 1)))
    (cons (:: gene rest) s2))))

; ---- Selection: sort by fitness descending ----

(defun sortPop (()) ())
(defun sortPop ((:: x xs)) (ins x (sortPop xs)))

(defun ins (x ()) (list x))
(defun ins (x (:: y ys))
  (if (>= (score x) (score y)) (:: x (:: y ys)) (:: y (ins x ys))))

; ---- Crossover: single-point (splitAt comes from lib.lsp) ----

(defun cross (g1 g2 seed)
  (let ((cons pt s1) (rng seed 4))
  (let (p1 (splitAt (+ pt 1) g1))
  (let (p2 (splitAt (+ pt 1) g2))
    (cons (list (append (car p1) (cdr p2)) (append (car p2) (cdr p1))) s1)))))

; ---- Mutation: flip one random bit ----

(defun flipAt (0 (:: x rest)) (:: (- 1 x) rest))
(defun flipAt (n (:: x rest)) (:: x (flipAt (- n 1) rest)))

(defun mutate (gene seed)
  (let ((cons pt s1) (rng seed 5))
    (cons (flipAt pt gene) s1)))

; ---- One generation step ----
; Keep best, crossover 2 pairs from top-4, mutate 2nd-best

(defun step (pop seed)
  (let (sorted (sortPop pop))
  (let (best (nth 0 sorted))
  (let ((cons c1 s1) (cross (nth 0 sorted) (nth 1 sorted) seed))
  (let ((cons c2 s2) (cross (nth 2 sorted) (nth 3 sorted) s1))
  (let ((cons m s3) (mutate (nth 1 sorted) s2))
    (cons (:: best (append c1 (append c2 (list m)))) s3)))))))

; ---- Display helpers ----

(defun showD (0) ())
(defun showD (n) (append (showD (intdiv n 10)) (list (+ (mod n 10) 48))))

(defun showInt (0) (list 48))
(defun showInt (n) (if (< n 0) (append (list 45) (showD (- 0 n))) (showD n)))

(defun showGene (()) ())
(defun showGene ((:: b bs)) (:: (+ b 48) (showGene bs)))

(defun showPop (()) "")
(defun showPop ((:: g gs))
  (append "  " (append (showGene g) (append " = " (append (showInt (decode g)) (append "\n" (showPop gs)))))))

; ---- Run evolution, build output string ----

(defun evolve (pop seed 0 gen)
  (append "Gen " (append (showInt gen) (append ":\n" (showPop (sortPop pop))))))
(defun evolve (pop seed n gen)
  (let (sorted (sortPop pop))
  (let ((cons newPop newSeed) (step pop seed))
    (append "Gen " (append (showInt gen)
      (append ":\n" (append (showPop sorted)
        (append "\n" (evolve newPop newSeed (- n 1) (+ gen 1))))))))))

(defun go (pop seed) (evolve pop seed 10 0))

; ===== Demo =====

(printf "=== Genetic Algorithm ===\n")
(printf "Target: minimize f = (16*b0 + 8*b1 + 4*b2 + 2*b3 + b4)\n")
(printf "Optimal: 00000 (f=0)\n")
(printf "Population: 6, Gene length: 5\n")

(let ((cons pop0 seed0) (mkPop 42 6))
  (printf "%s\n" (go pop0 seed0)))
