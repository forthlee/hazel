; math_edu.lsp — Educational demonstration of implementing math functions
; in pure hazel (port of math_edu.hop). hazel has no builtin
; sin/cos/tan/sqrt/exp/ln, so both a Taylor-series and a lookup-table
; implementation are hand-rolled, exactly as the original file intended

(load lib)

; ===== Square root via Newton's method =====
(defun sqrtIter (x g)
  (if (< (abs (- (* g g) x)) 0.00000000001) g
  (sqrtIter x (/ (+ g (/ x g)) 2))))
(defun sqrtS (x) (if (<= x 0) 0 (sqrtIter x (+ (/ x 2) 0.5))))

(printf "sqrt(2) = %v (expect 1.4142)\n" (sqrtS 2))
(printf "sqrt(9) = %v (expect 3.0)\n" (sqrtS 9))
(printf "sqrt(0) = %v (expect 0)\n" (sqrtS 0))

; ===== Constants =====
(define pi 3.14159265358979)

; ===== Angle normalization: reduce angle (radians) to [0, 2*pi) =====
(defun normalize (x)
  (if (< x 0) (normalize (+ x (* 2 pi)))
  (if (>= x (* 2 pi)) (normalize (- x (* 2 pi))) x)))

; ================================================================
;  Method 1: Taylor Series Approximation
; ================================================================
; sin(x) = x - x^3/3! + x^5/5! - ...   cos(x) = 1 - x^2/2! + x^4/4! - ...
; computed iteratively: each term = prev_term * (-x^2) / (n*(n+1))

(defun taylorStep (acc term x2 n)
  (if (< (abs term) 0.0000000001) acc
  (taylorStep (+ acc term) (/ (- 0 (* term x2)) (* n (+ n 1))) x2 (+ n 2))))

; reduce to [-pi, pi] for better convergence
(defun reduce (x)
  (let (y (normalize x))
    (if (> y pi) (- y (* 2 pi)) y)))

(defun sinS (x) (let (r (reduce x)) (taylorStep 0 r (* r r) 2)))
(defun cosS (x) (let (r (reduce x)) (taylorStep 0 1 (* r r) 1)))
(defun tanS (x) (/ (sinS x) (cosS x)))

; ---- Inverse trig: bisection method ----
(defun asinBisect (v lo hi 0) (/ (+ lo hi) 2))
(defun asinBisect (v lo hi n)
  (let (mid (/ (+ lo hi) 2))
    (if (< (sinS mid) v) (asinBisect v mid hi (- n 1)) (asinBisect v lo mid (- n 1)))))

(defun asinS (v)
  (if (< v 0) (- 0 (asinS (- 0 v)))
  (if (>= v 1) (/ pi 2)
  (asinBisect v 0 (/ pi 2) 50))))
(defun acosS (v) (- (/ pi 2) (asinS v)))
(defun atanS (v) (asinS (/ v (sqrtS (+ 1 (* v v))))))

; ---- Logarithm: ln(x) = k*ln2 + ln(m), x = m*2^k, m in [1,2) ----
; ln(m) via series: ln(m) = 2*(d + d^3/3 + d^5/5 + ...), d=(m-1)/(m+1)
(define ln2 0.69314718055995)

(defun lnReduce (x k)
  (if (< x 1) (lnReduce (* x 2) (- k 1))
  (if (>= x 2) (lnReduce (/ x 2) (+ k 1))
  (cons x k))))

(defun lnSeries (acc dp d2 i)
  (if (< (abs dp) 0.0000000001) (* 2 acc)
  (lnSeries (+ acc (/ dp i)) (* dp d2) d2 (+ i 2))))

(defun lnS (x)
  (let (p (lnReduce x 0))
  (let (d (/ (- (car p) 1) (+ (car p) 1)))
    (+ (* (cdr p) ln2) (lnSeries 0 d (* d d) 1)))))

(defun log10S (x) (* (/ (lnS x) ln2) 0.30102999566398))

; ---- Exponential: exp(x) = 1 + x + x^2/2! + x^3/3! + ... ----
(defun expTaylor (acc term x n)
  (if (< (abs term) 0.0000000001) acc
  (expTaylor (+ acc term) (/ (* term x) n) x (+ n 1))))
(defun expS (x) (expTaylor 0 1 x 1))

(printf "=== Taylor Series Approximation ===\n")
(printf "sin(0)     %v (expect 0.0)\n"     (sinS 0))
(printf "sin(pi/6)  %v (expect 0.5)\n"     (sinS (/ pi 6)))
(printf "sin(pi/4)  %v (expect 0.7071)\n"  (sinS (/ pi 4)))
(printf "sin(pi/3)  %v (expect 0.8660)\n"  (sinS (/ pi 3)))
(printf "sin(pi/2)  %v (expect 1.0)\n"     (sinS (/ pi 2)))
(printf "sin(pi)    %v (expect 0.0)\n"     (sinS pi))
(printf "cos(0)     %v (expect 1.0)\n"     (cosS 0))
(printf "cos(pi/3)  %v (expect 0.5)\n"     (cosS (/ pi 3)))
(printf "cos(pi/2)  %v (expect 0.0)\n"     (cosS (/ pi 2)))
(printf "cos(pi)    %v (expect -1.0)\n"    (cosS pi))
(printf "tan(pi/4)  %v (expect 1.0)\n"     (tanS (/ pi 4)))
(printf "sin(-pi/6) %v (expect -0.5)\n"    (sinS (- 0 (/ pi 6))))
(printf "asin(0.5)  %v (expect 0.5236)\n"  (asinS 0.5))
(printf "asin(1)    %v (expect 1.5708)\n"  (asinS 1))
(printf "asin(-0.5) %v (expect -0.5236)\n" (asinS (- 0 0.5)))
(printf "acos(0.5)  %v (expect 1.0472)\n"  (acosS 0.5))
(printf "acos(0)    %v (expect 1.5708)\n"  (acosS 0))
(printf "atan(1)    %v (expect 0.7854)\n"  (atanS 1))
(printf "atan(0)    %v (expect 0)\n"       (atanS 0))
(printf "atan(-1)   %v (expect -0.7854)\n" (atanS (- 0 1)))
(printf "ln(1)      %v (expect 0)\n"       (lnS 1))
(printf "ln(2)      %v (expect 0.6931)\n"  (lnS 2))
(printf "ln(e)      %v (expect 1.0)\n"     (lnS (expS 1)))
(printf "ln(10)     %v (expect 2.3026)\n"  (lnS 10))
(printf "ln(0.5)    %v (expect -0.6931)\n" (lnS 0.5))
(printf "log10(10)  %v (expect 1.0)\n"     (log10S 10))
(printf "log10(100) %v (expect 2.0)\n"     (log10S 100))
(printf "log10(2)   %v (expect 0.3010)\n"  (log10S 2))
(printf "exp(0)     %v (expect 1.0)\n"     (expS 0))
(printf "exp(1)     %v (expect 2.7183)\n"  (expS 1))
(printf "exp(2)     %v (expect 7.3891)\n"  (expS 2))
(printf "exp(-1)    %v (expect 0.3679)\n"  (expS (- 0 1)))

; ================================================================
;  Method 2: Lookup Table with Linear Interpolation
; ================================================================

; sin values for 0, 5, 10, ..., 90 degrees (every 5 deg, 19 entries)
; split across two (list ...) calls: hazel caps calls at 16 args (MAX_ARGS)
(define sinTable (append
  (list 0.0 0.08715574274766 0.17364817766693 0.25881904510252 0.34202014332567
        0.42261826174070 0.5 0.57357643635105 0.64278760968654 0.70710678118655)
  (list 0.76604444311766 0.81915204428612 0.86602540378444 0.90630778703665
        0.93969262078591 0.96592582628907 0.98480775301221 0.99619469809175 1.0)))

(defun sinLookupQ1 (deg table)
  (let (i (intdiv deg 5))
  (let (f (/ (- deg (* i 5)) 5))
    (lerp (nth i table) (nth (+ i 1) table) f))))

(defun sinQ1 (deg table) (if (>= deg 90) 1.0 (sinLookupQ1 deg table)))

; extend to [0,360) using symmetry:
;   sin(180-d)=sin(d), sin(180+d)=-sin(d), sin(360-d)=-sin(d)
(defun sinByTable (deg table)
  (if (< deg 0) (sinByTable (+ deg 360) table)
  (if (>= deg 360) (sinByTable (- deg 360) table)
  (if (<= deg 90) (sinQ1 deg table)
  (if (<= deg 180) (sinQ1 (- 180 deg) table)
  (if (<= deg 270) (- 0 (sinQ1 (- deg 180) table))
  (- 0 (sinQ1 (- 360 deg) table))))))))

(defun cosByTable (deg table) (sinByTable (- 90 deg) table))
(defun tanByTable (deg table) (/ (sinByTable deg table) (cosByTable deg table)))

; ---- Inverse trig by table: reverse lookup with interpolation ----
(defun asinSearch (i v table)
  (if (null (cdr table)) (* i 5)
  (if (<= v (car table)) (* i 5)
  (if (<= v (car (cdr table)))
      (+ (* i 5) (/ (* 5 (- v (car table))) (- (car (cdr table)) (car table))))
      (asinSearch (+ i 1) v (cdr table))))))

(defun asinT (v)
  (if (< v 0) (- 0 (asinT (- 0 v)))
  (if (>= v 1) 90
  (asinSearch 0 v sinTable))))
(defun acosT (v) (- 90 (asinT v)))
(defun atanT (v) (asinT (/ v (sqrtS (+ 1 (* v v))))))

; ---- Logarithm by table: ln values at 1.0, 1.1, ..., 2.0 (11 entries) ----
(define lnTable (list 0.0
  0.09531017980432 0.18232155679395 0.26236426446749 0.33647223662121
  0.40546510810816 0.47000362924573 0.53062825106217 0.58778666490212
  0.64185388617239 0.69314718055995))

(defun lnLookupI (m i)
  (let (f (/ (- (- m 1) (* i 0.1)) 0.1))
    (if (>= i 10) 0.69314718055995
    (lerp (nth i lnTable) (nth (+ i 1) lnTable) f))))
(defun lnLookup (m)
  (if (>= m 2) 0.69314718055995
  (lnLookupI m (intdiv (* (- m 1) 10) 1))))

(defun lnT (x)
  (let (p (lnReduce x 0))
    (+ (* (cdr p) 0.69314718055995) (lnLookup (car p)))))

(defun log10T (x) (* (/ (lnT x) 0.69314718055995) 0.30102999566398))

; ---- Exponential by table: exp values at 0.0, 0.1, ..., 1.0 (11 entries) ----
(define expTable (list 1.0
  1.10517091807565 1.22140275816017 1.34985880757600 1.49182469727656
  1.64872127070013 1.82211880039051 2.01375270747048 2.22554092670106
  2.45960311115695 2.71828182845905))

(defun expLookupI (f i)
  (let (t (/ (- f (* i 0.1)) 0.1))
    (if (>= i 10) 2.71828182845905
    (lerp (nth i expTable) (nth (+ i 1) expTable) t))))
(defun expLookup (f)
  (if (>= f 1) 2.71828182845905
  (expLookupI f (intdiv (* f 10) 1))))

; e^k by repeated multiplication of exp(1)
(defun powE (0) 1)
(defun powE (n) (if (< n 0) (/ 1 (powE (- 0 n))) (* 2.71828182845905 (powE (- n 1)))))

(defun expT (x)
  (if (< x 0) (/ 1 (expT (- 0 x)))
  (let (k (intdiv x 1))
    (* (powE k) (expLookup (- x k))))))

; convenience wrappers
(defun sinT (deg) (sinByTable deg sinTable))
(defun cosT (deg) (cosByTable deg sinTable))
(defun tanT (deg) (tanByTable deg sinTable))

(printf "=== Lookup Table with Interpolation ===\n")
(printf "(Input in degrees)\n")
(printf "sin(0)   %v (expect 0.0)\n"    (sinT 0))
(printf "sin(30)  %v (expect 0.5)\n"    (sinT 30))
(printf "sin(45)  %v (expect 0.7071)\n" (sinT 45))
(printf "sin(60)  %v (expect 0.8660)\n" (sinT 60))
(printf "sin(90)  %v (expect 1.0)\n"    (sinT 90))
(printf "sin(180) %v (expect 0.0)\n"    (sinT 180))
(printf "sin(270) %v (expect -1.0)\n"   (sinT 270))
(printf "cos(0)   %v (expect 1.0)\n"    (cosT 0))
(printf "cos(60)  %v (expect 0.5)\n"    (cosT 60))
(printf "cos(90)  %v (expect 0.0)\n"    (cosT 90))
(printf "cos(180) %v (expect -1.0)\n"   (cosT 180))
(printf "tan(45)  %v (expect 1.0)\n"    (tanT 45))
(printf "tan(30)  %v (expect 0.5774)\n" (tanT 30))
(printf "sin(15)  %v (expect 0.2588)\n" (sinT 15))
(printf "sin(75)  %v (expect 0.9659)\n" (sinT 75))
(printf "sin(-30) %v (expect -0.5)\n"   (sinT (- 0 30)))
(printf "sin(330) %v (expect -0.5)\n"   (sinT 330))
(printf "asinT(0.5)  deg %v (expect 30)\n"  (asinT 0.5))
(printf "asinT(1)    deg %v (expect 90)\n"  (asinT 1))
(printf "asinT(-0.5) deg %v (expect -30)\n" (asinT (- 0 0.5)))
(printf "acosT(0.5)  deg %v (expect 60)\n"  (acosT 0.5))
(printf "acosT(0)    deg %v (expect 90)\n"  (acosT 0))
(printf "atanT(1)    deg %v (expect 45)\n"  (atanT 1))
(printf "atanT(0)    deg %v (expect 0)\n"   (atanT 0))
(printf "atanT(-1)   deg %v (expect -45)\n" (atanT (- 0 1)))
(printf "lnT(1)      %v (expect 0)\n"       (lnT 1))
(printf "lnT(2)      %v (expect 0.6931)\n"  (lnT 2))
(printf "lnT(10)     %v (expect 2.3026)\n"  (lnT 10))
(printf "lnT(0.5)    %v (expect -0.6931)\n" (lnT 0.5))
(printf "log10T(10)  %v (expect 1.0)\n"     (log10T 10))
(printf "log10T(100) %v (expect 2.0)\n"     (log10T 100))
(printf "expT(0)     %v (expect 1.0)\n"     (expT 0))
(printf "expT(1)     %v (expect 2.7183)\n"  (expT 1))
(printf "expT(2)     %v (expect 7.3891)\n"  (expT 2))
(printf "expT(-1)    %v (expect 0.3679)\n"  (expT (- 0 1)))

(printf "=== Comparison: Taylor vs Table ===\n")
(printf "sin(45deg): Taylor %v, Table %v\n"           (sinS (/ pi 4)) (sinT 45))
(printf "cos(60deg): Taylor %v, Table %v\n"           (cosS (/ pi 3)) (cosT 60))
(printf "tan(30deg): Taylor %v, Table %v\n"           (tanS (/ pi 6)) (tanT 30))
(printf "asin(0.5):  Taylor(rad) %v, Table(deg) %v\n" (asinS 0.5)     (asinT 0.5))
(printf "acos(0.5):  Taylor(rad) %v, Table(deg) %v\n" (acosS 0.5)     (acosT 0.5))
(printf "atan(1):    Taylor(rad) %v, Table(deg) %v\n" (atanS 1)       (atanT 1))
(printf "ln(10):     Taylor %v, Table %v\n"           (lnS 10)        (lnT 10))
(printf "log10(100): Taylor %v, Table %v\n"           (log10S 100)    (log10T 100))
(printf "exp(2):     Taylor %v, Table %v\n"           (expS 2)        (expT 2))
