; dp.lsp — Classic Dynamic Programming examples
; Demonstrates bottom-up DP; hazel has no array type, so
; aget/aset/amake/array are shimmed on top of plain lists
; (O(n) access/update instead of O(1) — same algorithm, slower).

(load lib)

(defun aget (i lst) (nth i lst))
(defun aset (i v lst)
  (if (= i 0) (:: v (cdr lst)) (:: (car lst) (aset (- i 1) v (cdr lst)))))
(defun amake (n v) (replicate n v))
(defun array (lst) lst)

; ============================================================
; 1. Longest Increasing Subsequence (LIS) — O(n^2)
; ============================================================
; dp[i] = length of LIS ending at index i
; dp[i] = 1 + max(dp[j]) for all j < i where a[j] < a[i]

(defun lisMax (a dp i j)
  (if (>= j i) 0
  (if (< (aget j a) (aget i a)) (max (aget j dp) (lisMax a dp i (+ j 1)))
  (lisMax a dp i (+ j 1)))))

(defun buildLIS (a dp i n)
  (if (>= i n) dp
  (buildLIS a (aset i (+ (lisMax a dp i 0) 1) dp) (+ i 1) n)))

(defun lisResult (dp i n best)
  (if (>= i n) best
  (lisResult dp (+ i 1) n (max best (aget i dp)))))

(defun lis (lst)
  (let (a (array lst))
  (let (n (length lst))
  (let (dp (buildLIS a (amake n 0) 0 n))
    (lisResult dp 0 n 0)))))

(printf "=== LIS (Longest Increasing Subsequence) ===\n")
(printf "[10,9,2,5,3,7,101,18]:\n") (lis (list 10 9 2 5 3 7 101 18))
(printf "[0,1,0,3,2,3]:\n") (lis (list 0 1 0 3 2 3))
(printf "[7,7,7,7]:\n") (lis (list 7 7 7 7))

; ============================================================
; 2. Coin Change — minimum coins to make amount
; ============================================================
; dp[0] = 0, dp[i] = min(dp[i-c] + 1) over all coins c <= i

(defun bestCoin (dp i ()) 99999)
(defun bestCoin (dp i (:: c cs))
  (if (<= c i) (min (+ (aget (- i c) dp) 1) (bestCoin dp i cs)) (bestCoin dp i cs)))

(defun buildCoinDP (coins dp i amount)
  (if (> i amount) dp
  (buildCoinDP coins (aset i (bestCoin dp i coins) dp) (+ i 1) amount)))

(defun coinChange (coins amount)
  (aget amount (buildCoinDP coins (aset 0 0 (amake (+ amount 1) 99999)) 1 amount)))

(printf "=== Coin Change (min coins) ===\n")
(printf "coins=[1,5,10,25], amount=63:\n") (coinChange (list 1 5 10 25) 63)
(printf "coins=[1,3,4], amount=6:\n") (coinChange (list 1 3 4) 6)
(printf "coins=[1,5,10], amount=27:\n") (coinChange (list 1 5 10) 27)

; ============================================================
; 3. 0/1 Knapsack — max value within weight limit
; ============================================================
; 1D DP: for each item, scan capacities backward.
; Immutable lists make this naturally correct —
; smaller indices still hold values from before this item.

(defun applyItem (w v dp cap)
  (if (< cap w) dp
  (applyItem w v (aset cap (max (aget cap dp) (+ (aget (- cap w) dp) v)) dp) (- cap 1))))

(defun ksLoop (capW dp ()) dp)
(defun ksLoop (capW dp (:: item items))
  (ksLoop capW (applyItem (car item) (cdr item) dp capW) items))

(defun knapsack (items capW)
  (aget capW (ksLoop capW (amake (+ capW 1) 0) items)))

(printf "=== 0/1 Knapsack ===\n")
(printf "items=[(2,3),(3,4),(4,5),(5,6)], W=8:\n")
(knapsack (list (cons 2 3) (cons 3 4) (cons 4 5) (cons 5 6)) 8)
(printf "items=[(1,1),(3,4),(4,5),(5,7)], W=7:\n")
(knapsack (list (cons 1 1) (cons 3 4) (cons 4 5) (cons 5 7)) 7)

; ============================================================
; 4. Weighted Job Scheduling — max profit, non-overlapping
; ============================================================
; Jobs sorted by end time: [(start, end, profit), ...]
; dp[i] = max profit considering jobs 0..i
; dp[i] = max(dp[i-1], profit[i] + dp[lastCompatible(i)])

(defun jstart ((list s e p)) s)
(defun jend ((list s e p)) e)
(defun jpro ((list s e p)) p)

(defun lastCompat (ea si j)
  (if (< j 0) -1
  (if (<= (aget j ea) si) j
  (lastCompat ea si (- j 1)))))

(defun buildJDP (sa ea wa dp i n)
  (if (>= i n) dp
  (let (lc (lastCompat ea (aget i sa) (- i 1)))
  (let (prev (if (> i 0) (aget (- i 1) dp) 0))
    (buildJDP sa ea wa
      (aset i (max prev (+ (aget i wa) (if (>= lc 0) (aget lc dp) 0))) dp)
      (+ i 1) n)))))

(defun dpMax (dp i n best)
  (if (>= i n) best
  (dpMax dp (+ i 1) n (max best (aget i dp)))))

(defun jobSched (jobs)
  (let (sa (array (map jstart jobs)))
  (let (ea (array (map jend jobs)))
  (let (wa (array (map jpro jobs)))
  (let (n (length jobs))
    (dpMax (buildJDP sa ea wa (amake n 0) 0 n) 0 n 0))))))

(printf "=== Weighted Job Scheduling ===\n")
(printf "[(1,3,50),(2,4,10),(3,5,40),(3,6,70)]:\n")
(jobSched (list (list 1 3 50) (list 2 4 10) (list 3 5 40) (list 3 6 70)))
(printf "[(1,3,5),(2,5,6),(4,6,5),(6,7,4),(5,8,11),(7,9,2)]:\n")
(jobSched (list (list 1 3 5) (list 2 5 6) (list 4 6 5) (list 6 7 4) (list 5 8 11) (list 7 9 2)))
