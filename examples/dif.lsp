; dif.lsp — Symbolic Differentiation
;
; Expression representation (tagged flat lists):
;   (list 0 n)          — numeric constant n
;   (list 1 name)       — variable (name is a string)
;   (list 2 op e1 e2)   — binary op, op char code: 43'+' 45'-' 42'*' 47'/' 94'^'
;
; hazel patterns are linear (no repeated-variable equality check),
; so all equality tests use explicit =/streq/expreq calls, exactly
; like the original already did.

(load lib)

; --- Constructors ---
(defun kon (n) (list 0 n))
(defun va (name) (list 1 name))
(defun mk (op a b) (list 2 op a b))

; --- Expression equality ---
(defun expreq ((list 0 a) (list 0 b)) (= a b))
(defun expreq ((list 1 a) (list 1 b)) (streq a b))
(defun expreq ((list 2 op1 a1 b1) (list 2 op2 a2 b2))
  (if (and (= op1 op2) (expreq a1 a2)) (expreq b1 b2) 0))
(defun expreq (_ _) 0)

; --- Differentiation ---
(defun dif1 (dx (list 0 _)) (kon 0))
(defun dif1 (dx (list 1 name)) (if (streq dx name) (kon 1) (kon 0)))
(defun dif1 (dx (list 2 43 e1 e2)) (mk 43 (dif1 dx e1) (dif1 dx e2)))
(defun dif1 (dx (list 2 45 e1 e2)) (mk 45 (dif1 dx e1) (dif1 dx e2)))
(defun dif1 (dx (list 2 42 e1 e2))
  (mk 43 (mk 42 (dif1 dx e1) e2) (mk 42 e1 (dif1 dx e2))))
(defun dif1 (dx (list 2 47 e1 e2))
  (mk 47 (mk 45 (mk 42 e2 (dif1 dx e1)) (mk 42 e1 (dif1 dx e2)))
         (mk 94 e2 (kon 2))))
(defun dif1 (dx (list 2 94 e1 e2))
  (mk 42 (mk 42 e2 (mk 94 e1 (mk 45 e2 (kon 1)))) (dif1 dx e1)))

; --- Simplification rules ---
(defun sim ((list 0 n)) (list 0 n))
(defun sim ((list 1 name)) (list 1 name))
(defun sim ((list 2 op e1 e2)) (sim1 op (cons (sim e1) (sim e2))))

(defun sim1 (43 (cons (list 0 0) x)) x)
(defun sim1 (43 (cons x (list 0 0))) x)
(defun sim1 (43 (cons (list 0 a) (list 0 b))) (kon (+ a b)))
(defun sim1 (45 (cons x (list 0 0))) x)
(defun sim1 (45 (cons (list 0 a) (list 0 b))) (kon (- a b)))
(defun sim1 (42 (cons (list 0 0) _)) (kon 0))
(defun sim1 (42 (cons _ (list 0 0))) (kon 0))
(defun sim1 (42 (cons (list 0 1) x)) x)
(defun sim1 (42 (cons x (list 0 1))) x)
(defun sim1 (42 (cons (list 0 a) (list 0 b))) (kon (* a b)))
(defun sim1 (47 (cons (list 0 0) _)) (kon 0))
(defun sim1 (47 (cons x (list 0 1))) x)

; a/b where a,b are constants: fold if evenly divisible,
; otherwise fall through to the general '/' handling below.
(defun simDivGeneral (x y) (if (expreq x y) (kon 1) (simDiv x (cons y (mk 47 x y)))))
(defun sim1 (47 (cons (list 0 a) (list 0 b)))
  (if (= (mod a b) 0) (kon (/ a b)) (simDivGeneral (list 0 a) (list 0 b))))
(defun sim1 (47 (cons x y)) (simDivGeneral x y))

; --- Division: x / x^n = x^(1-n) ---
(defun simDiv (x (cons (list 2 94 b n) fallback))
  (if (expreq x b) (simPow x (sim1 45 (cons (kon 1) n))) fallback))
(defun simDiv (_ (cons _ fallback)) fallback)

; --- Negative exponent: x^(-n) = 1 / x^n ---
(defun simPow (x (list 0 n))
  (if (< n 0) (mk 47 (kon 1) (sim1 94 (cons x (kon (- 0 n)))))
  (sim1 94 (cons x (kon n)))))
(defun simPow (x e) (sim1 94 (cons x e)))

(defun sim1 (94 (cons _ (list 0 0))) (kon 1))
(defun sim1 (94 (cons x (list 0 1))) x)
(defun sim1 (94 (cons (list 0 0) _)) (kon 0))
(defun sim1 (94 (cons (list 0 1) _)) (kon 1))

; --- Addition commutativity: pull constants together: n+(m+e)=(n+m)+e ---
(defun sim1 (43 (cons (list 0 a) (list 2 43 (list 0 b) x))) (sim1 43 (cons (kon (+ a b)) x)))
(defun sim1 (43 (cons (list 0 a) (list 2 43 x (list 0 b)))) (sim1 43 (cons (kon (+ a b)) x)))
(defun sim1 (43 (cons (list 2 43 (list 0 a) x) (list 0 b))) (sim1 43 (cons (kon (+ a b)) x)))
(defun sim1 (43 (cons (list 2 43 x (list 0 a)) (list 0 b))) (sim1 43 (cons (kon (+ a b)) x)))

; --- Multiplication commutativity: pull constants together: n*(m*e)=(n*m)*e ---
(defun sim1 (42 (cons (list 0 a) (list 2 42 (list 0 b) x))) (sim1 42 (cons (kon (* a b)) x)))
(defun sim1 (42 (cons (list 0 a) (list 2 42 x (list 0 b)))) (sim1 42 (cons (kon (* a b)) x)))
(defun sim1 (42 (cons (list 2 42 (list 0 a) x) (list 0 b))) (sim1 42 (cons (kon (* a b)) x)))
(defun sim1 (42 (cons (list 2 42 x (list 0 a)) (list 0 b))) (sim1 42 (cons (kon (* a b)) x)))

; --- Distributive law: n*(m+e) = n*m + n*e ---
(defun sim1 (42 (cons (list 0 n) (list 2 43 (list 0 m) x)))
  (sim1 43 (cons (kon (* n m)) (sim1 42 (cons (kon n) x)))))
(defun sim1 (42 (cons (list 0 n) (list 2 43 x (list 0 m))))
  (sim1 43 (cons (kon (* n m)) (sim1 42 (cons (kon n) x)))))
(defun sim1 (42 (cons (list 2 43 (list 0 m) x) (list 0 n)))
  (sim1 43 (cons (kon (* n m)) (sim1 42 (cons (kon n) x)))))
(defun sim1 (42 (cons (list 2 43 x (list 0 m)) (list 0 n)))
  (sim1 43 (cons (kon (* n m)) (sim1 42 (cons (kon n) x)))))

(defun sim1 (op (cons a b)) (mk op a b))

; --- Fixed-point simplification ---
(defun fixsim (expr) (fixsim1 expr (sim expr)))
(defun fixsim1 (expr s) (if (expreq expr s) s (fixsim s)))

; --- Display: number to string ---
(defun digitsH (0 acc) acc)
(defun digitsH (n acc) (digitsH (intdiv n 10) (:: (+ (mod n 10) 48) acc)))
(defun showNat (0) "0")
(defun showNat (n) (digitsH n ()))
(defun showNum (n) (if (< n 0) (:: 45 (showNat (- 0 n))) (showNat n)))

; --- Display: expression to infix string ---
(defun showExpr ((list 0 n)) (showNum n))
(defun showExpr ((list 1 name)) name)
(defun showExpr ((list 2 op e1 e2))
  (append "(" (append (showExpr e1) (append " " (append (list op) (append " " (append (showExpr e2) ")")))))))

; --- Main: differentiate, simplify, display ---
(defun dif (dx expr) (showExpr (fixsim (dif1 dx expr))))

; --- Parser: string to expression ---
;   expr  = term (('+' | '-') term)*
;   term  = power (('*' | '/') power)*
;   power = atom ('^' atom)?
;   atom  = number | variable | '(' expr ')'

(defun isdigit (c) (* (>= c 48) (<= c 57)))
(defun isalpha (c) (+ (* (>= c 97) (<= c 122)) (* (>= c 65) (<= c 90))))
(defun digitval (c) (- c 48))

(defun pskip (()) ())
(defun pskip ((:: 32 r)) (pskip r))
(defun pskip ((:: 9 r)) (pskip r))
(defun pskip (r) r)

(defun pnum (acc ()) (cons (kon acc) ()))
(defun pnum (acc (:: c r))
  (if (isdigit c) (pnum (+ (* acc 10) (digitval c)) r) (cons (kon acc) (:: c r))))

(defun pvar (acc ()) (cons (va (reverse acc)) ()))
(defun pvar (acc (:: c r))
  (if (isalpha c) (pvar (:: c acc) r) (cons (va (reverse acc)) (:: c r))))

(defun patom ((:: 40 r))
  (let (res (pexpr (pskip r)))
    (cons (car res) (pskip (cdr (pskip (cdr res)))))))
(defun patom ((:: c r))
  (if (isdigit c) (pnum (digitval c) r) (pvar (:: c ()) r)))

(defun ppow (tokens)
  (let (a (patom tokens)) (ppow1 (car a) (pskip (cdr a)))))
(defun ppow1 (base (:: 94 r))
  (let (e (patom (pskip r))) (cons (mk 94 base (car e)) (cdr e))))
(defun ppow1 (base r) (cons base r))

(defun pmul (tokens)
  (let (f (ppow tokens)) (pmul1 (car f) (pskip (cdr f)))))
(defun pmul1 (acc (:: 42 r))
  (let (f (ppow (pskip r))) (pmul1 (mk 42 acc (car f)) (pskip (cdr f)))))
(defun pmul1 (acc (:: 47 r))
  (let (f (ppow (pskip r))) (pmul1 (mk 47 acc (car f)) (pskip (cdr f)))))
(defun pmul1 (acc r) (cons acc r))

(defun pexpr (tokens)
  (let (t (pmul tokens)) (padd1 (car t) (pskip (cdr t)))))
(defun padd1 (acc (:: 43 r))
  (let (t (pmul (pskip r))) (padd1 (mk 43 acc (car t)) (pskip (cdr t)))))
(defun padd1 (acc (:: 45 r))
  (let (t (pmul (pskip r))) (padd1 (mk 45 acc (car t)) (pskip (cdr t)))))
(defun padd1 (acc r) (cons acc r))

(defun parse (s) (car (pexpr (pskip s))))

; --- Convenience: differentiate from string ---
(defun sdif (dx s) (dif dx (parse s)))

; ===== Tests =====

(printf "=== Symbolic Differentiation ===\n")
(printf "d/dx (x^3) = %s\n" (dif "x" (mk 94 (va "x") (kon 3))))
(printf "d/dx (x^2 + 3x) = %s\n" (dif "x" (mk 43 (mk 94 (va "x") (kon 2)) (mk 42 (kon 3) (va "x")))))
(printf "d/dx (5) = %s\n" (dif "x" (kon 5)))
(printf "d/dx (x) = %s\n" (dif "x" (va "x")))
(printf "d/dx (y) = %s\n" (dif "x" (va "y")))
(printf "d/dx ((1+0)*x^3) = %s\n" (dif "x" (mk 42 (mk 43 (kon 1) (kon 0)) (mk 94 (va "x") (kon 3)))))
(printf "d/dx (x/y) = %s\n" (dif "x" (mk 47 (va "x") (va "y"))))
(printf "d/dx (3x^2+2x+1) = %s\n" (dif "x" (mk 43 (mk 43 (mk 42 (kon 3) (mk 94 (va "x") (kon 2)))
                                     (mk 42 (kon 2) (va "x")))
                              (kon 1))))
(printf "d/dx (x^5) = %s\n" (dif "x" (mk 94 (va "x") (kon 5))))

(printf "=== Simplification Tests ===\n")
(define sim4 (showExpr (fixsim (mk 43 (kon 0) (mk 42 (kon 2) (mk 42 (kon 3) (va "x")))))))
(printf "sim 0+2*(3*x) = %s\n" sim4)

(define sim5 (showExpr (fixsim (mk 43 (kon 4) (mk 43 (kon 3) (mk 42 (kon 5) (mk 42 (kon 3) (va "x"))))))))
(printf "sim 4+(3+5*(3*x)) = %s\n" sim5)

(define sim7 (showExpr (fixsim (mk 42 (kon 3) (mk 43 (kon 4) (mk 42 (kon 5) (va "x")))))))
(printf "sim 3*(4+5*x) = %s\n" sim7)

(define sim8 (showExpr (fixsim (mk 42 (mk 43 (mk 42 (kon 5) (va "x")) (kon 4)) (kon 3)))))
(printf "sim (5*x+4)*3 = %s\n" sim8)

(printf "=== Differentiation Tests ===\n")
(define dif10
  (dif "x" (mk 43 (mk 42 (va "y") (kon 0))
                  (mk 94 (mk 43 (mk 42 (kon 3) (va "x")) (kon 1)) (kon 2)))))
(printf "d/dx (y*0+(3x+1)^2) = %s\n" dif10)

(printf "=== Parser + Differentiation ===\n")
(printf "parse \"x^5\" = %s\n" (showExpr (parse "x^5")))
(printf "parse \"3*x^2+2*x+1\" = %s\n" (showExpr (parse "3*x^2+2*x+1")))
(printf "parse \"(a+b)*(a-b)\" = %s\n" (showExpr (parse "(a+b)*(a-b)")))
(printf "parse \"x/y\" = %s\n" (showExpr (parse "x/y")))

(printf "sdif x \"x^5\" = %s\n" (sdif "x" "x^5"))
(printf "sdif x \"3*x^2+2*x+1\" = %s\n" (sdif "x" "3*x^2+2*x+1"))
(printf "sdif x \"x/y\" = %s\n" (sdif "x" "x/y"))
(printf "sdif x \"(1+0)*x^3\" = %s\n" (sdif "x" "(1+0)*x^3"))
