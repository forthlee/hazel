; calc.lsp — string expression calculator
; Usage: (calc "1+2*(3+4)")
;
; Parses an arithmetic expression string and evaluates it.
; Supports: + - * / ( ) unary minus, multi-digit numbers, spaces.
;
; Grammar (recursive descent):
;   expr   = term (('+' | '-') term)*
;   term   = factor (('*' | '/') factor)*
;   factor = number | '(' expr ')' | '-' factor
;
; Char codes used: '0'=48 '9'=57 ' '=32 '\t'=9 '('=40 ')'=41
;                   '+'=43 '-'=45 '*'=42 '/'=47
; car/cdr are already hazel builtins, no need to redefine.
;
; Note: hazel patterns are linear (a repeated variable like the
; original `expect(c, c :: rest)` is NOT checked for equality), so
; the match is written explicitly with a wildcard instead.

(defun isdigit (c) (* (>= c 48) (<= c 57)))
(defun digitval (c) (- c 48))
(defun expect (c (:: _ rest)) rest)

(defun skip (()) ())
(defun skip ((:: 32 rest)) (skip rest))
(defun skip ((:: 9 rest)) (skip rest))
(defun skip (rest) rest)

(defun pnum (acc ()) (cons acc ()))
(defun pnum (acc (:: c rest))
  (if (isdigit c) (pnum (+ (* acc 10) (digitval c)) rest) (cons acc (:: c rest))))

(defun pfactor ((:: 40 rest))
  (let (r (pexpr (skip rest)))
    (cons (car r) (skip (expect 41 (skip (cdr r)))))))
(defun pfactor ((:: 45 rest))
  (let (f (pfactor (skip rest)))
    (cons (- 0 (car f)) (cdr f))))
(defun pfactor ((:: c rest)) (pnum (digitval c) rest))

(defun trest (acc (:: 42 rest))
  (let (f (pfactor (skip rest))) (trest (* acc (car f)) (skip (cdr f)))))
(defun trest (acc (:: 47 rest))
  (let (f (pfactor (skip rest))) (trest (/ acc (car f)) (skip (cdr f)))))
(defun trest (acc rest) (cons acc rest))

(defun pterm (tokens)
  (let (f (pfactor tokens)) (trest (car f) (skip (cdr f)))))

(defun erest (acc (:: 43 rest))
  (let (t (pterm (skip rest))) (erest (+ acc (car t)) (skip (cdr t)))))
(defun erest (acc (:: 45 rest))
  (let (t (pterm (skip rest))) (erest (- acc (car t)) (skip (cdr t)))))
(defun erest (acc rest) (cons acc rest))

(defun pexpr (tokens)
  (let (t (pterm tokens)) (erest (car t) (skip (cdr t)))))

(defun calc (tokens) (car (pexpr (skip tokens))))

; ===== Tests =====

(printf "=== String Expression Calculator ===\n")

(printf "calc \"1+2*(3+4)\" = %v\n" (calc "1+2*(3+4)"))
(printf "calc \"3*4+5\" = %v\n" (calc "3*4+5"))
(printf "calc \"(1+2)*(3+4)\" = %v\n" (calc "(1+2)*(3+4)"))
(printf "calc \"10-3-2\" = %v\n" (calc "10-3-2"))
(printf "calc \"100/10/2\" = %v\n" (calc "100/10/2"))
(printf "calc \"2*3+4*5\" = %v\n" (calc "2*3+4*5"))
(printf "calc \"42\" = %v\n" (calc "42"))
(printf "calc \"((50))\" = %v\n" (calc "((50))"))
(printf "calc \" 1 + 2 \" = %v\n" (calc " 1 + 2 "))
(printf "calc \"123+456\" = %v\n" (calc "123+456"))
(printf "calc \"-5+3\" = %v\n" (calc "-5+3"))
(printf "calc \"-(3+4)*2\" = %v\n" (calc "-(3+4)*2"))
(printf "calc \"10-(6-2)\" = %v\n" (calc "10-(6-2)"))
