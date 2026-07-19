; miniKanren.lsp — A miniKanren logic programming system
;
; Implements uKanren: unification, conjunction, disjunction,
; interleaving streams, relation calls with fresh variables.
; Includes appendo and membero relations with demos.
;
; Design notes:
;   Terms and goals use tagged-cons encoding (no algebraic types).
;   Goals are data structures interpreted by evalGoal.
;   Relations are functions: (termArgs, counter) -> (goal, newCounter).
;     Relation VALUES (e.g. `appendo`) are stored inside goal data via
;     gCallRel and invoked later through `(fn termArgs c)` — this works
;     because a bare reference to a top-level `defun` name in hazel is
;     already a first-class callable value (tag V_FUN), so it can be
;     carried inside a cons and applied later, exactly like a closure.
;   Streams are lazy lists of states with suspension markers (0 :: rest)
;     to avoid eager evaluation of infinite branches — hazel's ::
;     is already lazy in its tail, so this "just works".
;   No occurs check (standard uKanren omits it).
;
;   NOTE: 0-arity "constant" definitions from the original (mkNil,
;   gSucceed, gFail) must be `define`d, not `defun`-defined: a bare
;   reference to a 0-arg `defun` would evaluate to the *function value*
;   in hazel rather than calling it, unlike the ML source dialect.

(load lib)

; ============================================================
;  Utility: Number to String
; ============================================================

(defun digitCh (d) (+ d 48))
(defun showNatH (0 acc) acc)
(defun showNatH (n acc) (showNatH (intdiv n 10) (:: (digitCh (mod n 10)) acc)))
(defun showNat (0) (list 48))
(defun showNat (n) (showNatH n ()))
(defun showInt (n) (if (< n 0) (append (list 45) (showNat (- 0 n))) (showNat n)))

; ============================================================
;  Terms
;  Var(id)    = (0, id)      — logic variable
;  Sym(name)  = (1, name)    — symbol (char-code list)
;  Num(n)     = (2, n)       — number
;  Pair(a,b)  = (3, a, b)    — cons / ::
;  Nil        = (4, 0)       — nil / empty list
; ============================================================

(defun mkVar (id) (list 0 id))
(defun mkSym (s) (list 1 s))
(defun mkNum (n) (list 2 n))
(defun mkPair (a b) (list 3 a b))
(define mkNil (list 4 0))

; llist: convert a list of terms into a logic list
(defun llist (()) mkNil)
(defun llist ((:: t ts)) (mkPair t (llist ts)))

; ============================================================
;  Goals (tags 10-15, distinct from term tags 0-4)
;  Eq(t1, t2)         = (10, t1, t2)
;  Conj(g1, g2)       = (11, g1, g2)
;  Disj(g1, g2)       = (12, g1, g2)
;  Succeed            = (13, 0)
;  Fail               = (14, 0)
;  CallRel(fn, args)  = (15, fn, args)
; ============================================================

(defun gEq (t1 t2) (list 10 t1 t2))
(defun gConj (g1 g2) (list 11 g1 g2))
(defun gDisj (g1 g2) (list 12 g1 g2))
(define gSucceed (list 13 0))
(define gFail (list 14 0))
(defun gCallRel (fn args) (list 15 fn args))

; conjAll: conjunction of a list of goals
(defun conjAll (()) gSucceed)
(defun conjAll ((:: g ())) g)
(defun conjAll ((:: g gs)) (gConj g (conjAll gs)))

; conde: disjunction of conjunctions
(defun conde (()) gFail)
(defun conde ((:: gs ())) (conjAll gs))
(defun conde ((:: gs rest)) (gDisj (conjAll gs) (conde rest)))

; ============================================================
;  Substitution and Walking
;  Substitution = association list [(varId, term), ...]
; ============================================================

(defun walk ((list 0 id) s) (walkLookup id (cons s s)))
(defun walk (t _) t)

(defun walkLookup (id (cons () _)) (mkVar id))
(defun walkLookup (id (cons (:: (cons vid val) rest) fullS))
  (if (= id vid) (walk val fullS) (walkLookup id (cons rest fullS))))

; ============================================================
;  Unification
;  Returns [newSubst] on success, [] on failure.
; ============================================================

(defun unify (t1 t2 s) (unifyW (walk t1 s) (cons (walk t2 s) s)))

; both variables
(defun unifyW ((list 0 id1) (cons (list 0 id2) s))
  (if (= id1 id2) (list s) (list (:: (cons id1 (list 0 id2)) s))))
; one variable
(defun unifyW ((list 0 id) (cons t s)) (list (:: (cons id t) s)))
(defun unifyW (t (cons (list 0 id) s)) (list (:: (cons id t) s)))
; symbols
(defun unifyW ((list 1 a) (cons (list 1 b) s)) (if (streq a b) (list s) ()))
; numbers
(defun unifyW ((list 2 a) (cons (list 2 b) s)) (if (= a b) (list s) ()))
; pairs: unify components
(defun unifyW ((list 3 a1 a2) (cons (list 3 b1 b2) s)) (unifyPair (unify a1 b1 s) (cons a2 b2)))
; nils
(defun unifyW ((list 4 _) (cons (list 4 _) s)) (list s))
; mismatch
(defun unifyW (_ (cons _ _)) ())

(defun unifyPair (() _) ())
(defun unifyPair ((:: s2 _) (cons a2 b2)) (unify a2 b2 s2))

; ============================================================
;  Streams and Goal Evaluation
;  State = (substitution, counter); Stream = lazy list of states
; ============================================================

; mplus: interleaving merge of two streams
(defun mplus (() s2) s2)
(defun mplus ((:: 0 ss) s2) (:: 0 (mplus s2 ss)))
(defun mplus ((:: s1 ss) s2) (:: s1 (mplus s2 ss)))

; bind: flatMap a goal over a stream (for conjunction)
(defun bind (() _) ())
(defun bind ((:: 0 ss) g) (:: 0 (bind ss g)))
(defun bind ((:: st sts) g) (mplus (evalGoal g st) (:: 0 (bind sts g))))

; pullAll: strip suspensions from stream (lazily)
(defun pullAll (()) ())
(defun pullAll ((:: 0 s)) (pullAll s))
(defun pullAll ((:: x xs)) (:: x (pullAll xs)))

; evalGoal: goal x state -> stream of states
(defun evalGoal ((list 10 t1 t2) (cons s c)) (eqResult (unify t1 t2 s) c))
(defun evalGoal ((list 11 g1 g2) st) (bind (evalGoal g1 st) g2))
(defun evalGoal ((list 12 g1 g2) st) (mplus (evalGoal g1 st) (:: 0 (evalGoal g2 st))))
(defun evalGoal ((list 13 _) st) (list st))
(defun evalGoal ((list 14 _) _) ())
(defun evalGoal ((list 15 fn termArgs) (cons s c)) (callRelH (fn termArgs c) s))

(defun eqResult (() _) ())
(defun eqResult ((:: s2 _) c) (list (cons s2 c)))

(defun callRelH ((cons g c2) s) (evalGoal g (cons s c2)))

; ============================================================
;  Reification and Display
; ============================================================

(defun walkStar (t s) (walkStarH (walk t s) s))
(defun walkStarH ((list 3 a b) s) (mkPair (walkStar a s) (walkStar b s)))
(defun walkStarH (t _) t)

; isLList: is term a proper logic list (ending in Nil)?
(defun isLList ((list 4 _)) 1)
(defun isLList ((list 3 _ b)) (isLList b))
(defun isLList (_) 0)

; showResult: term -> string (char-code list)
(defun showResult ((list 4 _)) "[]")
(defun showResult ((list 0 id)) (append "_." (showNat id)))
(defun showResult ((list 1 name)) name)
(defun showResult ((list 2 n)) (showInt n))
(defun showResult ((list 3 a b))
  (if (isLList (list 3 a b))
      (append "[" (append (showLItems (list 3 a b)) "]"))
      (append "(" (append (showResult a) (append " . " (append (showResult b) ")"))))))
(defun showResult (_) "??")

(defun showLItems ((list 3 a (list 4 _))) (showResult a))
(defun showLItems ((list 3 a rest)) (append (showResult a) (append ", " (showLItems rest))))

; showResults: list of terms -> formatted string
(defun showResults (()) "  (none)")
(defun showResults ((:: t ())) (append "  " (showResult t)))
(defun showResults ((:: t ts))
  (append "  " (append (showResult t) (append "\n" (showResults ts)))))

; showPairResults: list of (term, term) pairs -> formatted string
(defun showPairResults (()) "  (none)")
(defun showPairResults ((:: (cons a b) ()))
  (append "  (" (append (showResult a) (append ", " (append (showResult b) ")")))))
(defun showPairResults ((:: (cons a b) ts))
  (append "  (" (append (showResult a) (append ", " (append (showResult b) (append ")\n" (showPairResults ts)))))))

; ============================================================
;  Run Interface
; ============================================================

; takeN: take with a 3rd dummy argument.  hazel's evaluator routes
; every 2-argument call through eval_binop, which forces BOTH arguments
; before checking the operator name — so lib's 2-arg (take 0 xs) would
; force xs.  When xs is the rest of an exhausted infinite search (only
; suspension markers, never another answer), that force diverges.
; With 3 arguments the binop path is skipped and (takeN 0 xs _) returns
; () without ever touching the stream.
(defun takeN (0 _ _) ())
(defun takeN (_ () _) ())
(defun takeN (n (:: x xs) d) (:: x (takeN (- n 1) xs d)))

; run(n, nq, goal): evaluate goal, reify var 0, take first n
;   nq = number of query variables (initial counter)
(defun run (n nq goal) (takeN n (reifyStream (pullAll (evalGoal goal (cons () nq)))) 0))

(defun reifyStream (()) ())
(defun reifyStream ((:: (cons s _) rest)) (:: (walkStar (mkVar 0) s) (reifyStream rest)))

; run2: like run but reifies both var 0 and var 1 as a cons
(defun run2 (n nq goal) (takeN n (reifyStream2 (pullAll (evalGoal goal (cons () nq)))) 0))

(defun reifyStream2 (()) ())
(defun reifyStream2 ((:: (cons s _) rest))
  (:: (cons (walkStar (mkVar 0) s) (walkStar (mkVar 1) s)) (reifyStream2 rest)))

; ============================================================
;  Relations
;  A relation: (termArgs, counter) -> (goal, newCounter)
; ============================================================

; appendo(l, s, out): append l and s to get out
(defun appendo ((cons l (cons s out)) c)
  (let (a (mkVar c))
  (let (d (mkVar (+ c 1)))
  (let (res (mkVar (+ c 2)))
    (cons
      (conde (list
        (list (gEq l mkNil) (gEq s out))
        (list (gEq l (mkPair a d))
              (gEq out (mkPair a res))
              (gCallRel appendo (cons d (cons s res))))))
      (+ c 3))))))

; membero(x, l): x is a member of logic list l
(defun membero ((cons x l) c)
  (let (h (mkVar c))
  (let (t (mkVar (+ c 1)))
    (cons
      (gConj (gEq l (mkPair h t))
             (gDisj (gEq x h) (gCallRel membero (cons x t))))
      (+ c 2)))))

; ============================================================
;  Demos
; ============================================================

(printf "=== miniKanren in hazel ===\n")

(printf "--- 1. Simple unification ---\n")
(printf "run* (q) (== q 5):\n")
(printf "%s\n" (showResults (run 10 1 (gEq (mkVar 0) (mkNum 5)))))

(printf "--- 2. Disjunction (conde) ---\n")
(printf "run* (q) (conde [[== q 1], [== q 2], [== q 3]]):\n")
(printf "%s\n" (showResults (run 10 1
  (conde (list (list (gEq (mkVar 0) (mkNum 1)))
               (list (gEq (mkVar 0) (mkNum 2)))
               (list (gEq (mkVar 0) (mkNum 3))))))))

(printf "--- 3. Conjunction ---\n")
(printf "run* (q) (== q 5, == q 5):\n")
(printf "%s\n" (showResults (run 10 1 (gConj (gEq (mkVar 0) (mkNum 5)) (gEq (mkVar 0) (mkNum 5))))))

(printf "run* (q) (== q 5, == q 6) [should fail]:\n")
(printf "%s\n" (showResults (run 10 1 (gConj (gEq (mkVar 0) (mkNum 5)) (gEq (mkVar 0) (mkNum 6))))))

(printf "--- 4. appendo forward ---\n")
(printf "appendo([1,2], [3,4], q):\n")
(printf "%s\n" (showResults (run 10 1
  (gCallRel appendo (cons (llist (list (mkNum 1) (mkNum 2)))
                          (cons (llist (list (mkNum 3) (mkNum 4))) (mkVar 0)))))))

(printf "--- 5. appendo backward ---\n")
(printf "appendo(q, r, [1,2,3]) - all splits:\n")
(printf "%s\n" (showPairResults (run2 10 2
  (gCallRel appendo (cons (mkVar 0) (cons (mkVar 1) (llist (list (mkNum 1) (mkNum 2) (mkNum 3)))))))))

(printf "--- 6. membero ---\n")
(printf "membero(q, [a, b, c]):\n")
(printf "%s\n" (showResults (run 10 1
  (gCallRel membero (cons (mkVar 0) (llist (list (mkSym "a") (mkSym "b") (mkSym "c"))))))))

(printf "--- 7. membero check ---\n")
(printf "membero(b, [a, b, c]) - succeeds?:\n")
(printf "%s\n" (showResults (run 1 1
  (gConj (gEq (mkVar 0) (mkSym "yes"))
         (gCallRel membero (cons (mkSym "b") (llist (list (mkSym "a") (mkSym "b") (mkSym "c")))))))))

(printf "membero(d, [a, b, c]) - succeeds?:\n")
(printf "%s\n" (showResults (run 1 1
  (gConj (gEq (mkVar 0) (mkSym "yes"))
         (gCallRel membero (cons (mkSym "d") (llist (list (mkSym "a") (mkSym "b") (mkSym "c")))))))))

(printf "--- 8. Fresh variables ---\n")
(printf "appendo(q, r, [1,2]) with only q shown:\n")
(printf "%s\n" (showResults (run 10 2
  (gCallRel appendo (cons (mkVar 0) (cons (mkVar 1) (llist (list (mkNum 1) (mkNum 2)))))))))

(printf "--- 9. Nested appendo ---\n")
(printf "appendo(q, [3], r), appendo(r, [4], [1,2,3,4]):\n")
(printf "%s\n" (showPairResults (run2 1 2
  (conjAll (list
    (gCallRel appendo (cons (mkVar 0) (cons (llist (list (mkNum 3))) (mkVar 1))))
    (gCallRel appendo (cons (mkVar 1) (cons (llist (list (mkNum 4)))
      (llist (list (mkNum 1) (mkNum 2) (mkNum 3) (mkNum 4)))))))))))

(printf "Done.\n")
