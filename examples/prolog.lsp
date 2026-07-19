; prolog.lsp — Prolog-style reasoning
;
; Key insight: Prolog's backtracking = list of solutions
;   Success    -> non-empty list
;   Failure    -> ()
;   A, B       -> concatmap B (results of A)   (conjunction)
;   A ; B      -> A <> B                       (disjunction)
;   \+ A       -> null(results of A)           (negation as failure)

(load lib)

; == Fact database (using strings directly) ==
;
;   parent(tom, bob).  parent(tom, liz).
;   parent(bob, ann).  parent(bob, pat).
;   male(tom). male(bob).
;   female(liz). female(ann). female(pat).

(define parent_db (list (cons "tom" "bob") (cons "tom" "liz") (cons "bob" "ann") (cons "bob" "pat")))
(define male_db (list "tom" "bob"))
(define female_db (list "liz" "ann" "pat"))

; print each string in a list on its own line
(defun showEach (()) (printf "\n"))
(defun showEach ((:: x xs)) (begin (printf "%s\n" x) (showEach xs)))

; == Helpers (streq-based) ==

(defun elem (_ ()) 0)
(defun elem (x (:: y ys)) (if (streq x y) 1 (elem x ys)))

(defun unique (()) ())
(defun unique ((:: x xs)) (if (elem x xs) (unique xs) (:: x (unique xs))))

; == Base predicates ==

; children(X) = ?- parent(X, Y).    -> list of Y
(defun children_in (_ ()) ())
(defun children_in (x (:: (cons p c) rest))
  (if (streq p x) (:: c (children_in x rest)) (children_in x rest)))
(defun children (x) (children_in x parent_db))

; parents(Y) = ?- parent(X, Y).     -> list of X
(defun parents_in (_ ()) ())
(defun parents_in (y (:: (cons p c) rest))
  (if (streq c y) (:: p (parents_in y rest)) (parents_in y rest)))
(defun parents (x) (parents_in x parent_db))

(defun male (x) (elem x male_db))
(defun female (x) (elem x female_db))

; == Derived rules ==

; grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
(defun grandchildren (x) (comp c (<- p (children x)) (<- c (children p))))
(defun grandparents (x) (comp p (<- c (parents x)) (<- p (parents c))))

; sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.
(defun siblings (x) (unique (comp c (<- p (parents x)) (<- c (children p)) (not (streq c x)))))

; ancestor(X, Y) :- parent(X, Y).
; ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).
(defun descendants (x) (append (children x) (concatmap descendants (children x))))

; uncle_aunt(U, X) :- parent(P, X), sibling(P, U).
(defun uncles_aunts (x) (unique (comp s (<- p (parents x)) (<- s (siblings p)))))

; brother(X, Y) :- sibling(X, Y), male(X).
(defun brothers (x) (comp s (<- s (siblings x)) (male s)))

; sister(X, Y) :- sibling(X, Y), female(X).
(defun sisters (x) (comp s (<- s (siblings x)) (female s)))

; == Queries ==

(printf "=== Prolog-style Reasoning ===\n")

(printf "Family tree:\n")
(printf "  tom -+- bob -+- ann\n")
(printf "       |       +- pat\n")
(printf "       +- liz\n")

(printf "?- parent(tom, X).\n")
(showEach (children "tom"))
(printf "?- parent(bob, X).\n")
(showEach (children "bob"))
(printf "?- parent(X, ann).\n")
(showEach (parents "ann"))
(printf "?- grandparent(tom, X).\n")
(showEach (grandchildren "tom"))
(printf "?- grandparent(X, ann).\n")
(showEach (grandparents "ann"))
(printf "?- sibling(ann, X).\n")
(showEach (siblings "ann"))
(printf "?- sibling(bob, X).\n")
(showEach (siblings "bob"))
(printf "?- ancestor(tom, X).\n")
(showEach (descendants "tom"))
(printf "?- uncle_aunt(X, ann).\n")
(showEach (uncles_aunts "ann"))
(printf "?- brother(liz, X).\n")
(showEach (brothers "liz"))
(printf "?- sister(bob, X).\n")
(showEach (sisters "bob"))

(printf "Conjunction: parent(tom, X), female(X).\n")
(printf "?- tom's daughters:\n")
(showEach (comp c (<- c (children "tom")) (female c)))

(printf "Disjunction: parent(tom, X) ; parent(bob, X).\n")
(printf "?- tom|bob's children:\n")
(showEach (append (children "tom") (children "bob")))

(printf "Negation as failure: parent(tom, X), not male(X).\n")
(printf "?- tom's non-male kids:\n")
(showEach (comp c (<- c (children "tom")) (not (male c))))
