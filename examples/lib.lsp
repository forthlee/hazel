; lib.lsp — Standard library for hazel

; ---- null (empty-list test) ----
(defun null (()) 1)
(defun null (_) 0)

; ---- append (lazy: :: thunks the tail) / length (tail-recursive) ----
(defun append (() ys) ys)
(defun append ((:: x xs) ys) (:: x (append xs ys)))

(defun lenH (n ()) n)
(defun lenH (n (:: _ xs)) (lenH (+ n 1) xs))
(defun length (xs) (lenH 0 xs))

; ---- map / filter / zip (lazy: :: thunks the tail) ----
(defun map (f ()) ())
(defun map (f (:: x xs)) (:: (f x) (map f xs)))
(defun map (f () _) ())                  ; two-list map, stops at shortest
(defun map (f _ ()) ())
(defun map (f (:: x xs) (:: y ys)) (:: (f x y) (map f xs ys)))

(defun filter (p ()) ())
(defun filter (p (:: x xs))
  (if (p x) (:: x (filter p xs)) (filter p xs)))

(defun zip (() _) ())
(defun zip (_ ()) ())
(defun zip ((:: x xs) (:: y ys)) (:: (cons x y) (zip xs ys)))

; ---- list predicates ----
(defun any (p ()) 0)
(defun any (p (:: x xs)) (if (p x) 1 (any p xs)))

(defun all (p ()) 1)
(defun all (p (:: x xs)) (if (p x) (all p xs) 0))

; ---- basic list operations ----
(defun flatten (()) ())
(defun flatten ((:: xs xss)) (append xs (flatten xss)))

(defun replicate (0 _) ())
(defun replicate (n x) (:: x (replicate (- n 1) x)))

(defun splitAt (0 xs) (cons () xs))
(defun splitAt (n (:: x xs))
  (let (p (splitAt (- n 1) xs))
    (cons (:: x (car p)) (cdr p))))

(defun last ((:: x ())) x)
(defun last ((:: _ xs)) (last xs))

(defun init ((:: x ())) ())
(defun init ((:: x xs)) (:: x (init xs)))

(defun take (0 _) ())
(defun take (_ ()) ())
(defun take (n (:: x xs)) (:: x (take (- n 1) xs)))

(defun drop (0 xs) xs)
(defun drop (_ ()) ())
(defun drop (n (:: _ xs)) (drop (- n 1) xs))

(defun rev (acc ()) acc)
(defun rev (acc (:: x xs)) (rev (:: x acc) xs))
(defun reverse (xs) (rev () xs))

(defun nth (0 (:: x _)) x)
(defun nth (n (:: _ xs)) (nth (- n 1) xs))

; ---- higher-order functions ----
(defun foldr (f z ()) z)
(defun foldr (f z (:: x xs)) (f x (foldr f z xs)))

(defun foldl (f z ()) z)
(defun foldl (f z (:: x xs)) (foldl f (f z x) xs))

(defun zipwith (f () _) ())
(defun zipwith (f _ ()) ())
(defun zipwith (f (:: x xs) (:: y ys)) (:: (f x y) (zipwith f xs ys)))

(defun takewhile (p ()) ())
(defun takewhile (p (:: x xs)) (if (p x) (:: x (takewhile p xs)) ()))

(defun dropwhile (p ()) ())
(defun dropwhile (p (:: x xs)) (if (p x) (dropwhile p xs) (:: x xs)))

; concatmap is lazy and stack-safe: the recursion hides in ::'s thunked
; tail (capp), so million-element inputs use constant C stack.
; comp desugars to concatmap, so this must stay lazy.
(defun concatmap (f ()) ())
(defun concatmap (f (:: x xs)) (capp (f x) f xs))
(defun capp (() f xs) (concatmap f xs))
(defun capp ((:: y ys) f xs) (:: y (capp ys f xs)))

(defun id (x) x)
(defun const (x _) x)
(defun compose (f g x) (f (g x)))
(defun flip (f a b) (f b a))
(defun twice (f x) (f (f x)))

(defun ntimes (0 f x) x)
(defun ntimes (n f x) (ntimes (- n 1) f (f x)))

; ---- numeric utilities ----
(defun sum (xs) (foldr + 0 xs))
(defun product (xs) (foldr * 1 xs))

(defun abs (x) (if (< x 0) (- x) x))
(defun max (a b) (if (> a b) a b))
(defun min (a b) (if (< a b) a b))

(defun even (n) (= (mod n 2) 0))
(defun odd (n) (/= (mod n 2) 0))

; trunc: truncate float to integer using mod's cast-to-long behavior
(defun trunc (x) (mod x 10000000))

; intdiv: integer division for non-negative a, positive b
(defun intdiv (a b) (trunc (/ (- a (mod a b)) b)))

; lerp: linear interpolation
(defun lerp (a b t) (+ a (* t (- b a))))

; ---- string utilities (strings are lists of char codes) ----
(defun contains (c ()) 0)
(defun contains (c (:: x xs)) (if (= c x) 1 (contains c xs)))

(defun count (c ()) 0)
(defun count (c (:: x xs)) (+ (if (= c x) 1 0) (count c xs)))

(defun indexOfH (c i ()) (- 0 1))
(defun indexOfH (c i (:: x xs)) (if (= c x) i (indexOfH c (+ i 1) xs)))
(defun indexOf (c s) (indexOfH c 0 s))

(defun streq (() ()) 1)
(defun streq (() _) 0)
(defun streq (_ ()) 0)
(defun streq ((:: a as) (:: b bs)) (if (= a b) (streq as bs) 0))

(defun startsWith (() _) 1)
(defun startsWith (_ ()) 0)
(defun startsWith ((:: a as) (:: b bs)) (if (= a b) (startsWith as bs) 0))

(defun endsWith (suffix s) (startsWith (reverse suffix) (reverse s)))

(defun isSubstring (needle ()) (if (null needle) 1 0))
(defun isSubstring (needle haystack)
  (if (startsWith needle haystack) 1 (isSubstring needle (cdr haystack))))

(defun splitH (d acc ()) (list (reverse acc)))
(defun splitH (d acc (:: x xs))
  (if (= x d) (:: (reverse acc) (splitH d () xs)) (splitH d (:: x acc) xs)))
(defun split (d s) (splitH d () s))

(defun join (d ()) ())
(defun join (d (:: x ())) x)
(defun join (d (:: x xs)) (append x (append d (join d xs))))

; ---- infinite sequences ----
(defun from (n) (:: n (from (+ n 1))))
(defun repeat (x) (:: x (repeat x)))
(defun iterate (f x) (:: x (iterate f (f x))))