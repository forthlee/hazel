; test.lsp — usage tests for every function in lib.lsp
; Each line exercises one library function; run with ./hazel examples/test.lsp

(load lib)

(defun double (x) (* x 2))
(defun square (x) (* x x))
(defun succ (x) (+ x 1))

; ---- null ----
(printf "null       %v %v\n" (null ()) (null (list 1)))

; ---- append / length ----
(printf "append     %v\n" (append (list 1 2 3) (list 4 5 6)))
(printf "length     %v\n" (length (list 1 2 3 4 5)))

; ---- map / filter / zip ----
(printf "map        %v\n" (map square (list 1 2 3 4)))
(printf "map2       %v\n" (map + (list 1 2 3) (list 10 20 30)))
(printf "filter     %v\n" (filter even (range 1 10)))
(printf "zip        %v\n" (zip (list 1 2 3) (list 4 5 6)))

; ---- list predicates ----
(printf "any        %v %v\n" (any even (list 1 3 6)) (any even (list 1 3 5)))
(printf "all        %v %v\n" (all even (list 2 4 6)) (all even (list 2 4 5)))

; ---- basic list operations ----
(printf "flatten    %v\n" (flatten (list (list 1 2) (list 3 4) (list 5))))
(printf "replicate  %v\n" (replicate 4 7))
(printf "splitAt    %v\n" (splitAt 2 (list 1 2 3 4 5)))
(printf "last       %v\n" (last (list 1 2 3 4)))
(printf "init       %v\n" (init (list 1 2 3 4)))
(printf "take       %v\n" (take 3 (range 1 100)))
(printf "drop       %v\n" (drop 3 (list 1 2 3 4 5)))
(printf "reverse    %v\n" (reverse (list 1 2 3 4)))
(printf "nth        %v\n" (nth 2 (list 10 20 30 40)))

; ---- higher-order functions ----
(printf "foldr      %v\n" (foldr + 0 (range 1 5)))
(printf "foldl      %v\n" (foldl - 0 (list 1 2 3)))
(printf "zipwith    %v\n" (zipwith + (list 1 2 3) (list 10 20 30)))
(printf "takewhile  %v\n" (takewhile even (list 2 4 6 1 8)))
(printf "dropwhile  %v\n" (dropwhile even (list 2 4 6 1 8)))
(printf "concatmap  %v\n" (concatmap (lambda (x) (list x x)) (list 1 2 3)))
(printf "id         %v\n" (id 42))
(printf "const      %v\n" (const 5 99))
(printf "compose    %v %v\n" (compose square double 3) (compose double square 3))
(printf "flip       %v\n" (flip - 3 10))
(printf "twice      %v\n" (twice double 5))
(printf "ntimes     %v\n" (ntimes 3 double 1))

; ---- numeric utilities ----
(printf "sum        %v\n" (sum (range 1 10)))
(printf "product    %v\n" (product (range 1 5)))
(printf "abs        %v\n" (abs (- 7)))
(printf "max        %v\n" (max 3 9))
(printf "min        %v\n" (min 3 9))
(printf "even       %v %v\n" (even 4) (even 5))
(printf "odd        %v %v\n" (odd 4) (odd 5))
(printf "trunc      %v\n" (trunc 3.9))
(printf "intdiv     %v\n" (intdiv 17 5))
(printf "lerp       %v\n" (lerp 0 10 0.5))

; ---- string utilities (contains/count/indexOf are generic) ----
(printf "contains   %v %v\n" (contains 3 (list 1 2 3)) (contains 9 (list 1 2 3)))
(printf "count      %v\n" (count 1 (list 1 2 1 3 1)))
(printf "indexOf    %v %v\n" (indexOf 3 (list 1 2 3 4)) (indexOf 9 (list 1 2 3)))
(printf "streq      %v %v\n" (streq "abc" "abc") (streq "abc" "abd"))
(printf "startsWith %v %v\n" (startsWith "he" "hello") (startsWith "lo" "hello"))
(printf "endsWith   %v %v\n" (endsWith "lo" "hello") (endsWith "he" "hello"))
(printf "isSubstring %v %v\n" (isSubstring "ll" "hello") (isSubstring "xy" "hello"))
(printf "split      %v\n" (split 0 (list 1 2 0 3 4)))
(printf "join       %v\n" (join (list 0) (list (list 1 2) (list 3 4))))

; ---- infinite sequences ----
(printf "from       %v\n" (take 5 (from 10)))
(printf "repeat     %v\n" (take 4 (repeat 7)))
(printf "iterate    %v\n" (take 5 (iterate double 1)))

; ---- laziness (take on huge ranges never forces the full range) ----
(printf "take-range %v\n" (take 10 (range 1 1000000000)))
(printf "take-map   %v\n" (take 10 (map double (range 1 1000000000))))
(printf "take-map   %v\n" (take 5 (map square (range 1 1000000000))))
(printf "take-map   %v\n" (take 20 (map succ (range 1 1000000000))))

; ---- composition of the above ----
(printf "pipeline   %v\n" (sum (map square (filter odd (range 1 10)))))
