; btree.lsp — Binary tree using lists
;
; Encoding:  empty = ()       node  = (list l v r)

(load lib)

(defun leaf (v) (list () v ()))

(defun sumtree (()) 0)
(defun sumtree ((list l v r)) (+ (+ (sumtree l) v) (sumtree r)))

(defun depth (()) 0)
(defun depth ((list l v r))
  (let (dl (depth l))
  (let (dr (depth r))
    (+ 1 (if (> dl dr) dl dr)))))

(defun inorder (()) ())
(defun inorder ((list l v r)) (append (inorder l) (:: v (inorder r))))

(defun insert (x ()) (leaf x))
(defun insert (x (list l v r))
  (if (< x v) (list (insert x l) v r)
              (list l v (insert x r))))

(defun fromlist (()) ())
(defun fromlist ((:: x xs)) (insert x (fromlist xs)))

(defun sort (xs) (inorder (fromlist xs)))

; === Tests ===

; Test 1: manual tree
;        3
;       / \
;      1   5
(printf "sumtree:\n")
(sumtree (list (leaf 1) 3 (leaf 5)))
(printf "inorder:\n")
(inorder (list (leaf 1) 3 (leaf 5)))
(printf "depth:\n")
(depth (list (leaf 1) 3 (leaf 5)))

; Test 2: build BST via insert
(printf "insert 1..5:\n")
(inorder (insert 4 (insert 2 (insert 5 (insert 1 (insert 3 ()))))))

; Test 3: tree sort
(printf "sort: %v\n" (sort (list 5 3 8 1 9 2 7)))

; Test 4: sum of BST
(printf "sum: %v\n" (sumtree (fromlist (list 10 20 30 40 50))))

; Test 5: depth of skewed tree (all inserts to right)
(printf "depth: %v\n" (depth (fromlist (list 1 2 3 4 5))))
