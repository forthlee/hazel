; permutations.lsp — permutations and combinations

(load lib)

(defun mapCons (x lst) (comp (:: x h) (<- h lst)))

(defun pickEach (nil) nil)
(defun pickEach ((:: h t)) (:: (cons h t) (mapConsPairs h (pickEach t))))

(defun mapConsPairs (h nil) nil)
(defun mapConsPairs (h (:: (cons x rem) t)) (:: (cons x (:: h rem)) (mapConsPairs h t)))

(defun permute (nil) (:: nil nil))
(defun permute (lst) (processChoices (pickEach lst)))

(defun processChoices (nil) nil)
(defun processChoices ((:: (cons x rem) t)) (append (mapCons x (permute rem)) (processChoices t)))

(defun combine (0 _) (:: nil nil))
(defun combine (k nil) nil)
(defun combine (k (:: h t)) (append (mapCons h (combine (- k 1) t)) (combine k t)))

(define lis (list 1 2 3))
(permute lis)
(combine 2 lis)
