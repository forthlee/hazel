; qsort.lsp — Quicksort

(load lib)

(defun qsort (()) ())
(defun qsort ((:: pivot xs))
  (let (greater (comp x (<- x xs) (> x pivot)))
  (let (lesser  (comp x (<- x xs) (<= x pivot)))
    (append (qsort lesser) (:: pivot (qsort greater))))))

(printf "qsort [3,1,4,1,5,9,2,6]:\n")
(qsort (list 3 1 4 1 5 9 2 6))