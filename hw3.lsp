#| Some things to note |#

;; Don't implement polynomial time algorithm
;; Think of early stopping conditions
;; If you can't implement PARTITION, implement SIMPART. This returns the numerical difference of final partition -> partial credit


#| Auxiliary function to precompute size of list |#

(defun LISTSUM(lst)
    (cond ((NULL lst) 0)
        (t (+ (car lst) (LISTSUM (cdr lst))))
    )
)

#| Partition function that takes in a list and partitions into two lists with minimum difference of sums, calls auxiliary recursive function |#

(defun PARTITION(lst)
    (recurse 0 (LISTSUM lst) lst nil nil)
)

#| Auxiliary function to compute absolute difference between the two partitions given the sum of one list, and the total sum of the overall list |#

(defun ABSDIFFERENCE(sumTotal sumOne)
    (abs (- (- sumTotal sumOne) sumOne))    ;; sum of elements in list two, is sumTotal - sumOne 
)

#| Recursive function to partitions lists. Goes through all possible ways to partitions the list into two lists, but stops early if a perfect partitioning is already found.
   sumOne is sum of elements in list one, sumTotal is the overall sum of the entire list, lst is the remaining elements to be partitioned, and firstList and secondList are 
   the first and second lists respectively. |#

(defun RECURSE(sumOne sumTotal lst firstList secondList)
    (cond ((NULL lst) (cons (ABSDIFFERENCE sumTotal sumOne) (append (list firstList) (list secondList))))       ;; if there are no more elements to be partitioned, return the partitioning and the difference
        (t (let ((elem (car lst))                                                                               ;; save next element to be partitioned                                                    
                 (rest (cdr lst)))                                                                              ;; save rest of list to be partitioned
                (let* ((first (recurse (+ sumOne elem) sumTotal rest (cons elem firstList) secondList))         ;; recurse on adding the element to the first list 
                       (firstDifference (car first)))                                                           ;; sequentially save best difference found from putting element in first list
                    (cond ((zerop (car first)) first)                                                           ;; if perfect partition found, no need to compute more 
                        (t (let* ((second (recurse sumOne sumTotal rest firstList (cons elem secondList)))      ;; else, find the best difference of puutting element in second list
                                 (secondDifference (car second)))                                               ;; sequentiall save best difference found from putting element in second list 
                                (cond ((< firstDifference secondDifference) first)                              ;; if first difference is better, output first result
                                        (t second)                                                              ;; else, output second
                                )
                            )
                        )
                    )
                ) 
            )
        ) 
    )
)
