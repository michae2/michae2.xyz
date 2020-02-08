#!r6rs

(library (ilist-gen)
  (export ilist-gen)
  (import (rnrs base)
          (gen))

  (gen-grammar ilist-gen

    ([gen-rules (ilist-scalar)
       [1 0]
       [1 -1]
       [1 2/3]
       [1 4.5]
       [1 6+7i]
       [1 +inf.0]
       [1 #\c]
       [1 #\d]
       [3 #\space]
       [1 "e"]
       [1 'x]
       [1 #f]
       [2 'WRAP]]

     [gen-rules (ilist-list)
       [2 '()]
       [1 (cons (ilist-list) (ilist-list))]
       [1 (cons (ilist-vector) (ilist-list))]
       [2 (cons (ilist-scalar) (ilist-list))]]

     [gen-rules (ilist-vector)
       [1 (list->vector (ilist-list))]]

     [gen-rules (ilist-single)
       [1 (ilist-scalar)]
       [2 (ilist-vector)]]

     [gen-rules (ilist)
       [2 '()]
       [1 (cons (ilist) (ilist))]
       [2 (cons (ilist-single) (ilist))]])

    (ilist)))
