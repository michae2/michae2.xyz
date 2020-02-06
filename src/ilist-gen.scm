#!r6rs

(library (ilist-gen)
  (export ilist-gen)
  (import (rnrs base)
          (gen))

  (gen-grammar ilist-gen

    ([ilist-scalar
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

     [ilist-list
       [2 '()]
       [1 (cons (ilist-list) (ilist-list))]
       [1 (cons (ilist-vector) (ilist-list))]
       [2 (cons (ilist-scalar) (ilist-list))]]

     [ilist-vector
       [1 (list->vector (ilist-list))]]

     [ilist-single
       [1 (ilist-scalar)]
       [2 (ilist-vector)]]

     [ilist
       [2 '()]
       [1 (cons (ilist) (ilist))]
       [2 (cons (ilist-single) (ilist))]])

    (ilist)))
