#!r6rs

(library (ilist-gen)
  (export ilist-gen)
  (import (rnrs base)
          (gen))

  (gen-rule ilist-scalar
            0 -1 2/3 4.5 6+7i +inf.0 #\c #\d #\space #\space #\space "e" 'x #f)

  (gen-rule ilist-list
            '()
            '()
            (cons (ilist-list) (ilist-list))
            (cons (ilist-vector) (ilist-list))
            (cons (ilist-scalar) (ilist-list))
            (cons (ilist-scalar) (ilist-list)))

  (gen-rule ilist-vector
            (list->vector (ilist-list)))

  (gen-rule ilist-single
            (ilist-scalar)
            (ilist-vector)
            (ilist-vector))

  (gen-rule ilist
            '()
            '()
            (cons (ilist) (ilist))
            (cons (ilist-single) (ilist))
            (cons (ilist-single) (ilist)))

  (gen-start ilist-gen ilist))
