#!r6rs

(library (ooff)
  (export ooff->html5)
  (import (rnrs base)
          (expand))

  (define ooff->html5-rules
    (list (expand-rule (n title body)
            '(opus n title body)
            `((!DOCTYPE ((html)))
              (html ((lang . "en"))
                (head ()
                  (meta ((charset . "UTF-8")))
                  (title () ,title))
                (body ()
                  (header ()
                    (nav ()))
                  (main ()
                    (article ()
                      (header ()
                        (h1 () ,title))
                      ,body))
                  (footer ())))))
          (expand-rule (time place body)
            '(© time place body)
            `(section ()
               (h2 () ,time (br ()) ,place)
               ,body))
          (expand-rule (body)
            '(§ . body)
            `(section () . ,body))
          (expand-rule (body)
            '(¶ . body)
            `(p () . ,body))
          (expand-rule (text)
            '(· . text)
            `(! () . ,text))))

  (define (ooff->html5 ooff)
    (expand ooff ooff->html5-rules)))
