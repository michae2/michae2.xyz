#!/usr/bin/env scheme-script
#!r6rs

;;; In homage to the ancient text-formatting program RUNOFF and its descendents
;;; runoff, roff, nroff, troff, groff, etc, I present to you ONE-OFF, a static
;;; site generator which shall build my blog.
;;;
;;; ONE-OFF is abbreviated as "ooff", which is also the sound my friends make
;;; when I tell them I am working on a static site generator.

(import (rnrs base)
        (rnrs io ports)
        (ilist)
        (html5))

(define (ooff in out)
  (let* ([tree (get-datum in)]
         [tree (html5->ilist tree)])
    (display-ilist tree out)))

(define (ooff-transcoder)
  (make-transcoder (utf-8-codec)
                   (eol-style lf)
                   (error-handling-mode raise)))

(define (ooff-input-port)
  (transcoded-port (standard-input-port)
                   (ooff-transcoder)))

(define (ooff-output-port)
  (transcoded-port (standard-output-port)
                   (ooff-transcoder)))

(let ([in (ooff-input-port)]
      [out (ooff-output-port)])
  (ooff in out))
