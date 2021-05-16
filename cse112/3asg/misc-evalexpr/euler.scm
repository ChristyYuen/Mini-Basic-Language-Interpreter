#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/racket/bin/mzscheme -qr
;; $Id: euler.scm,v 1.1 2021-04-28 13:52:03-07 - - $

(define i (sqrt -1))
(define pi (acos -1))
(define euler (+ (exp (* i pi)) 1))

(printf "i = ~a~n" i)
(printf "pi = ~a~n" pi)
(printf "euler = ~a~n" euler)

