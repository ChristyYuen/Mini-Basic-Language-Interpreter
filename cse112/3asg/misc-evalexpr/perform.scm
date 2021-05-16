#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/racket/bin/mzscheme -qr
;; $Id: perform.scm,v 1.3 2021-02-17 21:17:45-08 - - $

(define binops '(+ - * / expt))

(define data '((2 3) (4 5)))

(for-each 
   (lambda (args)
      (let ((flargs (map (lambda (arg) (+ 0.0 arg)) args)))
           (for-each
              (lambda (op)
                 (let ((result (apply (eval op) flargs)))
                      (printf "~a = ~a~n"
                              (cons op flargs) result)))
           binops)))
   data)

