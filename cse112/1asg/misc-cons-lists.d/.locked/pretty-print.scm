#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/racket/bin/mzscheme -qr
;; $Id: pretty-print.scm,v 1.1 2020-09-06 22:33:12-07 - - $
;;
;; NAME
;;    prettyprint.scm - pretty print a list
;;
;; DESCRIPTION
;;    For interactive use, (pprint object) will print an object
;;    hierarchally using indentation to show nesting level.
;;
;; BUGS
;;    Does not handle any data structure other than proper lists.
;;    Vectors, hash tables, etc., might be printed ut strangely.
;;    The only type test used is pair?.
;;

(define *stdin* (current-input-port))
(define *stdout* (current-output-port))
(define *stderr* (current-error-port))
(define *arg-list* (vector->list (current-command-line-arguments)))

(define INDENT "    ")
(define LPAREN "(   ")

(define (spaces depth)
   (when (> depth 0)
         (display INDENT)
         (spaces (- depth 1))))

(define (pr-head depth obj)
   (spaces depth)
   (if (not (pair? obj) (printf "~a~n" obj))
       (begin (display LPAREN)
              
