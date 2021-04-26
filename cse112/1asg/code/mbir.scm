;; Christy Yuen | cyuen7
;; Mellany Quiroz Almendarez | mquiroza 
#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/racket/bin/mzscheme -qr

;; $Id: mbir.scm,v 1.9 2021-01-12 11:57:59-08 - - $
;;
;; NAME
;;    mbir.scm filename.mbir
;;
;; SYNOPSIS
;;    mbir.scm - mini basic interper
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an mbir
;;    program, which is the executed.  Currently it is only printed.
;;

(define *DEBUG* #f)
(define *STDIN* (current-input-port))
(define *STDOUT* (current-output-port))
(define *STDERR* (current-error-port))
(define *ARG-LIST* (vector->list (current-command-line-arguments)))

(define *stmt-table*     (make-hash))
(define *function-table* (make-hash))
(define *var-table*      (make-hash))
(define *array-table*    (make-hash))
(define *label-table*    (make-hash))

;;Added to make code simplier
(define (array-put! key value)
    (hash-set! *array-table* key value)
)

(define (var-put! key value)
    (hash-set! *var-table* key value)
)

;;added from c-evalexpr/evalexpr.scm
(for-each
    (lambda (symfun) 
        (hash-set! *function-table* 
                (car symfun) (cadr symfun))) ;;FUNCTION TABLE
    `(
        (+    ,+)
        (-    ,-)
        (*    ,*)
        (/    ,/)
        (^    ,expt)
        (sqrt ,sqrt)
        (sqr  ,sqr)
        (asin ,asin)
        (acos ,acos)
        (atan ,atan)
        (sin ,sin)
        (cos ,cos)
        (tan ,tan)
        (round ,round)
        (< , <)
        (> , >)
        (= , =)
        (<= , <=)
        (>= , >=)
        (!= , (lambda (x y) (not (equal? x y)) )) ;;inf loop 
        (^           ,expt)
        (i           ,(sqrt -1))
        (div         ,(lambda (x y) (floor (/ x y))))
        (log10       ,(lambda (x) (/ (log x) (log 10.0))))
        (ceil        ,ceiling)
        (exp         ,exp)
        (floor       ,floor)
        (log         ,log)
        (sqrt        ,sqrt)
        (read ,read)
    )
)


(for-each 
    (lambda (var) 
                (hash-set! *var-table* 
                      (car var) (cadr var))) ;;VARIABLE TABLE
   `(
        (e    ,(exp 1.0)) ;;changed .0
        (eof  0.0)
        (nan  ,(/ 0.0 0.0))
        (pi   ,(acos -1.0))
    )
)

(define *RUN-FILE*
    (let-values
        (((dirname basename dir?)
            (split-path (find-system-path 'run-file))))
        (path->string basename)))

(define (die list)
    (for-each (lambda (item) (fprintf *STDERR* "~a " item)) list)
    (fprintf *STDERR* "~n")
    (when (not *DEBUG*) (exit 1)))

(define (dump . args)
    (when *DEBUG*
        (printf "DEBUG:")
        (for-each (lambda (arg) (printf " ~s" arg)) args)
        (printf "~n")))

(define (usage-exit)
    (die `("Usage: " ,*RUN-FILE* " [-d] filename")))

(define (line-number line)
    (car line))

(define (line-label line)
    (let ((tail (cdr line)))
         (and (not (null? tail))
              (symbol? (car tail))
              (car tail))))

(define (line-stmt line)
    (let ((tail (cdr line)))
         (cond ((null? tail) #f)
               ((pair? (car tail)) (car tail))
               ((null? (cdr tail)) #f)
               (else (cadr tail)))))

(define (not-implemented function args . nl)
    (printf "(NOT-IMPLEMENTED: ~s ~s)" function args)
    (when (not (null? nl)) (printf "~n")))

;;  --------------------------------------------------------------
;;added from c-evalexpr/evalexpr.scm

;; error for eval-expr
(define NAN (/ 0.0 0.0))

(define (eval-expr expr)
    (cond ((number? expr) (+ expr 0.0))
          ((symbol? expr) (hash-ref *var-table* expr 0.0))
          ((string? expr) ;;need to support words
            (printf "~a" (car expr))
          )
          ((pair? expr) 
              (let ((func (hash-ref *function-table* (car expr) #f))
                    (opnds (map eval-expr (cdr expr))))
                   (if (not func) NAN
                       (apply func opnds)))
          )
          (else NAN))
) 

;;-- interp-DIM ----
(define (interp-dim args continuation)
    ;(if (equal? ('asub (caar args) ) ) 
      ;(printf "incorrect useage")
      (array-put!
        (make-vector 
            (exact-round (eval-expr (caaddr args) 0.0) )
        )
      )
    ;)
    (interp-program continuation)
)

;;-- interp-let ---
(define (interp-let args continuation)
    (if (symbol? (car args) )
        (hash-set! *var-table* (car args) (eval-expr (cadr args) ) )
        (if (vector? (car args) )
            (vector-set! 
                  (hash-ref *array-table*
                        (car args) (eval-expr (cadr args) ) 
                  ) 
            )  
            (printf "Symbol table; entry is not an array")
        )
    )
    (interp-program continuation)
)

;;-- interp-goto -----
(define (interp-goto args continuation)
    (if (not  (hash-ref *label-table* (car args) ) )
        (printf "Error: args not found")
        (interp-program (hash-ref *label-table* (car args) ))
    )
) 

;;-- interp-IF ----
(define (interp-if args continuation)
    (if (not (eval-expr (car args) ) ) 
        (interp-program continuation)
        (interp-goto (cdr args) continuation)
    )
)

(define (interp-print args continuation)
    (define (print item)
        (if (string? item)
            (printf "~a" item)
            (printf " ~a" (eval-expr item))))
    (for-each print args)
    (printf "~n");
    (interp-program continuation))

;;--- interp-input ----
(define (interp-input args continuation)
    (cond (   (not (null? args) ) 
            (let ((input (read) ) )   
                
                (cond 
                    ( (eof-object? input)    (begin var-put! 'eof 1) )
                    ( (number? input)     (var-put! (car args) input) ) 
                    ( (vector? input) (array-put! (car args) input) )  
                    ( else NAN ) 
                ) 
            )
            (interp-input (cdr args) continuation) 
        )
        (else
            (interp-program continuation)
        )
    )
)

(for-each (lambda (fn) (hash-set! *stmt-table* (car fn) (cadr fn)))
   `(
        (dim   ,interp-dim)
        (let   ,interp-let)
        (goto  ,interp-goto)
        (if    ,interp-if)
        (print ,interp-print)
        (input ,interp-input)
    ))

(define (interp-program program)
    (when (not (null? program))
          (let ((line (line-stmt (car program)))
                (continuation (cdr program)))
               (if line
                   (let ((func (hash-ref *stmt-table* (car line) #f)))
                        (func (cdr line) continuation))
                   (interp-program continuation)))))

;; Code taken from 
;; labelhash.scm
(define (scan-for-labels program)  
    (define (get-label line)
        (and (not (null? line))
             (not (null? (cdr line)))
             (cadr line)))
    (when (not (null? program))
          (let ((label (get-label (car program))))
               (when (symbol? label)
                     (hash-set! *label-table* label program)))
          (scan-for-labels (cdr program))))

(define (readlist filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*RUN-FILE* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (dump-program filename program)
    (define (dump-line line)
        (dump (line-number line) (line-label line) (line-stmt line)))
    (dump *RUN-FILE* *DEBUG* filename)
    (dump program)
    (for-each (lambda (line) (dump-line line)) program))

(define (main arglist)
    (cond ((null? arglist)
                (usage-exit))
          ((string=? (car arglist) "-d")
                (set! *DEBUG* #t)
                (printf "~a: ~s~n" *RUN-FILE* *ARG-LIST*)
                (main (cdr arglist)))
          ((not (null? (cdr  arglist)))
                (usage-exit))
          (else (let* ((mbprogfile (car arglist))
                       (program (readlist mbprogfile)))
                (begin (when *DEBUG* (dump-program mbprogfile program))
                       (scan-for-labels program)
                       (interp-program program))))))

(main *ARG-LIST*)

