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

;;---------------------------------------------------------------------------
;;added from c-evalexpr/evalexpr.scm
(for-each
    (lambda (symfun) (hash-set! *function-table* (car symfun) (cadr symfun))) ;;FUNCTION TABLE
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
   	;;(complex-abs ,complex-abs)
    )
)


(for-each (lambda (var) (hash-set! *var-table* (car var) (cadr var))) ;;VARIABLE TABLE
   `(
        (e    ,(exp 1.0)) ;;changed .0
        (eof  0.0)
        (nan  ,(/ 0.0 0.0))
        (pi   ,(acos -1.0))
    ))

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
    ;;(printf "~a\n" expr)
    (cond ((number? expr) (+ expr 0.0))
          ((symbol? expr) (hash-ref *var-table* expr 0.0))
          ;;(else (not-implemented 'eval-expr expr))));; ---- OG code
          ;;added from c-evalexpr/evalexpr.scm
          ((string? expr) ;;need to support words
            ;(printf "~a\n" expr) ;;return the string
            ;expr
            ;(printf "hello\n")
            (printf "~a" (car expr))
          )
          ((pair? expr) 
              (let ((func (hash-ref *function-table* (car expr) #f))
                    (opnds (map eval-expr (cdr expr))))
                   (if (not func) NAN
                       (apply func opnds)))
          )
          (else NAN))) 

;;---------------------------------- interp-DIM -------------------------------------------
; The dim statement creates an array given by the variable name and
; inserts it into the array table, replacing any previous array already in
; the array table. The dimension of the array is given by the expression.
; All values in the array are initialized to 0.0 (as a real). The expression is
; rounded to the nearest integer before being used as the bound, which
; must be positive. Since the size of the vector must be an integer, use
; (make-vector (exact-round size) 0.0) to create the array. A subscript i
; must be in the range 0 ≤ i < n, where n is the dimension.
(define (interp-dim args continuation)
    ;;(not-implemented 'interp-dim args 'nl)
    ;;(hash-set! *var-table* (car args) (make-vector (exact-round (eval-expr (cdr args) ) )))
    ;;asub stuff - added if state for asub
    (array-put!
        (make-vector 
            (exact-round (eval-expr (caaddr args) 0.0) )
        )
    )
    
    (interp-program continuation)
)

;;----------------------------------- interp-let ------------------------------------------
; A let statement makes an assignment to a variable. The expression is
; first evaluated. For a Variable, its value is stored into the Symbol table,
; replacing whatever was there previously. For an Arrayref , the store mes-
; sage is sent to the vector representing the array. If the Symbol table
; entry is not an array, an error occurs.
; NOTE: NEED AN ERROR MESSAGE
(define (interp-let args continuation)
    (if (symbol? (car args) )
        (hash-set! *var-table* (car args) (eval-expr (cadr args) ) )
        (if (vector? (car args) )
            (vector-set! (hash-ref *array-table* (car args) (eval-expr (cadr args) ) ) )  
            (printf "Symbol table; entry is not an array")
        )
        ;;(hash-set! *var-table* (car args) (eval-expr (cdr args) ) )
    )
    ; (if (not (symbol? (car args))) ;; error message -- added in 
        
    ;     (continuation)
    ; )
    (interp-program continuation)
)

;;------------------------working version-----------------------------------
; (if (symbol? (car args) )
;         (hash-set! *var-table* (car args) (eval-expr (cadr args) ) )
;         (if (vector? (car args) )
;             (vector-set! (hash-ref *array-table* (car args) (exact-round (eval-expr (cadr args) ) ) (eval-expr (args))) )  
;             (exit 1) 
;         )
;         ;;(hash-set! *var-table* (car args) (eval-expr (cdr args) ) )
;     )
;     (interp-program continuation)

; (exact-round (eval-expr (caddr args))) (eval-expr args) 

;    (cond ((symbol? (car args)) (hash-set! *function-table* (car args) (eval-expr (cdr args))))
; 	((vector? (car args)) (hash-ref *array-table* args (exact-round (eval-expr (caddr args))) (eval-expr args)   ) ) 
;               (else interp-program continuation)
;    ) 
; )

;; OLD IMPLEMENTATION ------------------
    ;;(not-implemented 'interp-let args 'nl)
    ;;(if (symbol? (car args))
    ;;       (hash-ref *function-table* (cdr args)))
   ;;;;;;(cond ((vector? (car args)) (hash-set! *array-table* args (eval-expr(cdr args)))) 
   ;;;;;;	((symbol? (car args)) (hash-set! *function-table* (car args) (cdr args)))
   ;;;;;;           (else interp-program continuation)
	  ;;Might need an error message
	  ;;(exact-round (eval-expr (caddr args))) (eval-expr args)) 
	  ;;^^need that somewhere 
   ;;;;;;) 
;;;;;;)
    ;;(else interp-program continuation) ) 

;;------------------------------ interp-goto -----------------------------------------------
; Control transfers to the statement referred to by the Label. An error
; occurs if the Label is not defined.
;; WORKING LINE: (interp-program (hash-ref *label-table* (car args) ))
(define (interp-goto args continuation)
    (if (not  (hash-ref *label-table* (car args) ) )
        (printf "Error: args not found")
        (interp-program (hash-ref *label-table* (car args) ))
    )
) 
;;------------------------------ interp-goto brainstorm-----------------------------------------------       
        ;;(not-implemented 'interp-goto args 'nl)
        ;;((interp-program scan-for-labels args)) ;;changed from hash-set
        ;(eval-expr (hash-ref *label-table* (car args) )  );;Should work 
        ;;(printf "~a" (hash-ref *label-table* (args) )  )
        ;(printf "~a" args) ;;prints (label)
        ;(printf "~a" (cdr (hash-ref *label-table* (car args) ) ))
        ;(printf "~a\n"  (hash-ref *label-table* (car args) ) )
        ;(printf "~a" continuation)
    ;;(car ( hash-ref *label-table* (car args)  )  ) 
    ; (if (hash-has-key? *label-table* args)
    ;     (interp-program (hash-ref *label-table* args))
    ; )
    ;;(interp-program continuation) ;;-OG CODE
        ;; HINT FROM DISCORD: goto doesn't use cont. 
    ;; (display "Label is undefined" *stderr*) (newline)
    ;; OR (throw (make-error "~a is undefined" 'Label') )

;;-------------------------------- interp-IF ---------------------------------------------
; Relop → ‘=’ | ‘<’ | ‘>’ | ‘!=’ | ‘>=’ | ‘<=’
; The two Expressions are compared according to the given Relop, and if
; the comparison is true, control transfers to the statement, as for the goto
; statement.
;; Checks if the args are really the args 
(define (interp-if args continuation)
    (if (not (eval-expr (car args) ) ) 
        (interp-program continuation)
        (interp-goto (cdr args) continuation)
    )
)
    ;(not-implemented 'interp-if args 'nl)
    ; (printf "~a\n " args)
    ;(printf "~a\n " (cadr args))

    ;(printf "eval ~a\n " (not (eval-expr (car args) ) ))
    ;(hash-ref *function-table* (car args)));;if 2 args (expression) (label) - HINT FROM TA
        ;(printf "Error: args not found")
    ; (if (eval-expr (car args) ) ;(hash-ref *function-table* (car args) )
        ;     (eval-expr (cadr args)) 
        ;     (eval-expr (caddr args))
        ; )
    ;(interp-goto (cdr args) continuation)

; if (expression)
;  -> does smth 
;  -> else 

(define (interp-print args continuation)
    (define (print item)
        (if (string? item)
            (printf "~a" item)
            (printf " ~a" (eval-expr item))))
    (for-each print args)
    (printf "~n");
    (interp-program continuation))

;;--------------------------------- interp-input --------------------------------------------
; Numeric values are read in and assigned to the input variables in
; sequence. Arguments might be elements of an array. For each value
; read into a Memory, the value is inserted into the Symbol table under
; that variable’s key. For arrays, the array must already exist and the sub-
; script not be out of bounds.
; If an invalid value (anything that is not a number?) is read, the value
; returned is nan. If end of file is encountered, the value returned is nan
; and the variable eof is entered into the symbol table with the value 1.
; The value of nan can be computed using the expression (/ 0.0 0.0).
; Counterintuitively, the expression (= nan nan) is false.
;; HINT: NEED A FOR-EACH? maybe
(define (interp-input args continuation)

    
    ;(printf "~a\n" (cdr args))
    ;(printf "Before Cond \n")
    ;(not-implemented 'interp-input args 'nl)
    ;(cond ( (not(null? (cdr args))) (let ((input (read)))
    (cond (   (not (null? args) ) 
            (let ((input (read) ) )   
                
                (cond 
                    ( (eof-object? input)    (begin var-put! 'eof 1) )
                    ( (number? input)     (var-put! (car args) input) )   ;(printf "var:~a"  (car args) )  (printf "value~a: " input))
                    ( (vector? input) (array-put! (car args) input) )  
                    ( else NAN ) ;(printf "Error Message in Input, not a value input \n")
                ) 
            )
            (interp-input (cdr args) continuation) 
        )
        (else
            ;(printf "Else Cond \n")
            (interp-program continuation)
        )
    )
    ;(printf "Outside Cond \n")
)
    ; (if (not (null? args))
    ;     (let ((input (read)))
    ;     (printf "Halo")
    ;         (cond 
    ;                 ((eof-object? input)    (begin var-put! 'eof 1))
    ;                 ( (number? input)     (var-put! (car args) input) )
    ;                 (else (printf "Error Message in Input, not a value input"))
    ;         ) 
    ;     )
    ;     ((null? (cdr args))
    ;         (interp-program continuation)
    ;         (interp-input (cdr args))
    ;     )
    ; ) ;;should it ask for an input? 

    ; (if ((not null?) args)
    ;     (let (input (read)))
    ;             (cond 
    ;                 ((eof-object? input)    (begin var-put! 'eof 1))
    ;                 ( (number? input)     (var-put! (car args) input) )
    ;                 (else (printf "Error Message in Input, not a value input"))
    ;             )       
    ;     ((null? (cdr args))
    ;         (interp-program continuation)
    ;         (interp-input (cdr args))
    ;     )
    ; )

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

;;(define (scan-for-labels program)
;;    (not-implemented 'scan-for-labels '() 'nl))
;; SCAN FOR LABEL FUNCTION BELOW!! ----------------------------------------
;; Code taken from examples/c-evalexpr/labelhash.scm
(define (scan-for-labels program) ;;car -hash key || cutter is actual value 
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
    ;(eval-expr (print))
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

