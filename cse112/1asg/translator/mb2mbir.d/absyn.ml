(* $Id: absyn.ml,v 1.3 2020-09-06 20:13:06-07 - - $ *)

(*
* Abstract syntax definitions for MB->MBIR.
*)

type linenr     = int
and  variable   = string
and  label      = string
and  number     = float
and  oper       = string
and  array      = variable * expr
and  fncall     = variable * expr

and  print      = Printexpr of expr
                | String of string

and  memref     = Array of array
                | Variable of variable

and  expr       = Binop of oper * expr * expr
                | Unop of oper * expr
                | Memref of memref
                | Constant of number
                | Fncall of fncall

and  stmt       = Dim of array
                | Let of memref * expr
                | Goto of label
                | If of expr * label
                | Print of print list
                | Input of memref list

and  program    = (linenr * label option * stmt option) list

