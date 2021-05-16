#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/ocaml/bin/ocaml
(* $Id: perform.ml,v 1.2 2021-02-17 20:18:07-08 - - $ *)

open Printf;;

let binops  = [(+.); (-.); ( *.); (/.); ( ** )];;
let opnames = ["+."; "-.";  "*."; "/.";  "**" ];;

let data = [2., 3.; 4., 5.];;

let do_data (left, right) =
    let do_op binop opname =
        printf "%.15g %s %.15g = %.15g\n%!"
               left opname right (binop left right)
    in List.iter2 do_op binops opnames
in  List.iter do_data data;;

(*
List.iter (
   fun (left, right) -> 
   List.iter2 (
      fun op name ->
      printf "%.15g %s %.15g = %.15g\n%!"
             left name right (op left right)
   ) binops opnames
) data;;
*)

