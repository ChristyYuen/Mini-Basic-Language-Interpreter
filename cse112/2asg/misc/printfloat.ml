#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/ocaml/bin/ocaml
(* $Id: printfloat.ml,v 1.9 2021-01-25 21:54:54-08 - - $ *)

let print x = (print_float x;
               Printf.printf "  %.16g\n%!" x);;

let pi = acos ~-.1.0;;
let e = exp 1.0;;

let numbers = [
   1.0;
   1.0 /. 3.0;
   111122223333444455556666777788889999.0;
   0.0 /. 0.0;
   nan;
   3.0 /. 0.0;
   pi;
   e;
];;

List.map print numbers;;

