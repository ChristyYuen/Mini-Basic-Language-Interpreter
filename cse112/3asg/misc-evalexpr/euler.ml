#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/ocaml/bin/ocaml
(* $Id: euler.ml,v 1.1 2021-04-28 13:52:03-07 - - $ *)

open Printf;;

let ( *: ) = Complex.mul;;
let ( +: ) = Complex.add;;
let cexp = Complex.exp;;
let csqrt = Complex.sqrt;;
let complex_of_float x = {Complex.re = x; Complex.im = 0.0};;
let string_of_complex x =
    "{" ^ string_of_float x.re ^ " + " ^ string_of_float x.im ^ "i}";;

let i = Complex.sqrt (complex_of_float ~-1.0);;
let pi = complex_of_float (acos ~-1.0);;
let euler = Complex.exp (i *: pi) + complex_of_float 1.0;;

printf "i = %s\n" (string_of_complex i);;
printf "pi = %s\n" (string_of_complex pi);;
printf "euler = %s\n" (string_of_complex euler);;

