#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/ocaml/bin/ocaml
(* $Id: evalexpr.ml,v 1.1 2021-02-17 20:04:39-08 - - $ *)

open Printf;;

type expr = Number of float
          | Variable of string
          | Binop of string * expr * expr;;

let binops : (string, float -> float -> float) Hashtbl.t
           = Hashtbl.create 16;;
let _ = List.iter
        (fun (var, value) -> Hashtbl.add binops var value)
        ["+", ( +. );
         "-", ( -. );
         "*", ( *. );
         "/", ( /. );
         "^", ( ** )];;

let nan = 0.0 /. 0.0;;

let variables : (string, float) Hashtbl.t = Hashtbl.create 16;;
let _ = List.iter
        (fun (var, value) -> Hashtbl.add variables var value)
        ["pi"  , acos (-1.0);
         "e"   , exp 1.0;
         "i"   , sqrt (-1.0);
         "one" , 1.0;
         "zero", 0.0];;

let rec string_of_expr expr = match expr with
    | Number num -> string_of_float num
    | Variable var -> var
    | Binop (oper, opnd1, opnd2) ->
      "(" ^ oper ^ " " ^ (string_of_expr opnd1)
                 ^ " " ^ (string_of_expr opnd2) ^ ")";;

let findoper oper =
    try  Hashtbl.find binops oper
    with Not_found -> fun _ _ -> nan;;

let rec evalexpr expr = match expr with
    | Number num -> num
    | Variable var -> Hashtbl.find variables var
    | Binop (oper, opnd1, opnd2) ->
          let opfn = findoper oper
          and val1 = evalexpr opnd1
          and val2 = evalexpr opnd2
          in opfn val1 val2

let test expr =
    (printf "\nexpr: %s\n%!" (string_of_expr expr);
     printf "value: %.15g\n%!" (evalexpr expr));;

test (Number 3.0);;
test (Binop ("/", Number 1.0, Number 2.0));;
test (Binop ("+", Binop ("*", Number 3.0, Number 4.0),
                  Binop ("*", Number 5.0, Number 6.0)));

