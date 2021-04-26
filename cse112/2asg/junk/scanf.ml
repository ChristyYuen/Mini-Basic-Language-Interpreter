#!/bin/env ocaml
(* $Id: scanf.ml,v 1.1 2021-01-25 21:51:08-08 - - $ *)

open Scanf;;
open Printf;;

let id x : float = x;;

let rec read () =
    try let number = scanf " %g" id
        in (printf "number = %g\n%!" number;
            read ())
    with
       | Scan_failure foo ->
        (printf "Scan_failure %s\n%!" foo;
         scanf " " ();
         read ())
       | End_of_file ->
         printf "EOF\n%!";;

read ();;
