#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/ocaml/bin/ocaml
(* $Id: nan.ml,v 1.2 2021-01-25 12:13:38-08 - - $ *)

open Printf;;

printf "nan = %.16g\n%!" nan;;
printf "nan = nan = %B\n%!" (nan = nan);;
printf "nan < nan = %B\n%!" (nan < nan);;
printf "nan > nan = %B\n%!" (nan > nan);;
printf "nan = 0.0 = %B\n%!" (nan = 0.0);;
printf "nan < 0.0 = %B\n%!" (nan < 0.0);;
printf "nan > 0.0 = %B\n%!" (nan > 0.0);;

