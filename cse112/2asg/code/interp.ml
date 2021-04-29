(* $Id: interp.ml,v 1.18 2021-01-29 11:08:27-08 - - $ *)
(*
Christy Yuen | cyuen7
Mellany Quiroz Almendarez | mquiroza 
*)
open Absyn

let want_dump = ref false

let source_filename = ref ""

let rec eval_expr (expr : Absyn.expr) : float = match expr with
    | Number number -> number
    | Memref memref -> eval_memref memref
    | Unary (oper, expr) -> 
            (   Hashtbl.find Tables.unary_fn_table 
                oper (eval_expr expr)
            ) (*.binary.fntable * *)
    (*eval_STUB "eval_expr Unary"*)
    | Binary (oper, expr1, expr2) ->  
            (   Hashtbl.find Tables.binary_fn_table 
                oper 
                (eval_expr expr1) 
                (eval_expr expr2) 
            )
    (*eval_STUB "eval_expr Binary"*)
and eval_memref (memref : Absyn.memref) : float = match memref with
    | Arrayref (ident, expr) -> Array.get (Hashtbl.find Tables.array_table ident) (Float.to_int (eval_expr expr))
    | Variable ident -> try Hashtbl.find Tables.variable_table ident
                        with Not_found -> 0.0

and eval_STUB reason = (
    print_string ("(" ^ reason ^ ")");
    nan)

(* let rec eval_relexpr (oper, expr1, expr2) (continue : Absyn.program) : float = match oper with
     | Relexpr (oper, expr1, expr2) -> 
            (   Hashtbl.find Tables.bool_fn_table_t
                oper 
                (eval_expr expr1) 
                (eval_expr expr2) 
            ) *)

let rec interpret (program : Absyn.program) = match program with
    | [] -> ()
    | firstline::continue -> match firstline with
       | _, _, None -> interpret continue
       | _, _, Some stmt -> (interp_stmt stmt continue)

and interp_stmt (stmt : Absyn.stmt) (continue : Absyn.program) =
    match stmt with
    | Dim (ident, expr) -> interp_dim (ident, expr) continue
    | Let (memref, expr) -> interp_let (memref, expr) continue
    | Goto label -> interp_goto label continue
    | If (expr, label) -> interp_if (expr, label) continue
    | Print print_list -> interp_print print_list continue
    | Input memref_list -> interp_input memref_list continue

(*Added a LET function below*)
and interp_let (memref, expr) (continue : Absyn.program)  = 
    match memref with 
    | Variable var -> 
        (   Hashtbl.add Tables.variable_table 
            var (eval_expr expr);
            (interpret continue) 
        )
    | Arrayref(name, index) -> 
        try let arr = 
            Hashtbl.find Tables.array_table name 
                in 
                (Array.set arr
                    (Float.to_int (eval_expr index)) (*Hint from TA*) 
                    (eval_expr expr);
                    (interpret continue)
                )
        with Not_found -> Printf.printf "Not_found\n";  
        (* (exit 1); *)
        (* (interpret continue); need find_opt and return none *)
        (*Difference between exit 1 and exit 0, and does it matter?*)
        (* Printf.printf "%s: Not_found\n%!" expr; *)

and interp_dim (ident, expr) 
    (continue : Absyn.program)  = 
    Hashtbl.add Tables.array_table ident
    (Array.make 
        (Float.to_int(eval_expr expr)) 0.0
    ); 
    (* Printf.printf "Dim is working";  *)
    (interpret continue)

(* (interpret continue); *)

and interp_goto (label) (continue : Absyn.program)  = 
    try let gotoLabel = Hashtbl.find Tables.label_table label 
    in interpret gotoLabel; 
    with Not_found -> Printf.printf "Not_found\n";  
    (* (exit 1);  *)

and interp_if (expr, label) (continue : Absyn.program) =
    match expr with 
    | Relexpr (oper, ex1, ex2) ->  
     if (Hashtbl.find Tables.bool_fn_table_t oper (eval_expr ex1) (eval_expr ex2) )
        then interpret (Hashtbl.find Tables.label_table label)
        else interpret continue;  

        (* try let valu = Hashtbl.find Tables.bool_fn_table_t oper in 
        (* I wanted to try catch it because I had a fatal error *)
            if valu (eval_expr ex1) (eval_expr ex2)
            then 
                if (Hashtbl.find Tables.bool_fn_table_t oper) (eval_expr ex1) (eval_expr ex2) 
                then interpret (Hashtbl.find Tables.label_table label)
                else interpret continue; 
        with Not_found -> (exit 1);  *)


     (* if (Hashtbl.find Tables.bool_fn_table_t oper (eval_expr ex1) (eval_expr ex2) )
        then interpret (Hashtbl.find Tables.label_table label)
        else interpret continue;  Working BUT CAN'T PASS TESTS*) 
    
        (* if (Hashtbl.find Tables.bool_fn_table_t oper (eval_expr ex1) (eval_expr ex2) )
        then interpret (Hashtbl.find Tables.label_table label)
        else interpret continue; *)

    (* (eval_expr ex1) (eval_expr expr ex2)  *)
        (* (eval_expr expr) *)
    (* = (interpret continue); *)
    (* need pattern match
    probably same as let need a 
    binary match and an array match   *)



and interp_print (print_list : Absyn.printable list)
                 (continue : Absyn.program) =
    let print_item item = match item with
        | String string ->
          let regex = Str.regexp "\"\\(.*\\)\""
          in print_string (Str.replace_first regex "\\1" string)
        | Printexpr expr ->
          print_string " "; print_float (eval_expr expr)
    in (List.iter print_item print_list; print_newline ());
    interpret continue


and interp_input (memref_list : Absyn.memref list)
                 (continue : Absyn.program)  =
    let input_number memref =
        try  let number = Etc.read_number ()
             (* in (print_float number; print_newline ()) *)
            in match memref with
                (* copied from let *)
                | Variable var ->  
                    (   Hashtbl.add Tables.variable_table var number;
                        (* Printf.printf "%d: number(var)\n%!" (Float.to_int number);  *)
                        (* (interpret continue)  *)
                    )
                | Arrayref(name, index) -> 
                    try let arr = 
                    Hashtbl.find Tables.array_table name 
                    in 
                    (   Array.set arr 
                        (Float.to_int (eval_expr index)) 
                        number;
                        (* Printf.printf "%d: number(array)\n%!" (Float.to_int number);  *)
                        (* (interpret continue) *)
                    )
                    with Not_found -> (exit 0);
                    (* (interpret continue) *)
        with End_of_file -> (*(replace eof "1"); *)
            (
                print_string "End_of_file"; 
                Hashtbl.add Tables.variable_table "eof" 1.; 
                print_newline ()
            )
    in List.iter input_number memref_list;
    interpret continue

(* | Variable var ->  
                    (   let numnum = 1. in Printf.printf "%d: numnum(var)\n%!" (Float.to_int numnum); 
                        Hashtbl.add Tables.variable_table var number;
                        Printf.printf "%d: number(var)\n%!" (Float.to_int number); 
                        Printf.printf "%d: numnum(var)\n%!" (Float.to_int (numnum+1)); 
                        (interpret continue) 
                    ) *)

and interp_STUB reason continue = (
    print_string "Unimplemented: ";
    print_string reason;
    print_newline();
    interpret continue)

let interpret_program program =
    (Tables.init_label_table program; 
     if !want_dump then Tables.dump_label_table ();
     if !want_dump then Dumper.dump_program program;
     interpret program;
     if !want_dump then Tables.dump_label_table ())

