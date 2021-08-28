(* PlcInterp *)

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

fun eval (e:expr) (env:plcVal env) : plcVal = 
    case e of  
            ConB b => BoolV 
        | Prim1 (opr, e1) =>
            let
                val v1 = eval e1 env
            in
                case (opr, v1) of 
                        ("-", IntV i) => IntV (~i)
                    | ("!", BoolV b) => BoolV (not b)
                | ("ise", SeqV []) => BoolV true
                | ("ise", SeqV _) => BoolV false
                | ...
                | _ => raise Impossible
                end
        | Prim2 (opr, e1, e2) => 