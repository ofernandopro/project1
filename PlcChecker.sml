(* PlcChecker *)

exception EmptySeq
exception UnknownType
exception NotEqTypes
exception WrongRetType
exception DiffBrTypes
exception IfCondNotBool
exception NoMatchResults
exception MatchResTypeDiff
exception MatchCondTypesDiff
exception CallTypeMisM
exception NotFunc
exception ListOutOfRange
exception OpNonList

fun checkEqType (t:plcType):bool = 
    case t of
        BoolT => true
        | IntT ...

fun eval (e:expr) (env: plcType env) : plcType = 
    case e of
        conI _ => IntI
        | Prim1(opr, e1) =>
            let 
                val t1 = teval e1 env
            in
                case (opr, t1) of 
                        ("-", IntT) => IntT
                        | ("!", BoolT) => BoolT
                        | ("ise", SeqT _) => BoolT
                        | ...
                        | _ => raise NoMatchResults
        ...
        end
        | Prim2 ()