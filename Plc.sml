(* Plc interpreter main file *)

fun run (e:expr) : string = 
    let 
        val valEval = eval e[]
        val valTeval = teval e []
    in
        (val2string valEval) ^ " : " ^ (type2string valTeval)
    
        handle SymbolNotFound => "Undefined Symbol detected."
            |  EmptySeq => "Empty sequence as input."
            |  UnknownType => "Unknown Type."
            |  NotEqTypes => "Types not Equal."
            |  WrongRetType => "Wrong Return Type."
            |  DiffBrTypes => "Conditional branches have different type."
            |  IfCondNotBool => "If conditional is not Boolean."
            |  NoMatchResults => "No match results."
            |  MatchResTypeDiff => "Match result with wrong type."
            |  MatchCondTypesDiff => "Match is with different type."
            |  CallTypeMisM => "Function call with wrong Type."
            |  NotFunc => "Not a function."
            |  ListOutOfRange => "Trying to access invalid value from List."
            |  OpNonList => "Expression is not a List."
            |  HDEmptySeq => "Head from empty sequence is empty."
            |  TLEmptySeq => "Tail from empty sequence is empty"
            |  ValueNotFoundInMatch => "Value not found."
            |  NotAFunc => "Value is not a function."
            |  Impossible => "Operation can't be executed."
            |  _ => "Unexpected exeption."
    end








