functor PlcParserLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : PlcParser_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\014\000\003\000\013\000\007\000\012\000\013\000\011\000\
\\014\000\010\000\016\000\009\000\023\000\008\000\024\000\007\000\
\\025\000\006\000\000\000\
\\001\000\002\000\023\000\004\000\041\000\006\000\022\000\007\000\021\000\
\\008\000\020\000\009\000\019\000\010\000\018\000\011\000\017\000\
\\012\000\016\000\019\000\015\000\000\000\
\\001\000\002\000\023\000\005\000\051\000\006\000\022\000\007\000\021\000\
\\008\000\020\000\009\000\019\000\010\000\018\000\011\000\017\000\
\\012\000\016\000\019\000\015\000\000\000\
\\001\000\002\000\023\000\006\000\022\000\007\000\021\000\008\000\020\000\
\\009\000\019\000\010\000\018\000\011\000\017\000\012\000\016\000\
\\015\000\040\000\019\000\015\000\000\000\
\\001\000\002\000\023\000\006\000\022\000\007\000\021\000\008\000\020\000\
\\009\000\019\000\010\000\018\000\011\000\017\000\012\000\016\000\
\\017\000\039\000\019\000\015\000\000\000\
\\001\000\002\000\023\000\006\000\022\000\007\000\021\000\008\000\020\000\
\\009\000\019\000\010\000\018\000\011\000\017\000\012\000\016\000\
\\018\000\045\000\019\000\015\000\021\000\044\000\000\000\
\\001\000\002\000\023\000\006\000\022\000\007\000\021\000\008\000\020\000\
\\009\000\019\000\010\000\018\000\011\000\017\000\012\000\016\000\
\\019\000\015\000\026\000\060\000\000\000\
\\001\000\002\000\023\000\006\000\022\000\007\000\021\000\008\000\020\000\
\\009\000\019\000\010\000\018\000\011\000\017\000\012\000\016\000\
\\019\000\052\000\000\000\
\\001\000\003\000\013\000\007\000\012\000\013\000\011\000\014\000\010\000\
\\016\000\009\000\022\000\050\000\023\000\008\000\024\000\007\000\
\\025\000\006\000\000\000\
\\001\000\003\000\013\000\007\000\012\000\013\000\011\000\014\000\010\000\
\\016\000\009\000\023\000\008\000\024\000\007\000\025\000\006\000\000\000\
\\001\000\010\000\042\000\000\000\
\\001\000\018\000\045\000\021\000\044\000\000\000\
\\001\000\020\000\053\000\000\000\
\\001\000\023\000\029\000\000\000\
\\001\000\026\000\000\000\000\000\
\\060\000\002\000\023\000\006\000\022\000\007\000\021\000\008\000\020\000\
\\009\000\019\000\010\000\018\000\011\000\017\000\012\000\016\000\
\\019\000\015\000\000\000\
\\061\000\000\000\
\\062\000\000\000\
\\063\000\000\000\
\\064\000\000\000\
\\065\000\000\000\
\\066\000\000\000\
\\067\000\000\000\
\\068\000\000\000\
\\069\000\000\000\
\\070\000\000\000\
\\071\000\002\000\023\000\006\000\022\000\007\000\021\000\008\000\020\000\
\\009\000\019\000\010\000\018\000\011\000\017\000\012\000\016\000\
\\019\000\015\000\000\000\
\\072\000\000\000\
\\073\000\000\000\
\\074\000\000\000\
\\075\000\000\000\
\\076\000\000\000\
\\077\000\000\000\
\\078\000\000\000\
\\079\000\000\000\
\\080\000\000\000\
\\081\000\000\000\
\\082\000\000\000\
\\083\000\002\000\023\000\006\000\022\000\007\000\021\000\008\000\020\000\
\\009\000\019\000\010\000\018\000\011\000\017\000\012\000\016\000\
\\019\000\015\000\000\000\
\\084\000\000\000\
\"
val actionRowNumbers =
"\000\000\031\000\017\000\015\000\
\\035\000\034\000\032\000\009\000\
\\009\000\009\000\009\000\009\000\
\\013\000\009\000\009\000\009\000\
\\009\000\009\000\009\000\009\000\
\\009\000\009\000\004\000\003\000\
\\028\000\027\000\001\000\010\000\
\\026\000\025\000\024\000\023\000\
\\022\000\021\000\020\000\019\000\
\\029\000\011\000\033\000\009\000\
\\009\000\030\000\008\000\036\000\
\\002\000\007\000\012\000\038\000\
\\039\000\009\000\000\000\009\000\
\\018\000\006\000\016\000\005\000\
\\037\000\014\000"
val gotoT =
"\
\\001\000\057\000\002\000\003\000\003\000\002\000\004\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\022\000\003\000\002\000\004\000\001\000\000\000\
\\002\000\023\000\003\000\002\000\004\000\001\000\000\000\
\\002\000\024\000\003\000\002\000\004\000\001\000\000\000\
\\002\000\025\000\003\000\002\000\004\000\001\000\000\000\
\\002\000\026\000\003\000\002\000\004\000\001\000\000\000\
\\000\000\
\\002\000\028\000\003\000\002\000\004\000\001\000\000\000\
\\002\000\029\000\003\000\002\000\004\000\001\000\000\000\
\\002\000\030\000\003\000\002\000\004\000\001\000\000\000\
\\002\000\031\000\003\000\002\000\004\000\001\000\000\000\
\\002\000\032\000\003\000\002\000\004\000\001\000\000\000\
\\002\000\033\000\003\000\002\000\004\000\001\000\000\000\
\\002\000\034\000\003\000\002\000\004\000\001\000\000\000\
\\002\000\035\000\003\000\002\000\004\000\001\000\000\000\
\\002\000\036\000\003\000\002\000\004\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\041\000\000\000\
\\000\000\
\\002\000\044\000\003\000\002\000\004\000\001\000\000\000\
\\002\000\045\000\003\000\002\000\004\000\001\000\000\000\
\\000\000\
\\002\000\047\000\003\000\002\000\004\000\001\000\006\000\046\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\052\000\003\000\002\000\004\000\001\000\000\000\
\\001\000\054\000\002\000\053\000\003\000\002\000\004\000\001\000\000\000\
\\002\000\055\000\003\000\002\000\004\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\056\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 58
val numrules = 25
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | CBOOL of unit ->  (bool) | CINT of unit ->  (int)
 | NAME of unit ->  (string) | CondExpr of unit ->  (expr option)
 | MatchExpr of unit ->  ( ( expr option * expr )  list)
 | Const of unit ->  (expr) | AtomExpr of unit ->  (expr)
 | Expr of unit ->  (expr) | Prog of unit ->  (expr)
end
type svalue = MlyValue.svalue
type result = expr
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 25) => true | _ => false
val showTerminal =
fn (T 0) => "VAR"
  | (T 1) => "AND"
  | (T 2) => "IF"
  | (T 3) => "THEN"
  | (T 4) => "ELSE"
  | (T 5) => "PLUS"
  | (T 6) => "MINUS"
  | (T 7) => "MULT"
  | (T 8) => "DIV"
  | (T 9) => "EQ"
  | (T 10) => "LESS"
  | (T 11) => "LESSEQUAL"
  | (T 12) => "NEGATION"
  | (T 13) => "LPAR"
  | (T 14) => "RPAR"
  | (T 15) => "MATCH"
  | (T 16) => "WITH"
  | (T 17) => "END"
  | (T 18) => "SEMIC"
  | (T 19) => "ARROW"
  | (T 20) => "PIPE"
  | (T 21) => "UNDERLINE"
  | (T 22) => "NAME"
  | (T 23) => "CINT"
  | (T 24) => "CBOOL"
  | (T 25) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 25) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16)
 $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9)
 $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 
1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.Expr Expr1, Expr1left, Expr1right)) :: 
rest671)) => let val  result = MlyValue.Prog (fn _ => let val  (Expr
 as Expr1) = Expr1 ()
 in (Expr)
end)
 in ( LrTable.NT 0, ( result, Expr1left, Expr1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.Prog Prog1, _, Prog1right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, _, _)) :: _ :: ( _, ( MlyValue.NAME NAME1, _, _))
 :: ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = 
MlyValue.Prog (fn _ => let val  (NAME as NAME1) = NAME1 ()
 val  (Expr as Expr1) = Expr1 ()
 val  (Prog as Prog1) = Prog1 ()
 in (Let(NAME, Expr, Prog))
end)
 in ( LrTable.NT 0, ( result, VAR1left, Prog1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.AtomExpr AtomExpr1, AtomExpr1left, 
AtomExpr1right)) :: rest671)) => let val  result = MlyValue.Expr (fn _
 => let val  (AtomExpr as AtomExpr1) = AtomExpr1 ()
 in (AtomExpr)
end)
 in ( LrTable.NT 1, ( result, AtomExpr1left, AtomExpr1right), rest671)

end
|  ( 3, ( ( _, ( MlyValue.Expr Expr3, _, Expr3right)) :: _ :: ( _, ( 
MlyValue.Expr Expr2, _, _)) :: _ :: ( _, ( MlyValue.Expr Expr1, _, _))
 :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 val  Expr3 = Expr3 ()
 in (If(Expr1, Expr2, Expr3))
end)
 in ( LrTable.NT 1, ( result, IF1left, Expr3right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("+", Expr1, Expr2))
end)
 in ( LrTable.NT 1, ( result, Expr1left, Expr2right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("-", Expr1, Expr2))
end)
 in ( LrTable.NT 1, ( result, Expr1left, Expr2right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("*", Expr1, Expr2))
end)
 in ( LrTable.NT 1, ( result, Expr1left, Expr2right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("/", Expr1, Expr2))
end)
 in ( LrTable.NT 1, ( result, Expr1left, Expr2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("=", Expr1, Expr2))
end)
 in ( LrTable.NT 1, ( result, Expr1left, Expr2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("<", Expr1, Expr2))
end)
 in ( LrTable.NT 1, ( result, Expr1left, Expr2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("<=", Expr1, Expr2))
end)
 in ( LrTable.NT 1, ( result, Expr1left, Expr2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2(";", Expr1, Expr2))
end)
 in ( LrTable.NT 1, ( result, Expr1left, Expr2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: ( _, ( _, 
MINUS1left, _)) :: rest671)) => let val  result = MlyValue.Expr (fn _
 => let val  (Expr as Expr1) = Expr1 ()
 in (Prim1("-", Expr))
end)
 in ( LrTable.NT 1, ( result, MINUS1left, Expr1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: ( _, ( _, 
NEGATION1left, _)) :: rest671)) => let val  result = MlyValue.Expr (fn
 _ => let val  Expr1 = Expr1 ()
 in (Prim1("!", Expr1))
end)
 in ( LrTable.NT 1, ( result, NEGATION1left, Expr1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("&&", Expr1, Expr2))
end)
 in ( LrTable.NT 1, ( result, Expr1left, Expr2right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.MatchExpr MatchExpr1, _, MatchExpr1right))
 :: _ :: ( _, ( MlyValue.Expr Expr1, _, _)) :: ( _, ( _, MATCH1left, _
)) :: rest671)) => let val  result = MlyValue.Expr (fn _ => let val  (
Expr as Expr1) = Expr1 ()
 val  (MatchExpr as MatchExpr1) = MatchExpr1 ()
 in (Match(Expr, MatchExpr))
end)
 in ( LrTable.NT 1, ( result, MATCH1left, MatchExpr1right), rest671)

end
|  ( 16, ( ( _, ( MlyValue.Const Const1, Const1left, Const1right)) :: 
rest671)) => let val  result = MlyValue.AtomExpr (fn _ => let val  (
Const as Const1) = Const1 ()
 in (Const)
end)
 in ( LrTable.NT 2, ( result, Const1left, Const1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.NAME NAME1, NAME1left, NAME1right)) :: 
rest671)) => let val  result = MlyValue.AtomExpr (fn _ => let val  (
NAME as NAME1) = NAME1 ()
 in (Var(NAME))
end)
 in ( LrTable.NT 2, ( result, NAME1left, NAME1right), rest671)
end
|  ( 18, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.Expr Expr1, _,
 _)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let val  result = 
MlyValue.AtomExpr (fn _ => let val  (Expr as Expr1) = Expr1 ()
 in (Expr)
end)
 in ( LrTable.NT 2, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.CINT CINT1, CINT1left, CINT1right)) :: 
rest671)) => let val  result = MlyValue.Const (fn _ => let val  (CINT
 as CINT1) = CINT1 ()
 in (ConI(CINT))
end)
 in ( LrTable.NT 3, ( result, CINT1left, CINT1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.CBOOL CBOOL1, CBOOL1left, CBOOL1right)) :: 
rest671)) => let val  result = MlyValue.Const (fn _ => let val  (CBOOL
 as CBOOL1) = CBOOL1 ()
 in (ConB(CBOOL))
end)
 in ( LrTable.NT 3, ( result, CBOOL1left, CBOOL1right), rest671)
end
|  ( 21, ( ( _, ( _, END1left, END1right)) :: rest671)) => let val  
result = MlyValue.MatchExpr (fn _ => ([]))
 in ( LrTable.NT 4, ( result, END1left, END1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.MatchExpr MatchExpr1, _, MatchExpr1right))
 :: ( _, ( MlyValue.Expr Expr1, _, _)) :: _ :: ( _, ( 
MlyValue.CondExpr CondExpr1, _, _)) :: ( _, ( _, PIPE1left, _)) :: 
rest671)) => let val  result = MlyValue.MatchExpr (fn _ => let val  (
CondExpr as CondExpr1) = CondExpr1 ()
 val  (Expr as Expr1) = Expr1 ()
 val  (MatchExpr as MatchExpr1) = MatchExpr1 ()
 in ((CondExpr, Expr)::MatchExpr)
end)
 in ( LrTable.NT 4, ( result, PIPE1left, MatchExpr1right), rest671)

end
|  ( 23, ( ( _, ( MlyValue.Expr Expr1, Expr1left, Expr1right)) :: 
rest671)) => let val  result = MlyValue.CondExpr (fn _ => let val  (
Expr as Expr1) = Expr1 ()
 in (SOME(Expr))
end)
 in ( LrTable.NT 5, ( result, Expr1left, Expr1right), rest671)
end
|  ( 24, ( ( _, ( _, UNDERLINE1left, UNDERLINE1right)) :: rest671)) =>
 let val  result = MlyValue.CondExpr (fn _ => (NONE))
 in ( LrTable.NT 5, ( result, UNDERLINE1left, UNDERLINE1right), 
rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.Prog x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : PlcParser_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun MULT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun LESS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LESSEQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun NEGATION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun MATCH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun WITH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMIC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun PIPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun UNDERLINE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun NAME (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.NAME (fn () => i),p1,p2))
fun CINT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.CINT (fn () => i),p1,p2))
fun CBOOL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.CBOOL (fn () => i),p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
end
end
