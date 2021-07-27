%%

%name PlcParser

%pos int


%term VAR
    | AND
    | IF | THEN | ELSE
    | PLUS | MINUS | MULT | DIV | EQ | LESS | LESSEQUAL | NEGATION
    | LPAR | RPAR
    | MATCH | WITH | END
    | SEMIC | ARROW | PIPE | UNDERLINE
    | NAME of string | CINT of int | CBOOL of bool
    | EOF

%nonterm Prog of expr | Expr of expr | AtomExpr of expr | Const of expr 
    | MatchExpr of (expr option * expr) list | CondExpr of expr option

%right SEMIC
%left AND PLUS MINUS MULT DIV LESSEQUAL NEGATION EQ LESS
%nonassoc IF ELSE

%eop EOF

%noshift EOF

%start Prog

%%

Prog : Expr (Expr)
    | VAR NAME EQ Expr SEMIC Prog (Let(NAME, Expr, Prog))

Expr : AtomExpr (AtomExpr)
    | IF Expr THEN Expr ELSE Expr (If(Expr1, Expr2, Expr3))
    | Expr PLUS Expr (Prim2("+", Expr1, Expr2))
    | Expr MINUS Expr (Prim2("-", Expr1, Expr2))
    | Expr MULT Expr (Prim2("*", Expr1, Expr2))
    | Expr DIV Expr (Prim2("/", Expr1, Expr2))
    | Expr EQ Expr (Prim2("=", Expr1, Expr2))
    | Expr LESS Expr (Prim2("<", Expr1, Expr2))
    | Expr LESSEQUAL Expr (Prim2("<=", Expr1, Expr2))
    | Expr SEMIC Expr (Prim2(";", Expr1, Expr2))
    | MINUS Expr (Prim1("-", Expr))
    | NEGATION Expr (Prim1("!", Expr1))
    | Expr AND Expr (Prim2("&&", Expr1, Expr2))
    | MATCH Expr WITH MatchExpr (Match(Expr, MatchExpr))

AtomExpr : Const (Const)
    | NAME (Var(NAME))
    | LPAR Expr RPAR (Expr)

Const : CINT (ConI(CINT))
    | CBOOL (ConB(CBOOL))

MatchExpr : END ([])
	| PIPE CondExpr ARROW Expr MatchExpr ((CondExpr, Expr)::MatchExpr)

CondExpr : Expr (SOME(Expr))
	| UNDERLINE (NONE)