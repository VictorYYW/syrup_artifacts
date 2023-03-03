%{
open MyStdLib
open Lang

let mk_ctor_by_name
      (c:String.t)
      (e:Expr.t)
    : Expr.t =
  Expr.mk_ctor (Id.create c) e

%}

%token <string> LID
%token <string> UID
%token <string> STR
                (*%token <int> PROJ*)

%token <int> INT

%token FUN
%token MATCH
%token WITH
%token TYPE
%token OF
%token LET
%token LBRACKET
%token RBRACKET
(*%token IN*)
(*%token REC*)
%token UNIT

%token EQ
%token FATEQ
%token NEQ
%token EQUIV
%token ARR
%token COMMA
%token COLON
%token SIG
%token END
%token FORALL
%token VAL
%token BINDING
%token WILDCARD
%token MU
%token FIX
%token SYNTH
%token SATISFYING
%token SEMI
%token STAR
%token PIPE
%token LPAREN
%token RPAREN
%token DOT
%token EOF
%token INCLUDE

%start examples
%start exp
%type <(Lang.Expr.t list * Lang.Expr.t) list> examples
%type <Lang.Expr.t> exp

%%

examples:
  | exs=nonempty_examples EOF
    { exs }
  | { [] }

nonempty_examples:
  | ex=example SEMI exs=examples
    { ex::exs }
  | ex=example
    { [ex] }

example:
  | e1=exp ARR e2=exp
    { ([e1], e2) }
  | LPAREN es=nonempty_exp_list RPAREN ARR e=exp
    { (es,e) }

exp_list:
  | es=nonempty_exp_list
    { es }
  | { [] }

nonempty_exp_list:
  | e=exp COMMA es=exp_list
    { e::es }
  | e=exp
    { [e] }

(***** Expressions {{{ *****)

exp:
  | x=LID
    { Expr.mk_var (Id.create x) }
  | c=INT
    { Expr.from_int c }
  | c=UID
    {
      mk_ctor_by_name c Expr.mk_unit
    }
  | c=UID LPAREN RPAREN
    {
      mk_ctor_by_name c Expr.mk_unit
    }
  | c=UID LPAREN e=exp RPAREN
                     { mk_ctor_by_name c e }
  | c=UID e=exp
                     { mk_ctor_by_name c e }
  | c=UID LPAREN e=exp COMMA es=exp_comma_list_one RPAREN (* Sugar: ctor with tuple argument.  *)
		{ mk_ctor_by_name c (Expr.mk_tuple (e :: List.rev es)) }
  | LBRACKET RBRACKET
    { mk_ctor_by_name "Nil" (Expr.mk_unit)}
  | LBRACKET es=exp_comma_list_one RBRACKET
    {List.fold
	  es
          ~f:( fun acc e ->
	      mk_ctor_by_name "Cons" (Expr.mk_tuple [e; acc])
          )
	~init:(mk_ctor_by_name "Nil" (Expr.mk_unit))
    }

exp_comma_list_one:    (* NOTE: reversed *)
  | e=exp
    { [e] }
  | es=exp_comma_list_one COMMA e=exp
    { e :: es }

(***** }}} *****)

