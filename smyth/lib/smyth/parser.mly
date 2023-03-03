%{
open Lang

let mk_ctor_by_name
      (c:string)
      (e:exp)
    : exp =
    ECtor(c, [], e)

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
%type <(exp * exp) list> examples
%type <exp> exp

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
  | es=exp ARR e=exp
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
    { EVar x }
  | c=INT
    { Desugar.nat c }
  | c=UID
    {
      mk_ctor_by_name c (ETuple [])
    }
  | c=UID LPAREN RPAREN
    {
      mk_ctor_by_name c (ETuple [])
    }
  | c=UID e=exp
                     { mk_ctor_by_name c e }
  | c=UID LPAREN e=exp RPAREN
                     { mk_ctor_by_name c e }
  | c=UID LPAREN e=exp COMMA es=exp_comma_list_one RPAREN (* Sugar: ctor with tuple argument.  *)
		{ mk_ctor_by_name c (ETuple (e :: List.rev es)) }
  | LBRACKET RBRACKET
    { mk_ctor_by_name "Nil" (ETuple [])}
  | LBRACKET es=exp_comma_list_one RBRACKET
    {List.fold_left
          ( fun acc e ->
              ECtor ("Cons", [], ETuple [e; acc])
          )
          (ECtor ("Nil", [], ETuple []))
	  es
    }
  | LPAREN e=exp COMMA es=exp_comma_list_one RPAREN
    { ETuple (e :: List.rev es) }

exp_comma_list_one:    (* NOTE: reversed *)
  | e=exp
    { [e] }
  | es=exp_comma_list_one COMMA e=exp
    { e :: es }

(***** }}} *****)

