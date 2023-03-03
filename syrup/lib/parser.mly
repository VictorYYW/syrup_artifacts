%{
open Lang

let mk_ctor_by_name
      (c:string)
      (es:exp list)
    : exp =
  match c with
  | "Some" -> EOp(Just, es)
  | "None" -> EOp(No, [])
  | "T" -> EOp(True, [])
  | "F" -> EOp(False, [])
  | "Node" -> EOp (Node, match es with
			 | [l;v;r] -> [v;l;r]
			 | _ -> failwith "impossible")
  | "Leaf" -> EOp(Leaf, [])
  | _ -> failwith (c ^ " parser unimplemented")

let mk_type_by_name (c:string) : typ =
  match c with
  | "int" -> TNum
  | "bool" -> TBool
  | _ -> failwith (c ^ " parser unimplemented")

let mk_param_type_by_name (c:string) (t:typ): typ =
  match c with
  | "list" -> TList t
  | _ -> failwith (c ^ " parser unimplemented")

let rec rev_exps_to_exp ?(acc=EOp (Nil, [])) (es : exp list) : exp = 
  match es with
  | [] -> acc
  | hd::tl -> rev_exps_to_exp ~acc:(EOp (Cons, [hd; acc])) tl
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

(***** Declarations {{{ *****)

decl_list:
  | (* empty *)
    { [] }
  | d=decl ds=decl_list
    { d::ds }

decl:
  | TYPE i=LID EQ t=typ
    { Declaration.type_dec (Id.create i) t }
  | LET i=LID EQ e=exp SEMI SEMI
    { Declaration.expr_dec (Id.create i) e }

(*datatype:
  | TYPE d=LID EQ cs=ctors
    { DData (d, List.rev cs) }*)

(*letbind:
  | LET x=LID COLON t=typ EQ e=exp SEMI SEMI
    { DLet (x, false, [], t, e) }
  | LET x=LID args=arg_list COLON t=typ EQ e=exp SEMI SEMI
    { DLet (x, false, List.rev args, t, e) }
  | LET REC x=LID args=arg_list COLON t=typ EQ e=exp SEMI SEMI
    { DLet (x, true, List.rev args, t, e) }*)

(*ctors:  (* NOTE: reversed *)
  | (* empty *)
    { [] }
  | cs=ctors c=ctor
    { c::cs }*)

(*ctor:
  | PIPE c=UID OF t=typ
    { (c, t)  }
  | PIPE c=UID
    { (c, TUnit) }  *)

(*synth_problem:
  | LET x=LID COLON t=typ IMPLIES LBRACE es=evidencelist RBRACE EQ HOLE
    { (x, t, es) }*)

(***** }}} *****)

(***** Types {{{ *****)

typ:
  | t=typ_arrow   { t }
  | t=typ_tuple   { t }
  | t=typ_base    { t }
  | t=typ_paren   { t }
  | t=typ_param { t }

typ_param:
  | t=typ f=UID
    { mk_param_type_by_name f t }

typ_arrow:
  | t=typ_non_arrow ARR t2=typ { Typ.Arr (t, t2) }

typ_non_arrow:
  | t=typ_tuple { t }
  | t=typ_base  { t }
  | t=typ_paren { t }

typ_tuple:
  | ts=typ_tuple_list { Typ.Prod ts }

(* STAR binds more tightly than ARR, so we can't have an arrow type on the left. *)
typ_tuple_list:
  | t=typ_base  STAR ts=typ_tuple_list_one { t :: List.rev ts }
  | t=typ_paren STAR ts=typ_tuple_list_one { t :: List.rev ts }

typ_tuple_list_one: (* NOTE: reversed *)
  | t=typ_base  { [t] }
  | t=typ_paren { [t] }
  | ts=typ_tuple_list_one STAR t=typ_base  { t :: ts }
  | ts=typ_tuple_list_one STAR t=typ_paren { t :: ts }

typ_base:
  | d=LID { mk_type_by_name d }

typ_paren:
  | LPAREN t=typ RPAREN { t }

(***** }}} *****)

(***** Expressions {{{ *****)

exp:
  | x=LID
    { EVar x }
  | c=INT
    { Denotation.int c }
  | c=UID
    {
      mk_ctor_by_name c []
    }
  | c=UID LPAREN RPAREN
    {
      mk_ctor_by_name c []
    }
  | c=UID e=exp
                     { mk_ctor_by_name c [e] }
  | c=UID LPAREN e=exp RPAREN
                     { mk_ctor_by_name c [e] }
  | c=UID LPAREN e=exp COMMA es=exp_comma_list_one RPAREN (* Sugar: ctor with tuple argument.  *)
                                  { mk_ctor_by_name c (e :: List.rev es) }
  | LPAREN e=exp COMMA es=exp_comma_list_one RPAREN
    { ETuple (e :: List.rev es) }
  | LBRACKET RBRACKET
    { EOp(Nil, []) }
  | LBRACKET e=exp RBRACKET
    { EOp (Cons, [e; EOp (Nil, [])]) }
  | LBRACKET e=exp COMMA es=exp_comma_list_one RBRACKET
    { EOp (Cons, [e; rev_exps_to_exp es]) }

list_exp:
  | e=exp
    { EOp(Cons, [e; EOp(Nil, [])]) }
  | e=exp COMMA es=list_exp
    { EOp(Cons, [e; es]) }

exp_comma_list_one:    (* NOTE: reversed *)
  | e=exp
    { [e] }
  | es=exp_comma_list_one COMMA e=exp
    { e :: es }
(***** }}} *****)

