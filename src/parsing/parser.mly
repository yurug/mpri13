(** Syntactic analysis. *)

%{
  open Name
  open GAST
  open Types
  open Positions

  let fresh_record_name =
    let r = ref 0 in
    fun () -> incr r; Name ("_record_" ^ string_of_int !r)

%}

(** Punctuation. *)
%token LPAREN RPAREN LBRACKET RBRACKET RARROW EOF COMMA
%token LBRACE RBRACE SEMICOLON DOT UNDERSCORE WITH MATCH BIGRARROW

(** Operators. *)
%token EQUAL COLON PIPE STAR

(** Identifiers. *)
%token<string> LID UID TID

(** Literals. *)
%token<int> INT
%token<char> CHAR
%token<string> STRING

(** Keywords. *)
%token FUN LET IN AND REC TYPE OF TINT TUNIT TCHAR AS
%token CLASS INSTANCE EXTERNAL

%right RARROW branches_prec
%right PIPE
%nonassoc AS
%nonassoc LID
%nonassoc tapp
%nonassoc RPAREN

%parameter<GAST : AST.GenericS>

%start<GAST.program> program

%%

program: bs = block_sequence EOF
{
  bs
}
| error {
  Errors.fatal [$startpos; $endpos] "Syntax error"
}

block_sequence:
/* empty */ {
 []
}
| c = class_definition b = block_sequence {
  BClassDefinition c :: b
}
| i = instance_definition b = block_sequence {
  match b with
    | BInstanceDefinitions ds :: bs -> BInstanceDefinitions (i :: ds) :: bs
    | bs -> BInstanceDefinitions [i] :: bs
}
| t = type_mutual_definitions b = block_sequence {
  BTypeDefinitions t :: b
}
| d = value_binding b = block_sequence {
  BDefinition d :: b
}
| d = external_value_binding b = block_sequence {
  BDefinition d :: b
}

class_definition:
CLASS
superclasses           = superclasses
class_predicate        = class_predicate_declaration
class_members          = record_type
{
  let class_position = lex_join $startpos $endpos in
  let ClassPredicate (class_name, class_parameter) = class_predicate in
  if List.exists
    (fun (ClassPredicate (_, p)) -> p <> class_parameter)
    superclasses
  then
    Errors.fatal [$startpos; $endpos]
      "The same type parameter as in the class must be used by superclasses.";
  let superclasses = List.map (fun (ClassPredicate (k, _)) -> k) superclasses in
  { class_position; superclasses; class_parameter; class_members; class_name }
}

%inline superclasses:
/* empty */
{
  []
}
| ps=separated_nonempty_list(COMMA, class_predicate_declaration) BIGRARROW
{
  ps
}

class_predicate_declaration:
class_name             = class_name
class_parameter        = tvname
{
  ClassPredicate (class_name, class_parameter)
}

class_name: c=UID
{
  TName c
}

instance_definition:
INSTANCE
instance_parameters     = type_parameters
instance_typing_context = superclasses
instance_class_name     = class_name
instance_index          = mldatatype2
instance_members        = simple_record_expression
{
  let instance_members = instance_members in
  let instance_position = lex_join $startpos $endpos in
  let instance_index =
    match instance_index with
      | TyApp (_, i, tys) ->
        if List.(
          length tys <> length instance_parameters
          || not (for_all2 (function
            | TyVar (_, t) -> fun t' -> t = t'
            | _ -> fun _ -> false
          ) tys instance_parameters)
        )
        then
          Errors.fatal [$startpos; $endpos]
            "The type arguments of the instance index must match exactly
             the type parameters of the instance."
        else
          i
      | _ ->
        Errors.fatal [$startpos; $endpos] "Invalid instance index."
  in
  { instance_position; instance_typing_context; instance_class_name;
    instance_parameters; instance_index; instance_members }
}

type_mutual_definitions: TYPE tds=separated_nonempty_list(AND, type_definition)
{
  TypeDefs (lex_join $startpos $endpos, tds)
}

type_definition:
ts=type_parameters_declaration t=tname EQUAL dt=datatype_definition
{
  let pos = lex_join $startpos $endpos in
  let kind = kind_of_arity (List.length ts) in
  let tvs = List.map (fun v -> TyVar (pos, v)) ts in
  let rty = TyApp (pos, t, tvs) in
  TypeDef (pos, kind, t, dt ts rty)
}
| EXTERNAL ts=type_parameters_declaration t=tname EQUAL s=STRING
{
  let pos = lex_join $startpos $endpos in
  ExternalType (pos, ts, t, s)
}
| error {
  Errors.fatal [$startpos; $endpos] "Syntax error"
}

datatype_definition:
adt=algebraic_datatype_definition
{
  fun ts rty ->
    let datacon (pos, k, tys) = (pos, k, ts, ntyarrow pos tys rty) in
    DAlgebraic (List.map datacon adt)
}
| rdt=record_type
{
  fun ts _ -> DRecordType (ts, rdt)
}

type_parameters_declaration:
/* empty */
{
  []
}
| t=tvname
{
  [t]
}
| LPAREN ts=separated_nonempty_list(COMMA, tvname) RPAREN
{
  ts
}

algebraic_datatype_definition:
PIPE? ds=separated_nonempty_list(PIPE, dataconstructor_definition)
{
  ds
}

dataconstructor_definition:
k=UID
{
  (lex_join $startpos $endpos, DName k, [])
}
| k=UID OF tys=separated_nonempty_list(STAR, mltype)
{
  (lex_join $startpos $endpos, DName k, tys)
}
| error {
  Errors.fatal [$startpos; $endpos] "Syntax error"
}

external_value_binding:
LET EXTERNAL
ts=type_parameters
b=binding EQUAL s=STRING
{
  ExternalValue (lex_join $startpos $endpos, ts, b, s)
}

value_definition:
ts=type_parameters
c=class_predicates
b=binding EQUAL e=expression
{
  ValueDef (lex_join $startpos $endpos, ts, c, b, e)
}

%inline class_predicates:
/* empty */
{
  []
}
| LBRACKET cs=separated_nonempty_list(COMMA, class_predicate) RBRACKET
{
  cs
}

%inline class_predicate:
class_name = class_name
class_arg  = tvname
{
  ClassPredicate (class_name, class_arg)
}

%inline type_parameters:
/* empty */
{
  []
}
| LBRACKET ts=tvname+ RBRACKET
{
  ts
}

rectag: REC   { true  }
| /* empty */ { false }


%inline value_binding:
LET r=rectag vs=separated_nonempty_list(AND, value_definition) {
  let vpos = lex_join $startpos(vs) $endpos(vs) in
  if r then BindRecValue (vpos, vs) else BindValue (vpos, vs)
}


expression:
  FUN bs=binding+ RARROW e=expression
{
  List.fold_right (fun b e -> ELambda (lex_join $startpos $endpos, b, e)) bs e
}
| vb=value_binding IN e=expression
{
  EBinding (lex_join $startpos $endpos, vb, e)
}
| LPAREN e=expression COLON ty=mltype RPAREN
{
  ETypeConstraint (lex_join $startpos $endpos, e, ty)
}
| r=record_expression
{
  let pos = lex_join $startpos $endpos in
  let (i, fs) = r in
  ERecordCon (pos, fresh_record_name (), i, fs)
}
| LBRACE ts=tvname+ RBRACE e=expression
{
  EExists (lex_join $startpos $endpos, ts, e)
}
| LBRACKET ts=tvname+ RBRACKET e=expression
{
  if GAST.implicit then
    Errors.fatal [$startpos; $endpos] "Syntax error"
  else
    EForall (lex_join $startpos $endpos, ts, e)
}
| k=UID
{
  EDCon (lex_join $startpos $endpos, DName k,
         instantiation $startpos LeftImplicit,
         [])
}
| k=UID LBRACKET tys=separated_list(COMMA, mltype) RBRACKET
{
  EDCon (lex_join $startpos $endpos, DName k,
         instantiation $startpos (TypeApplication tys),
         [])
}
| MATCH e=expression WITH PIPE? bs=branches
{
  EMatch (lex_join $startpos $endpos, e, bs)
}
| e=expression0
{
  e
}

%inline record_expression:
LBRACE
fs=separated_nonempty_list(SEMICOLON, record_binding)
RBRACE LBRACKET tys=separated_list(COMMA, mltype) RBRACKET
{
  (instantiation $startpos (TypeApplication tys), fs)
}
| fs=simple_record_expression
{
  (instantiation $startpos LeftImplicit, fs)
}

%inline simple_record_expression:
LBRACE
fs=separated_nonempty_list(SEMICOLON, record_binding)
RBRACE
{
  fs
}

branches: b=branch %prec branches_prec
{
  [b]
}
| b=branch PIPE bs=branches
{
  b::bs
}

branch: p=pattern RARROW e=expression
{
  Branch (lex_join $startpos $endpos, p, e)
}

pattern:
x=name
{
  PVar (lex_join $startpos $endpos, x)
}
| UNDERSCORE
{
  PWildcard (lex_join $startpos $endpos)
}
| p=pattern AS x=name
{
  PAlias (lex_join $startpos $endpos, x, p)
}
| LPAREN p=pattern COLON ty=mltype RPAREN
{
  PTypeConstraint (lex_join $startpos $endpos, p, ty)
}
| LPAREN RPAREN
{
  PPrimitive (lex_join $startpos $endpos, PUnit)
}
| x=INT
{
  PPrimitive (lex_join $startpos $endpos, PIntegerConstant x)
}
| x=CHAR
{
  PPrimitive (lex_join $startpos $endpos, PCharConstant x)
}
| k=UID
{
  PData (lex_join $startpos $endpos, DName k,
         instantiation $startpos LeftImplicit,
         [])
}
| k=UID LBRACKET tys=separated_list(COMMA, mltype) RBRACKET
{
  PData (lex_join $startpos $endpos, DName k,
         instantiation $startpos (TypeApplication tys),
         [])
}
| k=UID LPAREN ps=separated_nonempty_list(COMMA, pattern) RPAREN
{
  PData (lex_join $startpos $endpos, DName k,
         instantiation $startpos LeftImplicit,
         ps)
}
| k=UID
  LBRACKET tys=separated_list(COMMA, mltype) RBRACKET
  LPAREN ps=separated_nonempty_list(COMMA, pattern) RPAREN
{
  PData (lex_join $startpos $endpos, DName k,
         instantiation $startpos (TypeApplication tys),
         ps)
}
| a=pattern PIPE b=pattern
{
  POr (lex_join $startpos $endpos, [a; b])
}
| LPAREN p=pattern RPAREN
{
  p
}

expression0: a=expression0 b=expression2bis
{
  EApp (lex_join $startpos $endpos, a, b)
}
| e=expression1
{
  e
}

expression1: k=UID LPAREN es=separated_nonempty_list(COMMA, expression) RPAREN
{
  EDCon (lex_join $startpos $endpos, DName k,
         instantiation $startpos LeftImplicit,
         es)
}
| k=UID
  LBRACKET tys=separated_list(COMMA, mltype) RBRACKET
  LPAREN es=separated_nonempty_list(COMMA, expression) RPAREN
{
  EDCon (lex_join $startpos $endpos, DName k,
         instantiation $startpos (TypeApplication tys),
         es)
}
| e=expression2
{
  e
}

expression2: x=name
{
  let pos = lex_join $startpos $endpos in
  EVar (pos, x, instantiation $startpos LeftImplicit)
}
| x=name LBRACKET tys=separated_list(COMMA, mltype) RBRACKET
{
  let pos = lex_join $startpos $endpos in
  EVar (pos, x, instantiation $startpos (TypeApplication tys))
}
| LPAREN RPAREN
{
  EPrimitive (lex_join $startpos $endpos, PUnit)
}
| x=INT
{
  EPrimitive (lex_join $startpos $endpos, PIntegerConstant x)
}
| x=CHAR
{
  EPrimitive (lex_join $startpos $endpos, PCharConstant x)
}
| e=expression2 DOT l=lname
{
  ERecordAccess (lex_join $startpos $endpos, e, l)
}
| LPAREN e=expression RPAREN
{
  e
}

expression2bis:
x=name
{
  let pos = lex_join $startpos $endpos in
  EVar (pos, x, instantiation $startpos LeftImplicit)
}
| x=name LBRACKET tys=separated_list(COMMA, mltype) RBRACKET
{
  let pos = lex_join $startpos $endpos in
  EVar (pos, x, instantiation $startpos (TypeApplication tys))
}
| LPAREN RPAREN
{
  EPrimitive (lex_join $startpos $endpos, PUnit)
}
| x=INT
{
  EPrimitive (lex_join $startpos $endpos, PIntegerConstant x)
}
| x=CHAR
{
  EPrimitive (lex_join $startpos $endpos, PCharConstant x)
}
| LPAREN e=expression RPAREN
{
  e
}
| k=UID
{
  EDCon (lex_join $startpos $endpos, DName k,
         instantiation $startpos LeftImplicit,
         [])
}
| k=UID LBRACKET tys=separated_list(COMMA, mltype) RBRACKET
{
  EDCon (lex_join $startpos $endpos, DName k,
         instantiation $startpos (TypeApplication tys),
         [])
}
| e=expression2 DOT l=lname
{
  ERecordAccess (lex_join $startpos $endpos, e, l)
}


record_binding: l=lname EQUAL e=expression
{
  RecordBinding (l, e)
}

binding: x=name
{
  binding $startpos x None
}
| LPAREN x=name COLON ty=mltype RPAREN
{
  binding $startpos x (Some ty)
}

mltype: x=tvname
{
  TyVar (lex_join $startpos $endpos, x)
}
| ity=mltype RARROW oty=mltype
{
  let pos = lex_join $startpos $endpos in
  TyApp (pos, TName "->", [ ity; oty ])
}
| LPAREN t=mltype RPAREN
{
  t
}
| t=mldatatype %prec tapp
{
  t
}

mldatatype2: LPAREN m=mldatatype RPAREN
{
  m
}
| m=mldatatype
{
  m
}

mldatatype: TINT
{
  let pos = lex_join $startpos $endpos in
  TyApp (pos, TName "int", [])
}
| TCHAR
{
  let pos = lex_join $startpos $endpos in
  TyApp (pos, TName "char", [])
}
| TUNIT
{
  let pos = lex_join $startpos $endpos in
  TyApp (pos, TName "unit",  [])
}
| t=tname
{
  let pos = lex_join $startpos $endpos in
  TyApp (pos, t, [])
}
| ty=mltype t=tname
{
  let pos = lex_join $startpos $endpos in
  TyApp (pos, t, [ty])
}
| LPAREN ty=mltype COMMA tys=separated_nonempty_list(COMMA, mltype) RPAREN
  t=tname
{
  let pos = lex_join $startpos $endpos in
  TyApp (pos, t, ty :: tys)
}

record_type: LBRACE b=separated_list(SEMICOLON, label_type_declaration) RBRACE
{
  b
}

label_type_declaration: l=lname COLON t=mltype
{
  let pos = lex_join $startpos $endpos in
  (pos, l, t)
}

%inline lname: x=LID
{
  LName x
}

%inline name: x=LID
{
  Name x
}

%inline tname: x=LID
{
  TName x
}

%inline tvname: x=TID
{
  TName x
}
