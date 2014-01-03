(** The front-end driver. *)

type filename = Filename of string

type ('a, 'b) pass = 'a -> filename -> 'b

let ( $> ) : ('a, 'b) pass -> ('b, 'c) pass -> ('a, 'c) pass = fun p1 p2 ->
  fun x filename ->
    let y = p1 x filename in
    p2 y filename

let save_as ext ?(check = ignore) f = fun x ((Filename origin) as ofilename) ->
  let (y, printed_y) = f x ofilename in
  let filename = Filename.chop_extension origin ^ ext in
  let cout = open_out filename in
  PPrint.ToChannel.pretty 0.8 100 cout printed_y;
  close_out cout;
  check filename;
  y

let parse : (unit, IAST.program) pass
= save_as ".mls" (fun () (Filename f) ->
  let iast = ASTio.IAST.parse_program f in
  (iast, ASTio.IAST.pprint_program iast)
)

let parse_explicitly_typed : (unit, XAST.program) pass
= save_as ".mlse" (fun () (Filename f) ->
  let xast = ASTio.XAST.parse_program f in
  (xast, ASTio.XAST.pprint_program xast)
)

let is_explicitly_typed_syntax f =
  try
    ignore (ASTio.XAST.parse_program f)
  with _ ->
    Errors.fatal [] (f ^ ": Syntax error in generated code.")

let elaborate_type_annotations : (IAST.program, XAST.program) pass
= save_as ".mle" ~check:is_explicitly_typed_syntax (fun iast _ ->
  let xast = InferTypes.program iast in
  (xast, ASTio.XAST.pprint_program xast)
)

let elaborate_dictionaries : (XAST.program, XAST.program) pass
= save_as ".mlr" ~check:is_explicitly_typed_syntax (fun xast _ ->
  let rast = ElaborateDictionaries.program xast in
  (rast, ASTio.XAST.pprint_program rast)
)

let compile : (XAST.program, unit) pass
= save_as ".ml" (fun xast _ ->
  ((), ASTio.XAST.pprint_program_in_ocaml xast)
)

let process : unit = Options.(
  match filename with
    | EMH f ->
      (parse_explicitly_typed $> elaborate_dictionaries $> compile)
        () (Filename f)

    | MH f ->
      (parse $> elaborate_type_annotations $> elaborate_dictionaries $> compile)
        () (Filename f)
)
