(** Parsing and pretty-printing of AST for ML variants. *)

module type S =
sig
  module AST : AST.GenericS
  val parse_program : string -> AST.program
  val pprint_program : AST.program -> PPrintEngine.document
  val pprint_program_in_ocaml : AST.program -> PPrintEngine.document
  val pprint_expression : AST.expression -> PPrint.document
  val pprint_ml_type : Types.t -> PPrint.document
  val pprint_ml_kind : Types.kind -> PPrint.document
end

module Make (AST : AST.GenericS) = struct

  module AST = AST
  module I = Parser.Make (AST)
  module O = PrettyPrint.Make (AST)

  let parse_program filename =
    let cin = open_in filename in
    let buf = Lexing.from_channel cin in
    Lexing.(buf.lex_curr_p <- { buf.lex_curr_p with pos_fname = filename });
    let ast = I.program Lexer.token buf in
    close_in cin;
    ast

  let pprint_program ast =
    O.program false ast

  let pprint_program_in_ocaml ast =
    O.program true ast

  let pprint_expression ast =
    O.expression ast

  let pprint_ml_type ast =
    O.ml_type ast

  let pprint_ml_kind ast =
    O.ml_kind ast

end

module IAST = Make (IAST)
module XAST = Make (XAST)

let to_string pprint ast =
  let b = Buffer.create 13 in
  PPrintEngine.ToBuffer.pretty 0.8 100 b (pprint ast);
  Buffer.contents b
