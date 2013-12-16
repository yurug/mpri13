module Make
          (GAST : AST.GenericS)
: sig

  exception Error
  
  
  val program: (Lexing.lexbuf -> Tokens.token) -> Lexing.lexbuf -> (GAST.program)

end
