(** Command line options parsing. *)

let options = Arg.align []

let message = "Usage: joujou [options] input_file"

type source =
  | EMH of string
  | MH of string

let filename =
  let filename = ref None in
  Arg.parse options (fun s -> filename := Some s) message;
  match !filename with
    | None -> Errors.fatal [] "No input file."
    | Some filename ->
      if Filename.check_suffix filename ".mle" then
        EMH filename
      else if Filename.check_suffix filename ".mlt" then
        MH filename
      else
        Errors.fatal [] "Only .mlt and .mle files are accepted."
