(** Command line options parsing. *)

type source =
  | EMH of string
  | MH of string

(** The filename that has been provided on the command line. *)
val filename: source
