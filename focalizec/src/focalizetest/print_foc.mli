
(** {6 Print the .foc file} *)

(** {6 A custom Format module} *)
module MyFormat :
  sig
    val print_string : string -> unit (** Adds a string. *)
    val force_newline : unit -> unit  (** Prints a new line. *)
    val open_box : int -> unit        (** Opens a new box adding indent. *)
    val close_box : unit -> unit      (** Closes the last box. *)
    val print_space : unit -> unit    (** Prints a new space. *)
    val print_int : int -> unit       (** Prints an integer. *)
    val set_margin : int -> unit      (** Sets the width of a line. *)
    val set_formatter_out_channel : out_channel -> unit
                                      (** Sets the file to output. *)
  end

val print_myexpr : (string * string) list -> Own_expr.myexpr -> unit
val set_out_channel : out_channel -> unit

val print_foc_file :
  string ->
    Own_expr.fichier_foc ->
      Own_expr.fichier_fml ->
        unit
(** [print_foc_file file foc] prints on file [file] then foc file [foc]. *)

