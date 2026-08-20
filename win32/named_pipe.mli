type t
(** Type of named pipe. *)

exception Error of string * int * string
(** Exception raised by all the functions of this module. The first string
    is the name of the function, the integer is the error code and the
    last string is a message produced by Windows.

    Never match on the last string as it could depends on the version of
    Windows and the language pack. *)

val create_named_pipe : string -> t
(** [open_pipe name] opens a new pipe with the name [name]. The name must be
    of the form `\\.\pipe\NAME`. *)

val to_file_descr : t -> Unix.file_descr

val connect : t -> unit

val disconnect : t -> unit

val flush : t -> unit

val wait : string -> unit

val close : t -> unit
(** [close t] closes the named pipe and flushes it before. *)
