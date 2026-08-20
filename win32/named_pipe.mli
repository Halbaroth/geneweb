type t
(** Type of file descriptors for named pipes. *)

exception Error of string * int * string
(** Exception raised by all the functions of this module. The first string is
    the name of the function, the integer is the error code and the last string
    is a message produced by Windows.

    Never match on the last string as it could depends on the version of Windows
    and the language pack. *)

type open_mode =
  | PIPE_ACCESS_DUPLEX  (** Bidirectional pipe. *)
  | PIPE_ACCESS_INBOUND
      (** Read-only pipe for the server and write-only for the client. *)
  | PIPE_ACCESS_OUTBOUND
      (** Write-only pipe for the server and read-only for the client. *)
  | FILE_FLAG_FIRST_PIPE_INSTANCE
  | FILE_FLAG_WRITE_THROUGH
  | FILE_FLAG_OVERLAPPED

type pipe_mode =
  | PIPE_TYPE_BYTE
  | PIPE_TYPE_MESSAGE
  | PIPE_READMODE_BYTE
  | PIPE_READMODE_MESSAGE
  | PIPE_WAIT
  | PIPE_NOWAIT
  | PIPE_ACCEPT_REMOTE_CLIENTS
  | PIPE_REJECT_REMOTE_CLIENTS

(** Time-out options for [wait_named_pipe]. *)
type time_out =
  | WAIT_USE_DEFAULT_WAIT
      (** The time-out interval is the default value specified by the server
          while creating the pipe with [open_pipe_name]. *)
  | WAIT_WAIT_FOREVER
      (** Wait forever until an instance of the named pipe is available. *)
  | TIME_OUT of int  (** The number of milliseconds to wait. *)

val pipe_unlimited_instances : unit -> int
(** [pipe_unlimited_instances ()] returns the maximum instances of a pipe, that
    is [255]. *)

val create_named_pipe :
  string -> open_mode list -> pipe_mode list -> int -> int -> int -> t
(** [open_pipe name] opens a new pipe with the name [name]. The name must be of
    the form `\\.\pipe\NAME`. *)

val connect_named_pipe : t -> unit
(** [connect_named_pipe fd] ... *)

val disconnect_named_pipe : t -> unit
(** [disconnect_named_pipe fd] ... *)

val wait_named_pipe : string -> time_out -> unit
(** [wait_named_pipe name] ... *)

val to_file_descr : t -> Unix.file_descr
(** [to_fiel_descr fd] returns the underlying Unix descriptor. It is recommended
    to close the pipe descriptor itself. *)

val close : t -> unit
(** [close fd] closes the named pipe and flushes it before. *)
