type t

val make : ?max_connection:int -> ?idle:float -> unit -> t
(** [make ?max_connection ?idle ()] creates a connection manager.

    If [max_connection], respectively [idle], is omitted, there is no limit. *)

val add : t -> Lwt_unix.file_descr -> bool
(** [add t conn] adds the connection [conn] to the manager [t]. Return
    [false] if the limit [max_connection] is reached. *)

val close : t -> Lwt_unix.file_descr -> unit Lwt.t
(** [close t s conn] closes the connection [conn]. The function does not fail
    if the file descriptor is already closed. *)

val close_all : t -> unit Lwt.t

val ping : t -> Lwt_unix.file_descr -> unit
(** [ping t conn] signales that [conn] is still active. *)

val gc : t -> unit Lwt.t
(** [gc t] closes all the idle connections and remove all the already closed
    file descriptors. *)

val run : t -> unit Lwt.u
