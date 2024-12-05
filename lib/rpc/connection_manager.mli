type t

val make : ?max_connection:int -> ?idle:float -> unit -> t

val add : t -> Unix.sockaddr -> Lwt_unix.file_descr -> bool
