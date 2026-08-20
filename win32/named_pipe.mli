val open_pipe : string -> Unix.file_descr
val connect : Unix.file_descr -> unit
val disconnect : Unix.file_descr -> unit
val wait : string -> unit
