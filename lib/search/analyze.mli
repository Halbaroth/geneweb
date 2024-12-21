type 'a loc = { content : 'a; offset : int; len : int }

val tokenize : string -> string loc list
val normalize : string -> string
val preprocess : string -> string loc list
val index_from_gzip : string -> Index.Default.t
