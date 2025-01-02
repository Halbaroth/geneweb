val tokenize : string -> string Index.loc list
val normalize : string -> string
val preprocess : string -> string Index.loc list
val index_from_gzip : string -> Index.Default.t
