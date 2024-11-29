module I = Geneweb_search.Index.Default

val dispatch : (string * unit I.t) list -> Server.handler
