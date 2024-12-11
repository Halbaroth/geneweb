module I = Geneweb_search.Index.Default
val dispatch : (string * I.t) list -> Server.handler
