module I = Geneweb_search.Index.Default

val dispatch : (string * Util.SS.t I.t) list -> Server.handler
