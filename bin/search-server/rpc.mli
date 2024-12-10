module I = Geneweb_search.Index.Default

module Context : sig
  type t = { word : string; offset : int; len : int }

  val compare : t -> t -> int

  module Set : Set.S with type elt = t
end

val dispatch : (string * I.t) list -> Server.handler
