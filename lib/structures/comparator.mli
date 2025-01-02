module type S = sig
  type t
  type witness

  val compare : t -> t -> int
  val dummy : t
end

type ('a, 'w) t = (module S with type t = 'a and type witness = 'w)

val make : compare:('a -> 'a -> int) -> dummy:'a -> ('a, 'w) t
