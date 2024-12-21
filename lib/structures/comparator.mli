module type S = sig
  type elt
  type witness

  val compare : elt -> elt -> int
  val dummy : elt
end

type ('a, 'w) t = (module S with type elt = 'a and type witness = 'w)

val make : compare:('a -> 'a -> int) -> dummy:'a -> ('a, 'w) t
