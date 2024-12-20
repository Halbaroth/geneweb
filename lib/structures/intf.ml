module type Ordered = sig
  type t

  val dummy : t
  val compare : t -> t -> int
end
