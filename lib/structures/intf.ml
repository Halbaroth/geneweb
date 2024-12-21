module type Ordered = sig
  type t
  type cmp

  val dummy : t
  val compare : t -> t -> int

  val comparator : (t, cmp) Comparator.t
end
