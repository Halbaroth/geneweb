module type S = sig
  type t
  type witness

  val compare : t -> t -> int
  val dummy : t
end

type ('a, 'w) t = (module S with type t = 'a and type witness = 'w)

let make (type a b) ~(compare : a -> a -> int) ~(dummy : a) : (a, b) t =
  (module struct
    type t = a
    type witness = b

    let compare = compare
    let dummy = dummy
  end)
