module type S = sig
  type elt
  type witness

  val compare : elt -> elt -> int
  val dummy : elt
end

type ('a, 'w) t = (module S with type elt = 'a and type witness = 'w)

let make (type a b) ~(compare : a -> a -> int) ~(dummy : a) : (a, b) t =
  (module struct
    type elt = a
    type witness = b

    let compare = compare
    let dummy = dummy
  end)
