module type S = sig
  type elt
  type t
  type cmp

  val of_seq : elt Seq.t -> t
  val to_seq : t -> elt Seq.t
  val mem : elt -> t -> bool
  val cardinal : t -> int
  val iterator : t -> (elt, cmp) Iterator.t
end

module Make (C : Comparator.S) = struct
  type elt = C.t
  type t = C.t array
  type cmp = C.witness

  let of_seq s =
    let l = List.of_seq s in
    let l = List.sort C.compare l in
    Array.of_list l

  let to_seq = Array.to_seq
  let cardinal = Array.length

  let binary_search e t lo hi =
    let rec loop l h =
      if l >= h then `Gap l
      else
        let mid = l + ((h - l) / 2) in
        let c = C.compare t.(mid) e in
        if c = 0 then `Found mid
        else if c < 0 then loop (mid + 1) h
        else loop l mid
    in
    loop lo hi

  let mem e t =
    match binary_search e t 0 (cardinal t) with
    | `Gap _ -> false
    | `Found _ -> true

  let exponential_search e t lo =
    let c = cardinal t in
    let rec loop i = if i < c && t.(i) < e then loop (2 * i) else min i c in
    let hi = loop 1 in
    binary_search e t (hi / 2) hi

  let iterator t =
    object
      val mutable idx = 0

      method comparator
          : (module Comparator.S with type t = C.t and type witness = C.witness)
          =
        (module C)

      method curr () = if idx < cardinal t then t.(idx) else raise Iterator.End
      method next () = if idx < cardinal t then idx <- idx + 1

      method seek e =
        if idx < cardinal t then
          let (`Gap i | `Found i) = exponential_search e t idx in
          idx <- i
    end
end
