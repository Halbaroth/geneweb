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
  type elt = C.elt
  type t = C.elt array
  type cmp = C.witness

  let of_seq s =
    let l = List.of_seq s in
    let l = List.sort C.compare l in
    Array.of_list l

  let to_seq = Array.to_seq
  let cardinal = Array.length

  let binary_search e t u v =
    let rec loop i j =
      if i >= j then `Gap i
      else
        let mid = i + ((j - i) / 2) in
        let c = C.compare t.(mid) e in
        if c = 0 then `Found mid
        else if c < 0 then loop (mid + 1) j
        else loop i mid
    in
    loop u v

  let mem e t =
    match binary_search e t 0 (cardinal t) with
    | `Gap _ -> false
    | `Found _ -> true

  let iterator t =
    object
      val mutable idx = 0
      method curr () = if idx < cardinal t then t.(idx) else raise Iterator.End
      method next () = if idx < cardinal t then idx <- idx + 1

      method seek e =
        if idx < cardinal t then
          let (`Gap i | `Found i) = binary_search e t idx (cardinal t) in
          idx <- i
    end
end
