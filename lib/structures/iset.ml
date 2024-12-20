module type S = sig
  type elt
  type t

  val of_seq : elt Seq.t -> t
  val to_seq : t -> elt Seq.t
  val mem : elt -> t -> bool
  val cardinal : t -> int

  module Iterator : Iterator.S with type elt = elt

  val iterator : t -> Iterator.t
end

module Make (O : Intf.Ordered) = struct
  type elt = O.t
  type t = O.t array

  let of_seq s =
    let l = List.of_seq s in
    let l = List.sort O.compare l in
    Array.of_list l

  let to_seq = Array.to_seq
  let cardinal = Array.length

  let binary_search e t u v =
    let rec loop i j =
      if i >= j then `Hole i
      else
        let mid = i + ((j - i) / 2) in
        let c = O.compare t.(mid) e in
        if c = 0 then `Found mid
        else if c < 0 then loop (mid + 1) j
        else loop i mid
    in
    loop u v

  let mem e t =
    match binary_search e t 0 (cardinal t) with
    | `Hole _ -> false
    | `Found _ -> true

  module Iterator = struct
    type nonrec elt = elt
    type nonrec t = { arr : t; mutable idx : int }

    exception End

    let curr { arr; idx } = if idx < cardinal arr then arr.(idx) else raise End
    let next it = if it.idx < cardinal it.arr then it.idx <- it.idx + 1

    let seek e it =
      if it.idx < cardinal it.arr then
        let (`Hole i | `Found i) =
          binary_search e it.arr it.idx (cardinal it.arr)
        in
        it.idx <- i
  end

  let iterator t = Iterator.{ arr = t; idx = 0 }
end
