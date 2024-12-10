type loc = { offset : int; len : int }

module Loc = struct
  type t = loc

  let compare { offset = o1; len = l1 } { offset = o2; len = l2 } =
    let c = Int.compare o1 o2 in
    if c <> 0 then c else Int.compare l1 l2

  module Set = Set.Make (struct
    type nonrec t = t

    let compare = compare
  end)
end

let compare_loc = Loc.compare

module type S = sig
  type t
  type word

  val empty : t
  val add : word -> word * loc -> t -> t
  val search : word -> t -> (word * loc Seq.t) Seq.t
  val pp_statistics : t Fmt.t
end

module Make (W : Word.S) = struct
  module T = Trie.Make (W)

  type word = W.t

  module HW = struct
    type t = { w : word; mutable tag : int }

    let equal u v = W.equal u.w v.w
    let hash u = W.hash u.w

    module Map = Map.Make (struct
      type nonrec t = t

      let compare u v = Int.compare u.tag v.tag
    end)
  end

  module W = Weak.Make (HW)

  let add_word =
    let tbl = W.create 1_024 in
    let ctr = ref 0 in
    fun w ->
      let hw = W.merge tbl { w; tag = -1 } in
      if hw.tag = -1 then (
        hw.tag <- !ctr;
        incr ctr);
      hw

  type t = Loc.Set.t HW.Map.t T.t

  let empty = T.empty

  let add w (s, loc) t =
    let hs = add_word s in
    T.update w
      (function
        | Some m ->
            HW.Map.update hs
              (function
                | Some s -> Some (Loc.Set.add loc s)
                | None -> Some (Loc.Set.singleton loc))
              m
            |> Option.some
        | None -> Some (HW.Map.singleton hs (Loc.Set.singleton loc)))
      t

  let search w t =
    T.search w t
    |> Seq.map (fun (_, m) ->
           HW.Map.to_seq m
           |> Seq.map (fun (hw, locs) -> (hw.HW.w, Loc.Set.to_seq locs)))
    |> Seq.concat

  let pp_statistics = T.pp_statistics
end

module Default = Make (Word.Default)
