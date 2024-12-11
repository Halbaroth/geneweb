type loc = { offset : int; len : int }

module type S = sig
  type t
  type word

  val empty : t
  val add : word -> word -> loc -> t -> t
  val search : word -> t -> (word * loc) Seq.t
  val search2 : word list -> t -> (word * loc) list
  val pp_statistics : t Fmt.t
end

module Make (W : Word.S) = struct
  module T = Trie.Make (W)

  type word = W.t

  module HW = struct
    type t = { w : word; mutable tag : int }

    let equal u v = W.equal u.w v.w
    let hash u = W.hash u.w
  end

  module W = Weak.Make (HW)

  (* module Set = Set.Make (struct
    type t = HW.t loc

    let compare { content = w1; offset = o1; length = l1 }
        { content = w2; offset = o2; length = l2 } =
      let c = Int.compare w1.HW.tag w2.HW.tag in
      if c <> 0 then c
      else
        let c = Int.compare o1 o2 in
        if c <> 0 then c else Int.compare l1 l2
  end)

  let add_word =
    let tbl = W.create 1_024 in
    let ctr = ref 0 in
    fun w ->
      let hw = W.merge tbl { w; tag = -1 } in
      if hw.tag = -1 then (
        hw.tag <- !ctr;
        incr ctr);
      hw

  type t = Set.t T.t

  let empty = T.empty

  let add w s t =
    let u =
      { content = add_word s.content; offset = s.offset; length = s.length }
    in
    T.update w
      (function
        | Some set -> Some (Set.add u set) | None -> Some (Set.singleton u))
      t

  let search w t =
    T.search w t
    |> Seq.map (fun (_, set) ->
           Seq.map
             (fun { content; offset; length } ->
               { content = content.HW.w; offset; length })
             (Set.to_seq set))
    |> Seq.concat

  let inter = assert false
  (* let inter (seq1 : word loc Seq.t) (seq2 : word loc Seq.t) : string Seq.t =
    match (seq1 (), seq2 ()) with
    | Seq.Cons ({ content = w1; _ }, tl1), Seq.Cons ({ content = w2; _ }) ->
        assert false
    | _ -> fun () -> Seq.Nil *)

  (* let search2 pats t =
     let tbl : (string, word loc list) Hashtbl.t = Hashtbl.create 17 in
     let l = List.map (fun w -> search w t |> List.of_seq) pats in
     List.iteri
       (fun i ws ->

       ) l; *)

  let pp_statistics = T.pp_statistics *)

  type t = int
  let empty = 0

  let add = assert false
  let search = assert false
  let search2 = assert false
  let pp_statistics = assert false
end

module Default = Make (Word.Default)
