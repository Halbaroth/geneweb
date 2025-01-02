module Iterator = Geneweb_structures.Iterator

module type S = sig
  type t
  type char_
  type word
  type entry

  val of_seq : (word * entry) Seq.t -> t
  val search : word list -> t -> entry Seq.t
  val search_prefix : word list -> t -> entry Seq.t
end

module type Entry = sig
  type t

  val dummy : t
  val compare : t -> t -> int
  val hash : t -> int
  val pp : t Fmt.t
end

module Make (W : Geneweb_structures.Word.S) (E : Entry) = struct
  type char_ = W.char_
  type word = W.t
  type entry = E.t

  module Trie = Geneweb_structures.Trie.Make (W)

  (* Hash consed entries *)
  module HE = struct
    type t = { e : E.t; mutable tag : int }
    type witness

    module W = Weak.Make (struct
      type nonrec t = t

      let hash { e; _ } = E.hash e
      let equal { e = e1; _ } { e = e2; _ } = E.compare e1 e2 = 0
    end)

    let compare { tag = t1; _ } { tag = t2; _ } = t1 - t2
    let dummy = { e = E.dummy; tag = -1 }
    let[@inline always] to_entry { e; _ } = e

    let add =
      let tbl = W.create 200 in
      let ctr = ref 0 in
      fun e ->
        let he = W.merge tbl { e; tag = -1 } in
        if he.tag = -1 then (
          he.tag <- !ctr;
          incr ctr);
        he
  end

  module Iset = Geneweb_structures.Iset.Make (HE)

  type t = Iset.t Trie.t

  let of_seq =
    let module SE = Set.Make (HE) in
    fun s ->
      let t : SE.t Trie.t = Trie.empty in
      let t =
        Seq.fold_left
          (fun t (w, e) ->
            let he = HE.add e in
            Trie.update w
              (fun so ->
                match so with
                | None -> Some (SE.singleton he)
                | Some s -> Some (SE.add he s))
              t)
          t s
      in
      let t =
        Trie.fold
          (fun w se t -> Trie.add w (Iset.of_seq @@ SE.to_seq se) t)
          t Trie.empty
      in
      t

  let search ws t =
    let rec loop w =
      let len = W.length w in
      fun i t ->
        if i = len then
          Option.bind (Trie.data t) @@ fun s -> Some (Iset.iterator s)
        else
          match Trie.next (W.get w i) t with
          | exception Not_found -> None
          | t -> loop w (i + 1) t
    in
    let l =
      List.fold_left
        (fun acc w ->
          match loop w 0 t with Some it -> it :: acc | None -> acc)
        [] ws
    in
    match l with
    | [] -> Seq.empty
    | _ :: _ -> Seq.map HE.to_entry @@ Iterator.to_seq @@ Iterator.join l

  let search_prefix ps t =
    let rec loop acc pfx =
      let len = W.length pfx in
      fun i t ->
        if i = len then
          Trie.fold (fun _ se acc -> Iset.iterator se :: acc) t acc
        else
          match Trie.next (W.get pfx i) t with
          | exception Not_found -> acc
          | t -> loop acc pfx (i + 1) t
    in
    let l =
      List.fold_left
        (fun acc pfx ->
          match loop acc pfx 0 t with [] -> acc | l -> Iterator.union l :: acc)
        [] ps
    in
    match l with
    | [] -> Seq.empty
    | _ :: _ -> Seq.map HE.to_entry @@ Iterator.to_seq @@ Iterator.join l
end

type 'a loc = { content : 'a; offset : int; len : int }

module Default =
  Make
    (Geneweb_structures.Word.Default)
    (struct
      type t = string loc

      let dummy = { content = ""; offset = -1; len = -1 }

      let compare e1 e2 =
        let c = Int.compare e1.offset e2.offset in
        if c <> 0 then c
        else
          let c = Int.compare e1.len e2.len in
          if c <> 0 then c else String.compare e1.content e2.content

      let hash = Hashtbl.hash

      let pp ppf { content; offset; len } =
        Fmt.pf ppf "{ content = %s; offset = %d; len = %d }" content offset len
    end)
