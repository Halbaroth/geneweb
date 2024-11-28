module type S = sig
  type 'a t
  type char_
  type word

  val empty : 'a t
  val cardinal : 'a t -> int
  val lookup : word -> 'a t -> (word * 'a) Seq.t
  val insert : word -> 'a -> 'a t -> 'a t
  val remove : word -> 'a t -> 'a t
  val fold : (word -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val iter : (word -> 'a -> unit) -> 'a t -> unit
  val to_seq : 'a t -> (word * 'a) Seq.t
  val pp : 'a Fmt.t -> 'a t Fmt.t
  val pp_statistics : 'a t Fmt.t
end

module Make (W : Word.S) = struct
  module M = Map.Make (struct
    type t = W.char_

    let compare = W.compare_char
  end)

  type char_ = W.char_
  type word = W.t

  type 'a node = Br of 'a t M.t | Lf of word
  and 'a t = { node : 'a node; data : 'a option; cardinal : int }

  let empty = { node = Br M.empty; data = None; cardinal = 0 }
  let[@inline always] cardinal { cardinal; _ } = cardinal
  let of_rev_list rl = W.of_list @@ List.rev rl

  let fold f t acc =
    let rec loop stack acc =
      match stack with
      | [] -> acc
      | (rev_pfx, t) :: stack ->
          let stack =
            match t.node with
            | Br children ->
                M.fold
                  (fun c tc stack -> (c :: rev_pfx, tc) :: stack)
                  children stack
            | Lf _ -> stack
          in
          let acc =
            match (t.node, t.data) with
            | Br _, Some v -> f (of_rev_list rev_pfx) v acc
            | Lf word, Some v -> f W.(of_rev_list rev_pfx @ word) v acc
            | Lf _, None -> assert false
            | Br _, None -> acc
          in
          loop stack acc
    in
    loop [ ([], t) ] acc

  let iter f t = fold (fun w v () -> f w v) t ()

  let to_seq pfx t =
    let rec loop rev_pfx t =
      match (t.node, t.data) with
      | Br children, _ -> (
          let seq =
            Seq.concat_map (fun (c, tc) -> loop (c :: rev_pfx) tc)
            @@ M.to_seq children
          in
          match t.data with
          | Some v ->
              let w = W.(pfx @ of_rev_list rev_pfx) in
              Seq.cons (w, v) seq
          | None -> seq)
      | Lf word, Some v ->
          let w = W.(pfx @ of_rev_list rev_pfx @ word) in
          Seq.return (w, v)
      | Lf _, None -> assert false
    in
    loop [] t

  (* Determine if the suffix [offset...] of [w1] is a prefix of [w2]. *)
  let is_prefix offset w1 w2 =
    let len1 = W.length w1 - offset in
    assert (len1 >= 0);
    let len2 = W.length w2 in
    let rec loop i =
      if i = len1 || i = len2 then i = len1
      else
        let c1 = W.get w1 (offset + i) in
        let c2 = W.get w2 i in
        W.compare_char c1 c2 = 0 && loop (i + 1)
    in
    loop 0

  let lookup pfx t =
    let len = W.length pfx in
    let rec loop rev_pfx i t =
      if i = len then to_seq (of_rev_list rev_pfx) t
      else
        match (t.node, t.data) with
        | Lf word, Some v when is_prefix i pfx word ->
            Seq.return (W.(of_rev_list rev_pfx @ word), v)
        | Lf _, Some _ -> Seq.empty
        | Lf _, None -> assert false
        | Br children, _ -> (
            let seq =
              let c = W.get pfx i in
              match M.find c children with
              | exception Not_found -> Seq.empty
              | t -> loop (c :: rev_pfx) (i + 1) t
            in
            match t.data with
            | Some v -> Seq.cons (of_rev_list rev_pfx, v) seq
            | None -> seq)
    in
    loop [] 0 t

  let insert pfx v t =
    let len = W.length pfx in
    let rec loop t i =
      if i = len then { t with data = Some v }
      else
        let node =
          match t.node with
          | Lf word when is_prefix i pfx word -> assert false
          | Br children ->
              Br
                (M.update (W.get pfx i)
                   (fun o ->
                     match o with
                     | Some child -> Some (loop child (i + 1))
                     | None ->
                         Some
                           {
                             node = Lf (W.suffix i pfx);
                             data = None;
                             cardinal = 1;
                           })
                   children)
        in
        { t with node; cardinal = t.cardinal + 1 }
    in
    loop t 0

  let remove s t = assert false
  (* let len = W.length s in *)
  (* let rec loop t i = *)
  (*   if i >= len then { t with data = None; cardinal = t.cardinal - 1 } *)
  (*   else *)
  (*     let children = *)
  (*       M.update (W.get s i) *)
  (*         (fun o -> *)
  (*           match o with *)
  (*           | None -> None *)
  (*           | Some children -> *)
  (*               let nc = loop children (i + 1) in *)
  (*               if nc.cardinal = 0 then None else Some nc) *)
  (*         t.children *)
  (*     in *)
  (*     { t with children; cardinal = t.cardinal - 1 } *)
  (* in *)
  (* loop t 0 *)

  let to_seq t = to_seq W.empty t

  let pp pp_val =
    Fmt.box
    @@ Fmt.iter_bindings ~sep:Fmt.sp iter
    @@ Fmt.parens
    @@ Fmt.pair ~sep:Fmt.comma W.pp pp_val

  let pp_human_size ppf i =
    let eucl u v = (u / v, u mod v) in
    let qm, rm = eucl i (1 lsl 20) in
    let qk, rk = eucl rm (1 lsl 10) in
    if qm > 0 then Fmt.pf ppf "%d Mio" qm
    else if qk > 0 then Fmt.pf ppf "%d Kio" rk
    else Fmt.pf ppf "%d o" rk

  let pp_statistics ppf t =
    Fmt.pf ppf "%d words (size %a)" t.cardinal pp_human_size
      (Obj.reachable_words (Obj.repr t))
end

module Default = Make (Word.Default)
