module type S = sig
  type 'a t
  type char_
  type word

  val empty : 'a t
  val of_list : (word * 'a) list -> 'a t
  val cardinal : 'a t -> int
  val mem : word -> 'a t -> bool
  val fuzzy_mem : max_dist:int -> word -> 'a t -> bool
  val add : word -> 'a -> 'a t -> 'a t
  val update : word -> ('a option -> 'a option) -> 'a t -> 'a t
  val remove : word -> 'a t -> 'a t
  val search : word -> 'a t -> (word * 'a) Seq.t
  val fuzzy_search : max_dist:int -> word -> 'a t -> (word * 'a) Seq.t
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

  module Automaton = Automaton.Make (W)

  type char_ = W.char_
  type word = W.t
  type 'a t = Node of 'a t M.t * 'a option * int

  let empty = Node (M.empty, None, 0)
  let[@inline always] cardinal (Node (_, _, cardinal)) = cardinal
  let of_rev_list rl = W.of_list @@ List.rev rl

  let fold f t acc =
    let rec loop stack acc =
      match stack with
      | [] -> acc
      | (rev_pfx, t) :: stack ->
          let (Node (children, data, _)) = t in
          let stack =
            M.fold
              (fun c tc stack -> (c :: rev_pfx, tc) :: stack)
              children stack
          in
          let acc =
            match data with
            | Some v -> f (of_rev_list rev_pfx) v acc
            | None -> acc
          in
          loop stack acc
    in
    loop [ ([], t) ] acc

  let iter f t = fold (fun w v () -> f w v) t ()

  let pp pp_val =
    Fmt.box
    @@ Fmt.iter_bindings ~sep:Fmt.sp iter
    @@ Fmt.parens
    @@ Fmt.pair ~sep:Fmt.comma W.pp pp_val

  let mem word t =
    let len = W.length word in
    let rec loop i t =
      let (Node (children, data, _)) = t in
      if i = len then Option.is_some data
      else
        let c = W.get word i in
        match M.find c children with
        | exception Not_found -> false
        | t -> loop (i + 1) t
    in
    loop 0 t

  let fuzzy_mem ~max_dist word t =
    let module A = Automaton (struct
      type nonrec word = word

      let pattern = word
      let max_dist = max_dist
    end) in
    let len = W.length word in
    let rec loop i t st =
      let (Node (children, _, _)) = t in
      if i = len && A.accept st then true
      else if A.can_match st then
        M.exists (fun c child -> loop (i + 1) child (A.next c st)) children
      else false
    in
    loop 0 t A.init

  let to_seq pfx t =
    let rec loop rev_pfx t =
      let (Node (children, data, _)) = t in
      let seq =
        Seq.concat_map (fun (c, tc) -> loop (c :: rev_pfx) tc)
        @@ M.to_seq children
      in
      match data with
      | Some v ->
          let w = W.(pfx ^ of_rev_list rev_pfx) in
          Seq.cons (w, v) seq
      | None -> seq
    in
    loop [] t

  let search pfx t =
    let len = W.length pfx in
    let rec loop rev_pfx i t =
      if i = len then to_seq (of_rev_list rev_pfx) t
      else
        let (Node (children, _, _)) = t in
        let c = W.get pfx i in
        match M.find c children with
        | exception Not_found -> Seq.empty
        | t -> loop (c :: rev_pfx) (i + 1) t
    in
    loop [] 0 t

  let fuzzy_search ~max_dist pfx t =
    let module A = Automaton (struct
      type nonrec word = word

      let pattern = pfx
      let max_dist = max_dist
    end) in
    let rec loop rev_pfx t st =
      let (Node (children, _, _)) = t in
      if A.accept st then to_seq (of_rev_list rev_pfx) t
      else if A.can_match st then
        M.to_seq children
        |> Seq.map (fun (c, child) -> loop (c :: rev_pfx) child (A.next c st))
        |> Seq.concat
      else Seq.empty
    in
    loop [] t A.init

  let update w f t =
    let len = W.length w in
    let rec loop t i =
      let (Node (children, data, cardinal)) = t in
      if i = len then Node (children, f data, cardinal)
      else
        let children =
          M.update (W.get w i)
            (fun o ->
              match o with
              | Some child -> Some (loop child (i + 1))
              | None -> Some (loop empty (i + 1)))
            children
        in
        Node (children, data, cardinal + 1)
    in
    loop t 0

  let add w v t = update w (fun _ -> Some v) t

  let of_list l =
    let rec loop l acc =
      match l with [] -> acc | (w, v) :: l -> loop l (add w v acc)
    in
    loop l empty

  let remove w t =
    let len = W.length w in
    let rec loop t i =
      let (Node (children, data, cardinal)) = t in
      if i = len then Node (children, None, cardinal - 1)
      else
        let children =
          M.update (W.get w i)
            (fun o ->
              match o with
              | None -> None
              | Some child ->
                  let (Node (_, _, cardinal) as nc) = loop child (i + 1) in
                  if cardinal = 0 then None else Some nc)
            children
        in
        Node (children, data, cardinal - 1)
    in
    loop t 0

  let to_seq t = to_seq W.empty t

  let pp_human_size ppf i =
    let eucl u v = (u / v, u mod v) in
    let qm, rm = eucl i (1 lsl 20) in
    let qk, rk = eucl rm (1 lsl 10) in
    if qm > 0 then Fmt.pf ppf "%d Mio" qm
    else if qk > 0 then Fmt.pf ppf "%d Kio" rk
    else Fmt.pf ppf "%d o" rk

  let pp_statistics ppf (Node (_, _, cardinal) as t) =
    Fmt.pf ppf "%d words (size %a)" cardinal pp_human_size
      (Obj.reachable_words (Obj.repr t))
end

module Default = Make (Word.Default)
