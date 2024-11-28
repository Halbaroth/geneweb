module type S = sig
  type 'a t
  type char_
  type word

  val empty : 'a t
  val cardinal : 'a t -> int
  val lookup : word -> 'a t -> (word * 'a) list
  val insert : word -> 'a -> 'a t -> 'a t
  val remove : word -> 'a t -> 'a t
  val iter : (word -> 'a -> unit) -> 'a t -> unit
  val pp : 'a Fmt.t -> 'a t Fmt.t
end

module Make (W : Word.S) = struct
  module M = Map.Make (struct
    type t = W.char_

    let compare = W.compare_char
  end)

  type char_ = W.char_
  type word = W.t
  type 'a t = { data : 'a option; children : 'a t M.t; cardinal : int }

  let empty = { data = None; children = M.empty; cardinal = 0 }
  let[@inline always] cardinal { cardinal; _ } = cardinal

  (* Return the content of the tries in lexicographic order.
     TODO: this function is really inefficient because we do not want
     to generate all the list sometimes and we use plenty of list
     concatenations! *)
  let rec to_list t =
    let acc = match t.data with Some v -> [ (W.empty, v) ] | None -> [] in
    M.fold
      (fun c tc acc ->
        let u =
          List.map
            (fun (s, v) ->
              let s = W.cat (W.of_list [ c ]) s in
              (s, v))
            (to_list tc)
        in
        acc @ u)
      t.children acc

  let iter f t =
    let rec loop stack =
      match stack with
      | [] -> ()
      | (rev_pfx, t) :: stack ->
          let () =
            match t.data with
            | Some v -> f (List.rev rev_pfx |> W.of_list) v
            | None -> ()
          in
          M.fold
            (fun c tc stack -> (c :: rev_pfx, tc) :: stack)
            t.children stack
          |> loop
    in
    loop [ ([], t) ]

  let lookup pattern t =
    let len = W.length pattern in
    let rec loop prefix t i =
      if i = len then
        let p = W.of_list @@ List.rev prefix in
        List.map (fun (s, v) -> (W.cat p s, v)) (to_list t)
      else
        let c = W.get pattern i in
        match M.find c t.children with
        | exception Not_found -> []
        | t -> loop (c :: prefix) t (i + 1)
    in
    loop [] t 0

  let insert s v t =
    let len = W.length s in
    let rec loop t i =
      if i >= len then { t with data = Some v }
      else
        let children =
          M.update (W.get s i)
            (fun o ->
              match o with
              | Some children -> Some (loop children (i + 1))
              | None -> Some (loop empty (i + 1)))
            t.children
        in
        { t with children; cardinal = t.cardinal + 1 }
    in
    loop t 0

  let remove s t =
    let len = W.length s in
    let rec loop t i =
      if i >= len then { t with data = None; cardinal = t.cardinal - 1 }
      else
        let children =
          M.update (W.get s i)
            (fun o ->
              match o with
              | None -> None
              | Some children ->
                  let nc = loop children (i + 1) in
                  if nc.cardinal = 0 then None else Some nc)
            t.children
        in
        { t with children; cardinal = t.cardinal - 1 }
    in
    loop t 0

  let pp pp_val =
    Fmt.box
    @@ Fmt.iter_bindings ~sep:Fmt.sp iter
    @@ Fmt.parens
    @@ Fmt.pair ~sep:Fmt.comma W.pp pp_val
end

module Default = Make (Word.Default)
