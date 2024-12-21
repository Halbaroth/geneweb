exception End

module type S = sig
  type elt
  type cmp

  val curr : unit -> elt
  val next : unit -> unit
  val seek : elt -> unit
end

type ('a, 'cmp) t = (module S with type elt = 'a and type cmp = 'cmp)

let curr (type a w) (module It : S with type elt = a and type cmp = w) =
  It.curr ()

let next (type a w) (module It : S with type elt = a and type cmp = w) =
  It.next ()

let seek (type a w) (e : a) (module It : S with type elt = a and type cmp = w) =
  It.seek e

let equal (type a w)
    (module C : Comparator.S with type elt = a and type witness = w)
    (module It1 : S with type elt = a and type cmp = w)
    (module It2 : S with type elt = a and type cmp = w) =
  let rec loop () =
    let v1 = try Some (It1.curr ()) with End -> None in
    let v2 = try Some (It2.curr ()) with End -> None in
    match (v1, v2) with
    | Some v1, Some v2 ->
        if C.compare v1 v2 <> 0 then false
        else (
          It1.next ();
          It2.next ();
          loop ())
    | None, Some _ | Some _, None -> false
    | None, None -> true
  in
  loop ()

let union (type a w)
    (module C : Comparator.S with type elt = a and type witness = w)
    (l : (a, w) t list) =
  (module struct
    type elt = a
    type cmp = w

    module H = Heap.Make (struct
      type elt = int * a
      type witness = C.witness

      let dummy = (0, C.dummy)
      let compare (_, v1) (_, v2) = C.compare v1 v2
    end)

    type nonrec t = { hp : H.t; arr : (a, w) t array }

    let state =
      let arr = Array.of_list l in
      let len = Array.length arr in
      let hp = H.create 256 in
      for i = 0 to len - 1 do
        match curr arr.(i) with exception End -> () | v -> H.insert hp (i, v)
      done;
      { hp; arr }

    let seek w =
      let rec loop () =
        match H.min state.hp with
        | exception H.Empty -> ()
        | _, v when C.compare w v <= 0 -> ()
        | i, _ ->
            let (_ : int * a) = H.delete_min state.hp in
            seek w state.arr.(i);
            let () =
              match curr state.arr.(i) with
              | exception End -> ()
              | v -> H.insert state.hp (i, v)
            in
            loop ()
      in
      loop ()

    let next () =
      match H.delete_min state.hp with
      | exception H.Empty -> ()
      | i, _ -> (
          next state.arr.(i);
          match curr state.arr.(i) with
          | exception End -> ()
          | v -> H.insert state.hp (i, v))

    let curr () =
      match H.min state.hp with exception H.Empty -> raise End | _, v -> v
  end : S
    with type elt = a
     and type cmp = w)

let join (type a w)
    (module C : Comparator.S with type elt = a and type witness = w)
    (l : (a, w) t list) =
  let arr = Array.of_list l in
  if Array.length arr = 0 then invalid_arg "join";
  (module struct
    type elt = a
    type cmp = w

    exception End

    type s = { arr : (a, w) t array; mutable ended : bool }

    let[@inline always] omin u v = if C.compare u v < 0 then u else v
    let[@inline always] omax u v = if C.compare u v > 0 then u else v

    let seek w t =
      let rec loop w =
        let module It = (val t.arr.(0)) in
        let v = It.curr () in
        let mi, ma =
          Array.fold_left
            (fun (mi, ma) (it : (a, w) t) ->
              let module It = (val it) in
              It.seek w;
              let v = It.curr () in
              (omin mi v, omax ma v))
            (v, v) t.arr
        in
        if mi < ma then loop ma
      in
      if not t.ended then try loop w with End -> t.ended <- true

    let state =
      match curr arr.(0) with
      | exception End -> { arr; ended = true }
      | w ->
          let t = { arr; ended = false } in
          seek w t;
          t

    let seek w = seek w state

    let[@inline always] curr () =
      if state.ended then raise End
      else
        let module It = (val state.arr.(0)) in
        It.curr ()

    let next () =
      if not state.ended then
        try
          let module It = (val state.arr.(0)) in
          let v = It.curr () in
          (* Advances each iterator by one step and return the maximum value. *)
          let max =
            Array.fold_left
              (fun ma (it : (a, w) t) ->
                let module It = (val it) in
                It.next ();
                let v = It.curr () in
                omax ma v)
              v state.arr
          in
          seek max
        with End -> state.ended <- true
  end : S
    with type elt = a
     and type cmp = w)

let to_seq (type a w) (module It : S with type elt = a and type cmp = w) =
  let rec loop () =
    match It.curr () with exception End -> Seq.Nil | v -> Seq.Cons (v, loop)
  in
  loop
