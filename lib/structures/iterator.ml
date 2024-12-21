module type S = sig
  type elt
  type t

  exception End

  val curr : t -> elt
  val next : t -> unit
  val seek : elt -> t -> unit
end

module Union (O : Intf.Ordered) (It : S with type elt = O.t) = struct
  type elt = O.t

  exception End

  module H = Heap.Make (struct
    type t = int * O.t

    let dummy = (0, O.dummy)
    let compare (_, v1) (_, v2) = O.compare v1 v2
  end)

  type t = { hp : H.t; arr : It.t array }

  let union l =
    let arr = Array.of_list l in
    let len = Array.length arr in
    let hp = H.create 256 in
    for i = 0 to len - 1 do
      match It.curr arr.(i) with
      | exception It.End -> ()
      | v -> H.insert hp (i, v)
    done;
    { hp; arr }

  let curr { hp; _ } =
    match H.min hp with exception H.Empty -> raise End | _, v -> v

  let next { hp; arr } =
    match H.delete_min hp with
    | exception H.Empty -> ()
    | i, _ -> (
        It.next arr.(i);
        match It.curr arr.(i) with
        | exception It.End -> ()
        | v -> H.insert hp (i, v))

  let seek w { hp; arr } =
    let rec loop () =
      match H.min hp with
      | exception H.Empty -> ()
      | _, v when O.compare w v <= 0 -> ()
      | i, _ ->
          let (_ : int * O.t) = H.delete_min hp in
          It.seek w arr.(i);
          let () =
            match It.curr arr.(i) with
            | exception It.End -> ()
            | v -> H.insert hp (i, v)
          in
          loop ()
    in
    loop ()
end

module Join (O : Intf.Ordered) (It : S with type elt = O.t) = struct
  type elt = O.t
  type t = { arr : It.t array; mutable ended : bool }

  exception End

  let[@inline always] curr t = if t.ended then raise End else It.curr t.arr.(0)
  let[@inline always] omin u v = if O.compare u v < 0 then u else v
  let[@inline always] omax u v = if O.compare u v > 0 then u else v

  let seek w t =
    let rec loop w =
      let v = It.curr t.arr.(0) in
      let mi, ma =
        Array.fold_left
          (fun (mi, ma) it ->
            It.seek w it;
            let v = It.curr it in
            (omin mi v, omax ma v))
          (v, v) t.arr
      in
      if mi < ma then loop ma
    in
    if not t.ended then try loop w with It.End -> t.ended <- true

  let join l =
    let arr = Array.of_list l in
    if Array.length arr = 0 then invalid_arg "join";
    match It.curr arr.(0) with
    | exception It.End -> { arr; ended = true }
    | w ->
        let t = { arr; ended = false } in
        seek w t;
        t

  let next t =
    if not t.ended then
      try
        let v = It.curr t.arr.(0) in
        (* Advances each iterator by one step and return the maximum value. *)
        let max =
          Array.fold_left
            (fun ma it ->
              It.next it;
              let v = It.curr it in
              omax ma v)
            v t.arr
        in
        seek max t
      with It.End -> t.ended <- true
end

let to_seq (type a b) (module It : S with type elt = a and type t = b) (t : b) =
  let rec loop () =
    match It.curr t with exception It.End -> Seq.Nil | v -> Seq.Cons (v, loop)
  in
  loop
