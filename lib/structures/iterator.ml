exception End

type ('a, 'cmp) t =
  < comparator : ('a, 'cmp) Comparator.t
  ; curr : unit -> 'a
  ; next : unit -> unit
  ; seek : 'a -> unit >

let equal (type a w) (it1 : (a, w) t) (it2 : (a, w) t) =
  let module C = (val it1#comparator : Comparator.S
                    with type t = a
                     and type witness = w)
  in
  let rec loop () =
    let v1 = try Some (it1#curr ()) with End -> None in
    let v2 = try Some (it2#curr ()) with End -> None in
    match (v1, v2) with
    | Some v1, Some v2 ->
        if C.compare v1 v2 <> 0 then false
        else (
          it1#next ();
          it2#next ();
          loop ())
    | None, Some _ | Some _, None -> false
    | None, None -> true
  in
  loop ()

let union (type a w) (l : (a, w) t list) =
  let arr = Array.of_list l in
  if Array.length arr = 0 then invalid_arg "union";
  let module C = (val arr.(0)#comparator : Comparator.S
                    with type t = a
                     and type witness = w)
  in
  let module H = Heap.Make (struct
    type t = int * a
    type witness = C.witness

    let dummy = (0, C.dummy)
    let compare (_, v1) (_, v2) = C.compare v1 v2
  end) in
  let len = Array.length arr in
  object
    val hp = H.create len
    method comparator = arr.(0)#comparator

    method seek w =
      let rec loop () =
        match H.min hp with
        | exception H.Empty -> ()
        | _, v when C.compare w v <= 0 -> ()
        | i, _ ->
            let (_ : int * a) = H.delete_min hp in
            arr.(i)#seek w;
            let () =
              match arr.(i)#curr () with
              | exception End -> ()
              | v -> H.insert hp (i, v)
            in
            loop ()
      in
      loop ()

    method next () =
      match H.delete_min hp with
      | exception H.Empty -> ()
      | i, _ -> (
          arr.(i)#next ();
          match arr.(i)#curr () with
          | exception End -> ()
          | v -> H.insert hp (i, v))

    method curr () =
      match H.min hp with exception H.Empty -> raise End | _, v -> v

    initializer
    for i = 0 to len - 1 do
      match arr.(i)#curr () with exception End -> () | v -> H.insert hp (i, v)
    done
  end

let join (type a w) (l : (a, w) t list) =
  let arr = Array.of_list l in
  if Array.length arr = 0 then invalid_arg "join";
  let module C = (val arr.(0)#comparator : Comparator.S
                    with type t = a
                     and type witness = w)
  in
  object (self)
    val mutable ended = false
    val mutable pos = 0
    method comparator = arr.(0)#comparator

    method private search () =
      let k = Array.length arr in
      let rec loop x =
        let y = arr.(pos)#curr () in
        if C.compare x y <> 0 then (
          arr.(pos)#seek x;
          match arr.(pos)#curr () with
          | exception End -> ended <- true
          | x ->
              pos <- (pos + 1) mod k;
              loop x)
      in
      loop (arr.((k + pos - 1) mod k)#curr ())

    method seek w =
      arr.(pos)#seek w;
      match arr.(pos)#curr () with
      | exception End -> ended <- true
      | _ ->
          let k = Array.length arr in
          pos <- (pos + 1) mod k;
          self#search ()

    method next () =
      arr.(pos)#next ();
      match arr.(pos)#curr () with
      | exception End -> ended <- true
      | _ ->
          let k = Array.length arr in
          pos <- (pos + 1) mod k;
          self#search ()

    method curr () = if ended then raise End else arr.(0)#curr ()

    initializer
    try
      Array.sort (fun it1 it2 -> C.compare (it1#curr ()) (it2#curr ())) arr;
      self#search ()
    with End -> ended <- true
  end

let to_seq it =
  let rec loop () =
    match it#curr () with
    | exception End -> Seq.Nil
    | v ->
        it#next ();
        Seq.Cons (v, loop)
  in
  loop
