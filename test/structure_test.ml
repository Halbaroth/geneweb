module A = Alcotest

module I = struct
  include Int

  let dummy = 0
end

module Iset = Geneweb_structures.Iset.Make (I)
module Union = Geneweb_structures.Iterator.Union (I) (Iset.Iterator)
module Join = Geneweb_structures.Iterator.Join (I) (Iset.Iterator)

module Naive = struct
  module SI = Set.Make (Int)

  type elt = int
  type t = SI.t

  let of_seq = SI.of_seq
  let to_seq = SI.to_seq
  let mem = SI.mem
  let cardinal = SI.cardinal
  let empty = SI.empty
  let inter = SI.inter
  let union = SI.union

  module Iterator = struct
    type elt = int
    type t = int Seq.t ref

    exception End

    let curr t =
      match !t () with Seq.Nil -> raise End | Seq.Cons (hd, _) -> hd

    let next t = match !t () with Seq.Nil -> () | Seq.Cons (_, tl) -> t := tl

    let seek w t =
      let rec loop () =
        match curr t with
        | exception End -> ()
        | v when w <= v -> ()
        | _ ->
            next t;
            loop ()
      in
      loop ()
  end

  let iterator t = ref (to_seq t)
end

module type S = Geneweb_structures.Iterator.S

let equal_iterator (type t1 t2)
    (module It1 : S with type elt = int and type t = t1) (it1 : t1)
    (module It2 : S with type elt = int and type t = t2) (it2 : t2) =
  let rec loop () =
    let v1 = try Some (It1.curr it1) with It1.End -> None in
    let v2 = try Some (It2.curr it2) with It2.End -> None in
    match (v1, v2) with
    | Some v1, Some v2 ->
        if not @@ Int.equal v1 v2 then false
        else (
          It1.next it1;
          It2.next it2;
          loop ())
    | None, Some _ | Some _, None -> false
    | None, None -> true
  in
  loop ()

let nonempty_array = QCheck.Gen.(array_size (int_range 1 100) int)

let index_array =
  QCheck.Gen.(
    nonempty_array >>= fun a ->
    int_range 0 (Array.length a - 1) >>= fun i -> pure (a, i))

let test_empty () =
  let s = Iset.of_seq Seq.empty in
  let it = Iset.iterator s in
  Iset.Iterator.next it;
  Iset.Iterator.seek 10 it;
  let b =
    match Iset.Iterator.curr it with
    | exception Iset.Iterator.End -> true
    | _ -> false
  in
  A.(check bool) "end iterator" true b

let test_seek_advance () =
  let s = Iset.of_seq (List.to_seq [ 1; 3; 5; 9 ]) in
  let it = Iset.iterator s in
  Iset.Iterator.seek 4 it;
  A.(check int) "first seek" 5 (Iset.Iterator.curr it);
  Iset.Iterator.seek 4 it;
  A.(check int) "second seek" 5 (Iset.Iterator.curr it)

let test_random_mem =
  QCheck.Test.make ~count:1000 ~name:"random mem" (QCheck.make index_array)
  @@ fun (a, i) ->
  let seq = Array.to_seq a in
  let s1 = Naive.of_seq seq in
  let s2 = Iset.of_seq seq in
  Naive.mem a.(i) s1 = Iset.mem a.(i) s2

let test_random_iterator_next =
  QCheck.Test.make ~count:1000 ~name:"random iterator next"
    (QCheck.make nonempty_array)
  @@ fun a ->
  let seq = Array.to_seq a in
  let it1 = Naive.iterator @@ Naive.of_seq seq in
  let it2 = Iset.iterator @@ Iset.of_seq seq in
  equal_iterator (module Naive.Iterator) it1 (module Iset.Iterator) it2

let test_random_iterator_seek =
  QCheck.Test.make ~count:1000 ~name:"random iterator seek"
    (QCheck.make index_array)
  @@ fun (a, i) ->
  let seq = Array.to_seq a in
  let it1 = Naive.iterator @@ Naive.of_seq seq in
  let it2 = Iset.iterator @@ Iset.of_seq seq in
  let probe = a.(i) + 5 in
  Naive.Iterator.seek probe it1;
  Iset.Iterator.seek probe it2;
  let v1 =
    try Some (Naive.Iterator.curr it1) with Naive.Iterator.End -> None
  in
  let v2 = try Some (Iset.Iterator.curr it2) with Iset.Iterator.End -> None in
  Option.equal Int.equal v1 v2

let test_random_iterator_union =
  QCheck.Test.make ~count:1000 ~name:"random iterator union"
    QCheck.(make Gen.(list_size (int_range 1 100) nonempty_array))
  @@ fun l ->
  let it1 =
    let l1 = List.map (fun a -> Array.to_seq a |> Naive.of_seq) l in
    List.fold_left Naive.union Naive.empty l1 |> Naive.iterator
  in
  let it2 =
    let l2 =
      List.map (fun a -> Array.to_seq a |> Iset.of_seq |> Iset.iterator) l
    in
    Union.union l2
  in
  equal_iterator (module Naive.Iterator) it1 (module Union) it2

let test_random_iterator_join =
  QCheck.Test.make ~count:1000 ~name:"random iterator join"
    QCheck.(make Gen.(list_size (int_range 1 100) nonempty_array))
  @@ fun l ->
  let it1 =
    let l1 = List.map (fun a -> Array.to_seq a |> Naive.of_seq) l in
    let x = List.hd l1 in
    List.fold_left Naive.inter x l1 |> Naive.iterator
  in
  let it2 =
    let l2 =
      List.map (fun a -> Array.to_seq a |> Iset.of_seq |> Iset.iterator) l
    in
    Join.join l2
  in
  equal_iterator (module Naive.Iterator) it1 (module Join) it2

let () =
  let quick_test s = A.test_case s `Quick in
  let qcheck_test = QCheck_alcotest.to_alcotest in
  A.run __FILE__
    [
      ( "corner cases",
        [
          quick_test "empty" test_empty;
          quick_test "seek advance" test_seek_advance;
        ] );
      ( "random set operations",
        [
          qcheck_test test_random_mem;
          qcheck_test test_random_iterator_next;
          qcheck_test test_random_iterator_seek;
          qcheck_test test_random_iterator_union;
          qcheck_test test_random_iterator_join;
        ] );
    ]
