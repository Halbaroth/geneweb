module Index = Geneweb_search.Index.Default
module A = Alcotest

let with_open file f =
  let ic = In_channel.open_text file in
  Fun.protect ~finally:(fun () -> close_in ic) (f ic)

let create_index file =
  with_open file @@ fun ic () ->
  let rec loop t =
    match In_channel.input_line ic with
    | None -> t
    | Some line -> loop (Index.insert line () t)
  in
  loop Index.empty

let count_lines file =
  with_open file @@ fun ic () ->
  let rec loop i =
    match In_channel.input_line ic with None -> i | Some _ -> loop (i + 1)
  in
  loop 0

let test_create_index file () =
  Alcotest.(check unit)
    "create an index from a file" ()
    (ignore (create_index file));
  let idx = create_index file in
  Alcotest.(check int)
    "check the cardinal of the index" (count_lines file) (Index.cardinal idx)

let test_cardinal () =
  let idx =
    Index.empty |> Index.insert "foo" () |> Index.insert "bar" ()
    |> Index.insert "saucisse" ()
  in
  A.(check int) "cardinal after insertion" 3 (Index.cardinal idx)

let test_lookup file () =
  let idx = create_index file in
  Fmt.pr "%a@."
    Fmt.(seq ~sep:sp @@ pair string nop)
    (Index.lookup "Vieux-Fort" idx);
  Alcotest.(check int)
    "lookup Vieux-Fort" 1
    (Seq.length @@ Index.lookup "Vieux-Fort" idx)

let () =
  match Array.to_list Sys.argv with
  | x :: dict :: xs ->
      let argv = Array.of_list (x :: xs) in
      A.run ~argv __FILE__
        [
          ( "basic operations",
            [
              A.test_case "create_index" `Quick (test_create_index dict);
              A.test_case "cardinal" `Quick test_cardinal;
              A.test_case "lookup" `Quick (test_lookup dict);
            ] );
        ]
  | _ -> failwith "expected a dictionary file in txt format as first argument"
