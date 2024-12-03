let bname = ref ""
let trace = ref false
let fnames = ref false
let snames = ref false
let alias = ref false
let pub_names = ref false
let fname_alias = ref false
let sname_alias = ref false
let places = ref false
let estates = ref false
let titles = ref false
let occupations = ref false
let qualifiers = ref false
let sources = ref false
let all = ref false
let prog = ref false
let width = ref 50
let cache_dir = ref ""
let ( // ) = Filename.concat

(* Attention: cache files are reorg independant *)
let set_cache_dir bname =
  let cache_dir = Secure.base_dir () // "etc" // bname // "cache" in
  File.create_dir ~required_perm:0o755 cache_dir;
  cache_dir

let write_cache_file bname fname l =
  let filename = bname ^ "_" ^ fname ^ ".cache" in
  let file = !cache_dir // filename in
  let gz_file = file ^ ".gz" in
  let oc = Gzip.open_out gz_file in
  Fun.protect ~finally:(fun () -> Gzip.close_out oc) @@ fun () ->
  List.iter
    (fun s ->
      let s = s ^ "\n" in
      Gzip.output_substring oc s 0 (String.length s))
    l

let with_timer f =
  let start = Unix.gettimeofday () in
  let r = f () in
  let stop = Unix.gettimeofday () in
  Format.printf "%6.2f s" (stop -. start);
  r

module HT = struct
  include Hashtbl.Make (struct
    type t = Gwdb_driver.istr

    let equal = Gwdb_driver.eq_istr
    let hash = Gwdb_driver.hash_istr
  end)

  let add s i v = if not @@ Gwdb_driver.is_empty_string i then add s i v
end

let fullname bname fname = !cache_dir // (bname ^ "_" ^ fname ^ ".cache.gz")

let iter_places f base =
  let ipers = Gwdb.ipers base in
  let ifams = Gwdb.ifams base in
  Gwdb.Collection.iteri
    (fun i iper ->
      let per = Gwdb.poi base iper in
      f i (Gwdb.get_birth_place per);
      f i (Gwdb.get_baptism_place per);
      f i (Gwdb.get_death_place per);
      f i (Gwdb.get_burial_place per))
    ipers;
  Gwdb.Collection.iteri
    (fun i ifam ->
      let fam = Gwdb.foi base ifam in
      f i (Gwdb.get_marriage_place fam))
    ifams

let iter_names f base = Gwdb.Collection.iteri f (Gwdb.ipers base)

(* FIXME: find the good heuristic *)
let places_all base bname fname =
  let len = Gwdb.nb_of_persons base + Gwdb.nb_of_families base in

  if !prog then (
    Printf.eprintf "\nplaces\n";
    flush stdout;
    ProgrBar.full := '*';
    ProgrBar.start ());

  let set : unit HT.t = HT.create 2048 in
  let add_place i istr =
    if !prog && i mod 50 = 0 then ProgrBar.run i len;
    HT.add set istr ()
  in
  iter_places add_place base;

  if !prog then ProgrBar.finish ();

  let places_list =
    HT.fold (fun k () acc -> Gwdb.sou base k :: acc) set []
    |> List.sort Gutil.alphabetic_utf_8
  in
  write_cache_file bname fname places_list;

  Format.printf "@[<h>%-*s@ %8d@ %-14s@@]@," !width (fullname bname fname)
    (List.length places_list) "places"

let places_all base bname fname =
  with_timer @@ fun () -> places_all base bname fname

let names_all base bname fname alias =
  let ht : (string, string) Hashtbl.t = Hashtbl.create 17 in
  let nb_ind = Gwdb.nb_of_persons base in
  flush stderr;
  if !prog then (
    Printf.eprintf "\n%s\n" fname;
    flush stdout;
    ProgrBar.full := '*';
    ProgrBar.start ());

  Gwdb.Collection.iteri
    (fun i ip ->
      if !prog then ProgrBar.run i nb_ind;
      let p = Gwdb.poi base ip in
      let nam =
        match fname with
        | "fnames" -> [ Gwdb.get_first_name p ]
        | "snames" -> [ Gwdb.get_surname p ]
        | "aliases" -> Gwdb.get_aliases p
        | "occupations" -> [ Gwdb.get_occupation p ]
        | "qualifiers" -> Gwdb.get_qualifiers p
        | "pub_names" -> [ Gwdb.get_public_name p ]
        | "estates" ->
            List.fold_left
              (fun acc t -> t.Def.t_place :: acc)
              [] (Gwdb.get_titles p)
        | "titles" ->
            List.fold_left
              (fun acc t -> t.Def.t_ident :: acc)
              [] (Gwdb.get_titles p)
        | "sources" ->
            let p_sources =
              List.fold_right
                (fun evt events ->
                  let src = evt.Def.epers_src in
                  src :: events)
                (Gwdb.get_pevents p)
                [ Gwdb.get_psources p ]
            in
            let ifams = Array.to_list (Gwdb.get_family p) in
            let f_sources =
              List.fold_left
                (fun acc ifam ->
                  List.fold_right
                    (fun evt fam_fevents ->
                      let src = evt.Def.efam_src in
                      src :: fam_fevents)
                    (Gwdb.get_fevents (Gwdb.foi base ifam))
                    []
                  :: acc)
                [] ifams
            in
            p_sources @ List.flatten f_sources
        | _ -> []
      in
      List.iter
        (fun nam ->
          let key = Gwdb.sou base nam in
          if not (Hashtbl.mem ht key) then Hashtbl.add ht key key
          else
            let vv = Hashtbl.find ht key in
            Hashtbl.replace ht key vv)
        nam;

      let nam2 =
        match (fname, alias) with
        | "fnames", "fna" -> Gwdb.get_first_names_aliases p
        | "snames", "sna" -> Gwdb.get_surnames_aliases p
        | _, _ -> []
      in
      List.iter
        (fun nam ->
          let key = Gwdb.sou base nam in
          if not (Hashtbl.mem ht key) then Hashtbl.add ht key key
          else
            let vv = Hashtbl.find ht key in
            Hashtbl.replace ht key vv)
        nam2)
    (Gwdb.ipers base);

  if !prog then ProgrBar.finish ();
  let name_list = Hashtbl.fold (fun _k v acc -> v :: acc) ht [] in
  let name_list = List.sort (fun v1 v2 -> compare v1 v2) name_list in
  write_cache_file bname fname name_list;
  Format.printf "@[<h>%-*s@ %8d@ %-14s@]@." !width (fullname bname fname)
    (List.length name_list) fname

let names_all base bname fname alias =
  with_timer @@ fun () -> names_all base bname fname alias

let speclist =
  [
    ("-bd", Arg.String Secure.set_base_dir, " bases folder");
    ("-fn", Arg.Set fnames, " first names");
    ("-sn", Arg.Set snames, " surnames");
    ("-al", Arg.Set alias, " aliases");
    ("-pu", Arg.Set pub_names, " public names");
    ("-fna", Arg.Set fname_alias, " add first name aliases");
    ("-sna", Arg.Set sname_alias, " add surname aliases");
    ("-qu", Arg.Set qualifiers, " qualifiers");
    ("-pl", Arg.Set places, " places");
    ("-es", Arg.Set estates, " estates");
    ("-ti", Arg.Set titles, " titles");
    ("-oc", Arg.Set occupations, " occupations");
    ("-so", Arg.Set sources, " sources");
    ("-all", Arg.Set all, " all");
    ("-prog", Arg.Set prog, " show progress bar");
  ]
  |> List.sort compare |> Arg.align

let anonfun i = bname := i

let usage =
  "Usage: cache_files [options] base\n cd bases; before running cache_files."

let () =
  Secure.set_base_dir ".";
  Arg.parse speclist anonfun usage;
  bname := Filename.remove_extension (Filename.basename !bname);
  if !bname = "" || !bname <> Filename.basename !bname then (
    Arg.usage speclist usage;
    exit 2);
  let base = Gwdb.open_base !bname in
  bname := Filename.basename !bname;
  cache_dir := set_cache_dir !bname;
  if not (Sys.file_exists !cache_dir) then Mutil.mkdir_p !cache_dir;
  Printf.printf "Generating cache(s) compressed with gzip\n";

  let full_name = !cache_dir // (!bname ^ "_occupations.cache.gz") in
  width := String.length full_name + 2;
  Format.printf "@[<v>";
  let fn_alias = if !fname_alias then "fna" else "" in
  let sn_alias = if !sname_alias then "sna" else "" in
  (* Ouvre une boite verticale *)
  if !places then places_all base !bname "places";
  if !fnames then names_all base !bname "fnames" fn_alias;
  if !snames then names_all base !bname "snames" sn_alias;
  if !alias then names_all base !bname "aliases" "";
  if !pub_names then names_all base !bname "pub_names" "";
  if !estates then names_all base !bname "estates" "";
  if !titles then names_all base !bname "titles" "";
  if !occupations then names_all base !bname "occupations" "";
  if !sources then names_all base !bname "sources" "";
  if !qualifiers then names_all base !bname "qualifiers" "";
  if !all then (
    let fn_alias = "fna" in
    let sn_alias = "sna" in
    places_all base !bname "places";
    names_all base !bname "fnames" fn_alias;
    names_all base !bname "snames" sn_alias;
    names_all base !bname "aliases" "";
    names_all base !bname "pub_names" "";
    names_all base !bname "estates" "";
    names_all base !bname "titles" "";
    names_all base !bname "occupations" "";
    names_all base !bname "sources" "";
    names_all base !bname "qualifiers" "");
  Format.printf "@]";
  (* Ferme la boite verticale *)
  flush stderr
