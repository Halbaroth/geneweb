let bname = ref ""
let trace = ref false
let fnames = ref false
let snames = ref false
let alias = ref false
let pub_names = ref false
let fname_aliases = ref false
let sname_aliases = ref false
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

let iteri_places f base =
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

let iteri_pers f base =
  Gwdb.Collection.iteri
    (fun i iper -> f i (Gwdb.poi base iper))
    (Gwdb.ipers base)

(* FIXME: find the good heuristic *)
let places_all base bname fname =
  let len = Gwdb.nb_of_persons base + Gwdb.nb_of_families base in
  if !prog then (
    Printf.printf "\nplaces@.";
    ProgrBar.full := '*';
    ProgrBar.start ());

  let set : unit HT.t = HT.create 2048 in
  iteri_places
    (fun i istr ->
      if !prog && i mod 50 = 0 then ProgrBar.run i len;
      HT.add set istr ())
    base;

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

let iter_field base p f = function
  | `Fnames -> f (Gwdb.get_first_name p)
  | `Snames -> f (Gwdb.get_surname p)
  | `Fname_aliases -> List.iter f (Gwdb.get_first_names_aliases p)
  | `Sname_aliases -> List.iter f (Gwdb.get_surnames_aliases p)
  | `Aliases -> List.iter f (Gwdb.get_aliases p)
  | `Occupations -> f (Gwdb.get_occupation p)
  | `Qualifiers -> List.iter f (Gwdb.get_qualifiers p)
  | `Pub_names -> f (Gwdb.get_public_name p)
  | `Estates -> List.iter (fun t -> f t.Def.t_place) (Gwdb.get_titles p)
  | `Titles -> List.iter (fun t -> f t.Def.t_ident) (Gwdb.get_titles p)
  | `Sources ->
      f (Gwdb.get_psources p);
      List.iter (fun t -> f t.Def.epers_src) (Gwdb.get_pevents p);
      Array.iter
        (fun ifam ->
          List.iter
            (fun evt -> f evt.Def.efam_src)
            (Gwdb.get_fevents (Gwdb.foi base ifam)))
        (Gwdb.get_family p)

let field_to_string = function
  | `Fnames -> "fnames"
  | `Snames -> "snames"
  | `Fname_aliases -> "fname_aliases"
  | `Sname_aliases -> "Sname_aliases"
  | `Aliases -> "aliases"
  | `Occupations -> "occupations"
  | `Qualifiers -> "qualifiers"
  | `Pub_names -> "pub_names"
  | `Estates -> "estates"
  | `Titles -> "titles"
  | `Sources -> "sources"

let names_all base bname field =
  let len = Gwdb.nb_of_persons base in
  let fname = field_to_string field in
  if !prog then (
    Printf.eprintf "\n%s@." fname;
    ProgrBar.full := '*';
    ProgrBar.start ());

  let set : unit HT.t = HT.create 17 in

  iteri_pers
    (fun i p ->
      if !prog && i mod 50 = 0 then ProgrBar.run i len;
      iter_field base p (fun istr -> HT.add set istr ()) field)
    base;

  if !prog then ProgrBar.finish ();

  let name_list =
    HT.fold (fun k () acc -> Gwdb.sou base k :: acc) set []
    |> List.sort Gutil.alphabetic_utf_8
  in
  write_cache_file bname fname name_list;
  Format.printf "@[<h>%-*s@ %8d@ %-14s@]@." !width (fullname bname fname)
    (List.length name_list) fname

let names_all base bname field =
  with_timer @@ fun () -> names_all base bname field

let speclist =
  [
    ("-bd", Arg.String Secure.set_base_dir, " bases folder");
    ("-fn", Arg.Set fnames, " first names");
    ("-sn", Arg.Set snames, " surnames");
    ("-al", Arg.Set alias, " aliases");
    ("-pu", Arg.Set pub_names, " public names");
    ("-fna", Arg.Set fname_aliases, " add first name aliases");
    ("-sna", Arg.Set sname_aliases, " add surname aliases");
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

  let fds = [] in
  let fds = if !all || !fnames then `Fnames :: fds else fds in
  let fds = if !all || !snames then `Fnames :: fds else fds in
  let fds = if !all || !fname_aliases then `Fname_aliases :: fds else fds in
  let fds = if !all || !sname_aliases then `Sname_aliases :: fds else fds in
  let fds = if !all || !pub_names then `Pub_names :: fds else fds in
  let fds = if !all || !estates then `Estates :: fds else fds in
  let fds = if !all || !titles then `Titles :: fds else fds in
  let fds = if !all || !occupations then `Occupations :: fds else fds in
  let fds = if !all || !qualifiers then `Qualifiers :: fds else fds in
  let fds = if !all || !sources then `Sources :: fds else fds in

  if !places then places_all base !bname "places";
  List.iter (names_all base !bname) fds
