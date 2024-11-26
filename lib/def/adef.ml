(* $Id: adef.ml,v 5.6 2007-02-21 18:14:01 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

type fix = int

let float_of_fix x = float x /. 1000000.0
let fix_of_float x = truncate ((x *. 1000000.0) +. 0.5)

external fix : int -> fix = "%identity"
external fix_repr : fix -> int = "%identity"

let no_consang = fix (-1)

type precision =
  | Sure
  | About
  | Maybe
  | Before
  | After
  | OrYear of dmy2
  | YearInt of dmy2

and dmy = { day : int; month : int; year : int; prec : precision; delta : int }
and dmy2 = { day2 : int; month2 : int; year2 : int; delta2 : int }

let rec pp_precision ppf dmy =
  match dmy with
  | Sure -> Format.fprintf ppf "Sure"
  | About -> Format.fprintf ppf "About"
  | Maybe -> Format.fprintf ppf "Maybe"
  | Before -> Format.fprintf ppf "Before"
  | After -> Format.fprintf ppf "After"
  | OrYear dmy2 -> Format.fprintf ppf "OrYear(%a)" pp_dmy2 dmy2
  | YearInt dmy2 -> Format.fprintf ppf "YearInt(%a)" pp_dmy2 dmy2

and pp_dmy ppf { day; month; year; prec; delta } =
  Format.fprintf ppf
    "@[{ day = %i; month = %i; year = %i; prec = %a; delta = %i}@]" day month
    year pp_precision prec delta

and pp_dmy2 ppf { day2; month2; year2; delta2 } =
  Format.fprintf ppf "@[{ day2 = %i; month2 = %i; year2 = %i; delta2 = %i}@]"
    day2 month2 year2 delta2

type calendar = Dgregorian | Djulian | Dfrench | Dhebrew
type date = Dgreg of dmy * calendar | Dtext of string

type cdate =
  | Cgregorian of int
  | Cjulian of int
  | Cfrench of int
  | Chebrew of int
  | Ctext of string
  | Cdate of date
  | Cnone

type 'person gen_couple = { father : 'person; mother : 'person }
and 'person gen_parents = { parent : 'person array }

let father cpl =
  if Obj.size (Obj.repr cpl) = 2 then cpl.father else (Obj.magic cpl).parent.(0)

let mother cpl =
  if Obj.size (Obj.repr cpl) = 2 then cpl.mother else (Obj.magic cpl).parent.(1)

let couple father mother = { father; mother }
let parent parent = { father = parent.(0); mother = parent.(1) }

let parent_array cpl =
  if Obj.size (Obj.repr cpl) = 2 then [| cpl.father; cpl.mother |]
  else (Obj.magic cpl).parent

let multi_couple father mother : 'person gen_couple =
  Obj.magic { parent = [| father; mother |] }

let multi_parent parent : 'person gen_couple = Obj.magic { parent }

type 'a astring = string
type safe_string = [ `encoded | `escaped | `safe ] astring
type escaped_string = [ `encoded | `escaped ] astring
type encoded_string = [ `encoded ] astring

let ( ^^^ ) : 'a astring -> 'a astring -> 'a astring =
 fun (a : 'a astring) (b : 'a astring) : 'a astring -> a ^ b

let ( ^>^ ) : 'a astring -> string -> 'a astring =
 fun (a : 'a astring) (b : string) : 'a astring -> a ^ b

let ( ^<^ ) : string -> 'a astring -> 'a astring =
 fun (a : string) (b : 'a astring) : 'a astring -> a ^ b

let ( <^> ) : 'a astring -> 'a astring -> bool = ( <> )

external safe : string -> safe_string = "%identity"
external escaped : string -> escaped_string = "%identity"
external encoded : string -> encoded_string = "%identity"
external as_string : 'a astring -> string = "%identity"

let safe_fn = ( @@ )
