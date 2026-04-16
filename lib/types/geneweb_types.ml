module Y = Yojson.Safe
module U = Yojson.Safe.Util

type (_, _) eq = Equal : ('a, 'a) eq
type _ id = ..
type error = [ `Json_error of string | `Invalid_type of string ]

let pp_error ppf = function
  | `Json_error e -> Fmt.string ppf e
  | `Invalid_type e -> Fmt.string ppf e

module type Generic = sig
  type t
  type _ id += Id : t id

  val name : string
  val compare : t -> t -> int
  val of_json : Yojson.Safe.t -> (t, error) result
  val to_json : t -> Yojson.Safe.t
  val pp : t Fmt.t
end

type 'a generic = (module Generic with type t = 'a)

type 'a t =
  | Unit : unit t
  | Bool : bool t
  | Float : float t
  | Int : int t
  | String : string t
  | List : 'a t -> 'a list t
  | Array : 'a t -> 'a array t
  | Option : 'a t -> 'a option t
  | Tuple2 : 'a t * 'b t -> ('a * 'b) t
  | Tuple3 : 'a t * 'b t * 'c t -> ('a * 'b * 'c) t
  | Tuple4 : 'a t * 'b t * 'c t * 'd t -> ('a * 'b * 'c * 'd) t
  | Generic : 'a generic -> 'a t

let make (type a) ~name ~compare ~of_json ~to_json ~pp () : a t =
  Generic
    (module struct
      type t = a
      type _ id += Id : t id

      let name = name
      let compare = compare
      let of_json = of_json
      let to_json = to_json
      let pp = pp
    end)

let rec equal : type a b. a t -> b t -> (a, b) eq option =
 fun w1 w2 ->
  match (w1, w2) with
  | Unit, Unit -> Some Equal
  | Bool, Bool -> Some Equal
  | Float, Float -> Some Equal
  | Int, Int -> Some Equal
  | String, String -> Some Equal
  | List w1, List w2 -> (
      match equal w1 w2 with Some Equal -> Some Equal | None -> None)
  | Array w1, Array w2 -> (
      match equal w1 w2 with Some Equal -> Some Equal | None -> None)
  | Option w1, Option w2 -> (
      match equal w1 w2 with Some Equal -> Some Equal | None -> None)
  | Tuple2 (w1, w2), Tuple2 (w3, w4) -> (
      match (equal w1 w3, equal w2 w4) with
      | Some Equal, Some Equal -> Some Equal
      | _ -> None)
  | Tuple3 (w1, w2, w3), Tuple3 (z1, z2, z3) -> (
      match (equal w1 z1, equal w2 z2, equal w3 z3) with
      | Some Equal, Some Equal, Some Equal -> Some Equal
      | _ -> None)
  | Tuple4 (w1, w2, w3, w4), Tuple4 (z1, z2, z3, z4) -> (
      match (equal w1 z1, equal w2 z2, equal w3 z3, equal w4 z4) with
      | Some Equal, Some Equal, Some Equal, Some Equal -> Some Equal
      | _ -> None)
  | Generic (module M1), Generic (module M2) -> (
      match M1.Id with M2.Id -> Some Equal | _ -> None)
  | _ -> None

let[@inline] cantor x y = ((x + y) * (x + y + 1) / 2) + y

let rec uid : type a. a t -> int =
 fun w ->
  match w with
  | Unit -> 0
  | Bool -> 1
  | Float -> 2
  | Int -> 3
  | String -> 4
  | List w -> cantor 5 (uid w)
  | Array w -> cantor 6 (uid w)
  | Option w -> cantor 7 (uid w)
  | Tuple2 (w1, w2) -> cantor 8 (cantor (uid w1) (uid w2))
  | Tuple3 (w1, w2, w3) -> cantor 9 (cantor (uid w1) (cantor (uid w2) (uid w3)))
  | Tuple4 (w1, w2, w3, w4) ->
      cantor 10 (cantor (uid w1) (cantor (uid w2) (cantor (uid w3) (uid w4))))
  | Generic (module M) ->
      let u = Obj.Extension_constructor.id [%extension_constructor M.Id] in
      cantor 11 u

let[@inline] compare w1 w2 = uid w1 - uid w2

let array_compare cmp a b =
  let len = Array.length a in
  let c = Int.compare len (Array.length b) in
  if c <> 0 then c
  else
    let rec loop i =
      if i = len then 0
      else
        let c = cmp a.(i) b.(i) in
        if c <> 0 then c else loop (i + 1)
    in
    loop 0

let rec compare_values : type a b. a t -> b t -> a -> b -> int =
 fun w1 w2 v1 v2 ->
  match (w1, w2) with
  | Unit, Unit -> 0
  | Unit, _ -> -1
  | _, Unit -> 1
  | Bool, Bool -> Bool.compare v1 v2
  | Bool, _ -> -1
  | _, Bool -> 1
  | Float, Float -> Float.compare v1 v2
  | Float, _ -> -1
  | _, Float -> 1
  | Int, Int -> Int.compare v1 v2
  | Int, _ -> -1
  | _, Int -> 1
  | String, String -> String.compare v1 v2
  | String, _ -> -1
  | _, String -> 1
  | List w1, List w2 -> (
      match equal w1 w2 with
      | Some Equal -> List.compare (compare_values w1 w2) v1 v2
      | None -> compare w1 w2)
  | List _, _ -> -1
  | _, List _ -> 1
  | Array w1, Array w2 -> (
      match equal w1 w2 with
      | Some Equal -> array_compare (compare_values w1 w2) v1 v2
      | None -> compare w1 w2)
  | Array _, _ -> -1
  | _, Array _ -> 1
  | Option w1, Option w2 -> (
      match equal w1 w2 with
      | Some Equal -> Option.compare (compare_values w1 w2) v1 v2
      | None -> compare w1 w2)
  | Option _, _ -> -1
  | _, Option _ -> 1
  | Tuple2 (w1, w2), Tuple2 (z1, z2) ->
      let v11, v12 = v1 in
      let v21, v22 = v2 in
      let c = compare_values w1 z1 v11 v21 in
      if c <> 0 then c else compare_values w2 z2 v12 v22
  | Tuple2 _, _ -> -1
  | _, Tuple2 _ -> 1
  | Tuple3 (w1, w2, w3), Tuple3 (z1, z2, z3) ->
      let v11, v12, v13 = v1 in
      let v21, v22, v23 = v2 in
      let c = compare_values w1 z1 v11 v21 in
      if c <> 0 then c
      else
        let c = compare_values w2 z2 v12 v22 in
        if c <> 0 then c else compare_values w3 z3 v13 v23
  | Tuple3 _, _ -> -1
  | _, Tuple3 _ -> 1
  | Tuple4 (w1, w2, w3, w4), Tuple4 (z1, z2, z3, z4) ->
      let v11, v12, v13, v14 = v1 in
      let v21, v22, v23, v24 = v2 in
      let c = compare_values w1 z1 v11 v21 in
      if c <> 0 then c
      else
        let c = compare_values w2 z2 v12 v22 in
        if c <> 0 then c
        else
          let c = compare_values w3 z3 v13 v23 in
          if c <> 0 then c else compare_values w4 z4 v14 v24
  | Tuple4 _, _ -> -1
  | _, Tuple4 _ -> 1
  | Generic (module M1), Generic (module M2) -> (
      match equal w1 w2 with
      | Some Equal -> M1.compare v1 v2
      | None -> compare w1 w2)

let invalid_type fmt = Fmt.kstr (fun e -> Error (`Invalid_type e)) fmt

let exn_to_json_error f json =
  try Ok (f json) with Yojson.Json_error e -> Error (`Json_error e)

let[@inline] to_list_res (json : Y.t) = exn_to_json_error U.to_list json
let[@inline] to_bool_res (json : Y.t) = exn_to_json_error U.to_bool json
let[@inline] to_float_res (json : Y.t) = exn_to_json_error U.to_float json
let[@inline] to_int_res (json : Y.t) = exn_to_json_error U.to_int json
let[@inline] to_string_res (json : Y.t) = exn_to_json_error U.to_string json

let rec map_bind =
  let ( let* ) = Result.bind in
  fun f l ->
    match l with
    | [] -> Ok []
    | x :: xs ->
        let* y = f x in
        let* ys = map_bind f xs in
        Ok (y :: ys)

let rec of_json : type a. a t -> Y.t -> (a, error) result =
  let ( let* ) = Result.bind in
  fun t j ->
    match t with
    | Unit -> (
        match j with
        | `Assoc [ ("Unit", `Null) ] -> Ok ()
        | _ ->
            invalid_type "expected unit value, got %a"
              (Y.pretty_print ~std:true) j)
    | Bool -> to_bool_res j
    | Float -> to_float_res j
    | Int -> to_int_res j
    | List e ->
        let g = of_json e in
        let* l = to_list_res j in
        map_bind g l
    | Array e ->
        let g = of_json e in
        let* l = to_list_res j in
        let* l = map_bind g l in
        Ok (Array.of_list l)
    | String -> to_string_res j
    | Option e -> (
        match j with
        | `Assoc [ ("None", `Null) ] -> Ok None
        | `Assoc [ ("Some", j) ] ->
            let* v = of_json e j in
            Ok (Some v)
        | _ ->
            invalid_type "expected an option value, got %a"
              (Y.pretty_print ~std:true) j)
    | Tuple2 (e1, e2) -> (
        match j with
        | `List [ j1; j2 ] ->
            let* v1 = of_json e1 j1 in
            let* v2 = of_json e2 j2 in
            Ok (v1, v2)
        | _ ->
            invalid_type "expected a tuple of size 2, got %a"
              (Y.pretty_print ~std:true) j)
    | Tuple3 (e1, e2, e3) -> (
        match j with
        | `List [ j1; j2; j3 ] ->
            let* v1 = of_json e1 j1 in
            let* v2 = of_json e2 j2 in
            let* v3 = of_json e3 j3 in
            Ok (v1, v2, v3)
        | _ ->
            invalid_type "expected a tuple of size 3, got %a"
              (Y.pretty_print ~std:true) j)
    | Tuple4 (e1, e2, e3, e4) -> (
        match j with
        | `List [ j1; j2; j3; j4 ] ->
            let* v1 = of_json e1 j1 in
            let* v2 = of_json e2 j2 in
            let* v3 = of_json e3 j3 in
            let* v4 = of_json e4 j4 in
            Ok (v1, v2, v3, v4)
        | _ ->
            invalid_type "expected a tuple of size 4, got %a"
              (Y.pretty_print ~std:true) j)
    | Generic (module M) -> M.of_json j

let rec to_json : type a. a t -> a -> Y.t =
 fun t v ->
  match t with
  | Unit -> `Assoc [ ("unit", `Null) ]
  | Bool -> `Bool v
  | Float -> `Float v
  | Int -> `Int v
  | List e ->
      let to_json = to_json e in
      `List (List.map to_json v)
  | Array e ->
      let to_json = to_json e in
      let l = Array.to_seq v |> List.of_seq in
      `List (List.map to_json l)
  | String -> `String v
  | Option e -> (
      match v with
      | Some v -> `Assoc [ ("Some", to_json e v) ]
      | None -> `Assoc [ ("None", `Null) ])
  | Tuple2 (e1, e2) ->
      let v1, v2 = v in
      let j1 = to_json e1 v1 in
      let j2 = to_json e2 v2 in
      `List [ j1; j2 ]
  | Tuple3 (e1, e2, e3) ->
      let v1, v2, v3 = v in
      let j1 = to_json e1 v1 in
      let j2 = to_json e2 v2 in
      let j3 = to_json e3 v3 in
      `List [ j1; j2; j3 ]
  | Tuple4 (e1, e2, e3, e4) ->
      let v1, v2, v3, v4 = v in
      let j1 = to_json e1 v1 in
      let j2 = to_json e2 v2 in
      let j3 = to_json e3 v3 in
      let j4 = to_json e4 v4 in
      `List [ j1; j2; j3; j4 ]
  | Generic (module M) -> M.to_json v

let rec pp : type a. _ -> a t -> unit =
 fun ppf w ->
  match w with
  | Unit -> Fmt.pf ppf "unit"
  | Bool -> Fmt.pf ppf "bool"
  | Float -> Fmt.pf ppf "float"
  | Int -> Fmt.pf ppf "int"
  | String -> Fmt.pf ppf "string"
  | List w -> Fmt.pf ppf "(list %a)" pp w
  | Array w -> Fmt.pf ppf "(array %a)" pp w
  | Option w -> Fmt.pf ppf "(option %a)" pp w
  | Tuple2 (w1, w2) -> Fmt.pf ppf "(%a, %a)" pp w1 pp w2
  | Tuple3 (w1, w2, w3) -> Fmt.pf ppf "(%a, %a, %a)" pp w1 pp w2 pp w3
  | Tuple4 (w1, w2, w3, w4) ->
      Fmt.pf ppf "(%a, %a, %a, %a)" pp w1 pp w2 pp w3 pp w4
  | Generic (module M) -> Fmt.string ppf M.name

let pp_tuple3 ?(sep = Fmt.nop) pp_x pp_y pp_z ppf (x, y, z) =
  Fmt.pf ppf "%a%a%a%a%a" pp_x x sep () pp_y y sep () pp_z z

let pp_tuple4 ?(sep = Fmt.nop) pp_x pp_y pp_z pp_t ppf (x, y, z, t) =
  Fmt.pf ppf "%a%a%a%a%a%a%a" pp_x x sep () pp_y y sep () pp_z z sep () pp_t t

let rec pp_value : type a. a t -> a Fmt.t =
 fun w ppf v ->
  match w with
  | Unit -> Fmt.any "()" ppf ()
  | Bool -> Fmt.bool ppf v
  | Float -> Fmt.float ppf v
  | Int -> Fmt.int ppf v
  | String -> Fmt.string ppf v
  | List w -> Fmt.(list ~sep:comma (pp_value w)) ppf v
  | Array w -> Fmt.(array ~sep:comma (pp_value w)) ppf v
  | Option w -> Fmt.(option ~none:nop (pp_value w)) ppf v
  | Tuple2 (w1, w2) -> Fmt.(pair ~sep:comma (pp_value w1) (pp_value w2)) ppf v
  | Tuple3 (w1, w2, w3) ->
      (pp_tuple3 ~sep:Fmt.comma (pp_value w1) (pp_value w2) (pp_value w3)) ppf v
  | Tuple4 (w1, w2, w3, w4) ->
      (pp_tuple4 ~sep:Fmt.comma (pp_value w1) (pp_value w2) (pp_value w3)
         (pp_value w4))
        ppf v
  | Generic (module M) -> M.pp ppf v

module Syntax = struct
  let unit = Unit
  let bool = Bool
  let float = Float
  let int = Int
  let string = String
  let[@inline] list x = List x
  let[@inline] array x = Array x
  let[@inline] option x = Option x
  let[@inline] tuple2 x y = Tuple2 (x, y)
  let[@inline] tuple3 x y z = Tuple3 (x, y, z)
  let[@inline] tuple4 x y z t = Tuple4 (x, y, z, t)
end

type 'a wit = 'a t

module Map = struct
  module type Key = sig
    type 'a t

    val type_of : 'a t -> 'a wit
    val compare : 'a t -> 'a t -> int
  end

  module type S = sig
    type 'a key
    type t

    val empty : t
    val add : 'a key -> 'a -> t -> t
    val find : 'a key -> t -> 'a
  end

  module Make (K : Key) = struct
    type 'a key = 'a K.t
    type binding = Binding : 'a key * 'a -> binding

    module S = Stdlib.Set.Make (struct
      type t = binding

      let compare (Binding (k1, _)) (Binding (k2, _)) =
        let w1 = K.type_of k1 in
        let w2 = K.type_of k2 in
        match equal w1 w2 with
        | Some Equal -> K.compare k1 k2
        | None -> compare w1 w2
    end)

    type t = S.t

    let empty = S.empty
    let add k v = S.add (Binding (k, v))

    let find : type a. a key -> t -> a =
     fun k m ->
      let w = K.type_of k in
      let (Binding (k', v)) =
        S.find_first
          (fun (Binding (k', _)) ->
            let w' = K.type_of k' in
            match equal w w' with
            | Some Equal -> K.compare k k' = 0
            | None -> false)
          m
      in
      let w' = K.type_of k' in
      match equal w w' with Some Equal -> v | None -> raise Not_found
  end
end
