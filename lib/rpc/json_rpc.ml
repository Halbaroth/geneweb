module Y = Yojson.Safe
module U = Yojson.Safe.Util

(* According to the specification, a valid JSON-RPC v2.0 must contain
   a field "jsonrpc" whose the value is exactly this string. *)
let version = "2.0"
let msg l = `Assoc (("jsonrpc", `String version) :: l)

let is_jsonrpc j =
  try
    match U.member "jsonrpc" j with
    | `String v -> String.equal v version
    | _ -> false
  with U.Type_error _ -> false

type id = [ `String of string | `Int of int | `Null ]
type obj = (string * Yojson.Safe.t) list
type params = ByPosition of Yojson.Safe.t list | ByName of obj

type request =
  | Request of id * string * params option
  | Notification of string * params option

type error = { code : int; message : string; data : Yojson.Safe.t option }
type response = Success of id * Yojson.Safe.t | Echec of id * error

module Error = struct
  type t = error

  let[@inline always] error ?data code message = { code; message; data }

  let parse_error ?data () =
    error ?data (-32_700)
      "Invalid JSON was received by the server. An error occurred on the \
       server while parsing the JSON text."

  let invalid_request ?data () =
    error ?data (-32_600) "The JSON sent is not a valid Request object."

  let method_not_found ?data () =
    error ?data (-32601) "The method does not exist / is not available."

  let invalid_params ?data () =
    error ?data (-32603) "Invalid method parameter(s)."

  let internal_error ?data () = error ?data (-32603) "Internal JSON-RPC error"

  let server_error ?data i msg =
    if i < -32_099 || i > -32_000 then Fmt.invalid_arg "internal_error";
    error ?data i msg

  let to_json { code; message; data } =
    match data with
    | Some data ->
        `Assoc
          [ ("code", `Int code); ("message", `String message); ("data", data) ]
    | None -> `Assoc [ ("code", `Int code); ("message", `String message) ]

  let of_json j =
    try
      match (U.member "code" j, U.member "message" j, U.member "data" j) with
      | `Int code, `String message, `Null -> Some { code; message; data = None }
      | `Int code, `String message, data ->
          Some { code; message; data = Some data }
      | _ -> None
    with U.Type_error _ -> None

  let pp ppf t = Y.pretty_print ppf (to_json t)
end

module Request = struct
  type t = request

  let[@inline always] request ?params meth id = Request (id, meth, params)
  let[@inline always] notification ?params meth = Notification (meth, params)
  let params_to_json = function ByPosition l -> `List l | ByName o -> `Assoc o

  let params_of_json = function
    | `List l -> Some (ByPosition l)
    | `Assoc o -> Some (ByName o)
    | _ -> None

  let to_json t =
    match t with
    | Request (id, meth, Some p) ->
        msg
          [
            ("id", (id :> Yojson.Safe.t));
            ("method", `String meth);
            ("params", params_to_json p);
          ]
    | Request (id, meth, None) ->
        msg [ ("id", (id :> Yojson.Safe.t)); ("method", `String meth) ]
    | Notification (meth, Some p) ->
        msg [ ("method", `String meth); ("params", params_to_json p) ]
    | Notification (meth, None) -> msg [ ("method", `String meth) ]

  let of_json j =
    if not @@ is_jsonrpc j then None
    else
      try
        let params = U.member "params" j |> params_of_json in
        match (U.member "id" j, U.member "method" j) with
        | ((`String _ | `Int _) as id), `String meth ->
            Some (request ?params meth (id :> id))
        | `Null, `String meth -> Some (notification ?params meth)
        | _ -> None
      with U.Type_error _ -> None

  let pp ppf t = Y.pretty_print ppf (to_json t)
end

module Response = struct
  type t = response
  type nonrec error = error

  let[@inline always] success id r = Success (id, r)
  let[@inline always] echec id e = Echec (id, e)
  let id = function Success (id, _) -> id | Echec (id, _) -> id
  let parse_error ?data () = echec `Null @@ Error.parse_error ?data ()
  let invalid_request ?data () = echec `Null @@ Error.invalid_request ?data ()
  let method_not_found ?data id = echec id @@ Error.method_not_found ?data ()
  let invalid_params ?data id = echec id @@ Error.invalid_params ?data ()
  let internal_error ?data id = echec id @@ Error.internal_error ?data ()

  let server_error ?data ~code id fmt =
    Fmt.kstr (fun msg -> echec id @@ Error.server_error ?data code msg) fmt

  let to_json t =
    match t with
    | Success (id, c) -> msg [ ("id", (id :> Yojson.Safe.t)); ("result", c) ]
    | Echec (id, e) ->
        msg [ ("id", (id :> Yojson.Safe.t)); ("error", Error.to_json e) ]

  let of_json j =
    if not @@ is_jsonrpc j then None
    else
      try
        let error_opt = U.member "error" j |> Error.of_json in
        match (U.member "id" j, U.member "result" j, error_opt) with
        | ((`String _ | `Int _ | `Null) as id), `Null, Some e ->
            Some (echec id e)
        | ((`String _ | `Int _ | `Null) as id), r, None when r <> `Null ->
            Some (success id r)
        | _ -> None
      with U.Type_error _ -> None

  let pp ppf t = Y.pretty_print ppf (to_json t)
end
