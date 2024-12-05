module Response = Json_rpc.Response
module MS = Map.Make (String)

type ('a, 'r) meth = { name : string; desc : ('a, 'r) Encoding.desc; f : 'a }

let decl name desc f = { name; desc; f }

module Service = struct
  type binding = Binding : ('a, 'r) Encoding.desc * 'a -> binding
  type t = binding MS.t

  let empty = MS.empty
  let add meth = MS.add meth.name (Binding (meth.desc, meth.f))
  let find = MS.find_opt
end

type t = string * Service.t

let path n srv = (n, srv)
let not_implemented id = Response.server_error ~code:10 id "not implemented"

let route l =
  let map = MS.of_seq (List.to_seq l) in
  fun _sockaddr target request ->
    match MS.find target map with
    | exception Not_found -> Lwt.return @@ Response.invalid_request ()
    | srv -> (
        match request with
        | Json_rpc.Notification _ -> Lwt.return @@ not_implemented `Null
        | Request (id, meth, params) -> (
            let call params =
              match Service.find meth srv with
              | None -> Lwt.return @@ Response.method_not_found id
              | Some (Service.Binding (desc, f)) -> (
                  match Encoding.eval desc f params with
                  | Some r -> Lwt.return @@ Response.success id r
                  | None -> Lwt.return @@ Response.invalid_params id)
            in
            match params with
            | Some (ByPosition l) -> call l
            | None -> call []
            | Some (ByName _) -> Lwt.return @@ not_implemented `Null))

module PingPong = struct
  let ping = decl "ping" Encoding.Syntax.(ret string) "pong"

  let srv = Service.empty |> Service.add ping
end
