module Json_rpc = Geneweb_rpc.Json_rpc
module Encoding = Geneweb_rpc.Encoding

type t = { name : string; protocol : Geneweb_rpc.Protocol.t }

let path name protocol = { name; protocol }
let ( let* ) = Option.bind

let route l sockaddr request =
  let protocol = (List.hd l).protocol in
  match request with
  | Json_rpc.Request (id, meth, params) -> (
      let params =
        match params with
        | Some (ByPosition l) -> l
        | Some (ByName _) -> assert false
        | None -> []
      in
      match Geneweb_rpc.Protocol.find meth protocol with
      | Some (Binding (desc, f)) -> (
          match Encoding.eval desc f params with
          | Some r -> Lwt.return @@ Json_rpc.Response.success id r
          | None -> Lwt.return @@ Json_rpc.Response.invalid_params id)
      | None -> Lwt.return @@ Json_rpc.Response.method_not_found id)
  | Notification _ -> assert false
