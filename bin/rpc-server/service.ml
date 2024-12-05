type t = {
  name : string;
  handler : Geneweb_rpc.Server.handler;
}

let path name handler = { name; handler }

let route l = (List.hd l).handler
  (* let srv = List.find (fun srv -> String.equal "foo" srv.name) l in
  srv.handler *)
