type 'a meth = string * 'a Encoding.desc * 'a

let meth name desc f = (name, desc, f)

type binding = Binding : 'a Encoding.desc * 'a -> binding

module MS = Map.Make (String)

type t = binding MS.t

let empty = MS.empty
let[@inline always] add (name, desc, f) = MS.add name (Binding (desc, f))
let find = MS.find_opt

module E = Encoding.Syntax

module PingPong = struct
  let ping = meth "ping" E.(ret string) "pong"
  let all = empty |> add ping
end
