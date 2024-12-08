type meth = Method : 'a Encoding.desc * 'a -> meth

module M = Map.Make (String)

type t = meth M.t

let empty = M.empty
let[@inline always] add (name, desc, f) = M.add name (Method (desc, f))

let find : type a. t -> string -> a Encoding.desc -> a option =
 fun t name desc ->
  match M.find name t with
  | exception Not_found -> None
  | Method (d, v) -> (
      match Encoding.equal_desc d desc with
      | Some Equal -> Some v
      | None -> None)

module E = Encoding.Syntax

type 'a val_ = string * 'a Encoding.desc * 'a

let mk name desc f = (name, desc, f)

module PingPong = struct
  let ping =
    mk __FUNCTION__ E.(int @-> ret (tup2 int string)) @@ fun i -> (i + 1, "ping")

  let pong = mk __FUNCTION__ E.(ret string) "pong"
  let t = empty |> add ping |> add pong
end

let pingpong = PingPong.t
