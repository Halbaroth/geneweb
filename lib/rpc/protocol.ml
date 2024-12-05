type ('a, 'b, 'e) proc = {
  name : string;
  f : 'a -> 'b;
  e : ('a, 'b) Encoding.arrow;
  err : 'e Encoding.t;
}

module S = Encoding.Syntax

module PingPong = struct
  let ping = (S.(unit @-> ret string), fun () -> "ping")
  let pong = (S.(unit @-> ret string), fun () -> "pong")
end
