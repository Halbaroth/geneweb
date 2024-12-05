type ('a, 'b, 'e) proc = private {
  name : string;
  f : 'a -> 'b;
  e : ('a, 'b) Encoding.arrow;
  err : 'e Encoding.t;
}
