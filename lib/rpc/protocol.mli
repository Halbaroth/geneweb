type t
type 'a meth
type binding = Binding : 'a Encoding.desc * 'a -> binding

val meth : string -> 'a Encoding.desc -> 'a -> 'a meth
val add : 'a meth -> t -> t
val find : string -> t -> binding option

module PingPong : sig
  val all : t
  val ping : string meth
end
