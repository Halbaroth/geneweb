type ('a, 'r) meth
(** Type of method. *)

val decl : string -> ('a, 'r) Encoding.desc -> 'a -> ('a, 'r) meth
(** [decl n desc f] declares the method [n] with description [desc] and the
    underlying function [f]. *)

module Service : sig
  type t
  (** Type of a service. A service is just a heterogeneous set of methods. *)

  (** Internal binding used to store the methods. *)
  type binding = private Binding : ('a, 'r) Encoding.desc * 'a -> binding

  val empty : t
  (** An empty service. *)

  val add : ('a, 'r) meth -> t -> t
  (** [add meth s] adds the method [meth] in the service [s]. *)

  val find : string -> t -> binding option
  (** [find name s] finds the method [name] in the service [s]. *)
end

type t
(** Type of a route. *)

val path : string -> Service.t -> t
(** [path n srv] attaches the service [srv] to the path [n]. *)

val route : t list -> Server.handler
(** [route l] generates the handler for a server corresponding to the routes
    [l]. *)

module PingPong : sig
  val ping : (string, string) meth
  val srv : Service.t
end
