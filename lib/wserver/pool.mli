val src : Logs.src

type t
(** Type of a worker pool for workers. *)

val start : int -> (int -> unit) -> unit
(** [start n f] creates a worker pool of [n] workers for the function [f]. *)
