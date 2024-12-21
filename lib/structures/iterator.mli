module type S = sig
  type elt
  (** Type of elements of the collection. *)

  type t
  (** Type of forward iterator. *)

  exception End
  (** Exception raised when we reach the end of the collection. *)

  val curr : t -> elt
  (* [curr it] returns the element currently pointed by the iterator [it].
     @raise End if we reach the end of the collection. *)

  val next : t -> unit
  (* [next it] advances the iterator [it] to the next element. *)

  val seek : elt -> t -> unit
  (* [seek e it] advances the iterator [it] to the smallest element in the
     collection that is greater or equal to [e].

     If [it] is already positioned at this smallest element, it remains
     unchanged. *)
end

module Union (O : Intf.Ordered) (It : S with type elt = O.t) : sig
  type t

  include S with type elt = O.t and type t := t

  val union : It.t list -> t
end

module Join (O : Intf.Ordered) (It : S with type elt = O.t) : sig
  type t

  include S with type elt = O.t and type t := t

  val join : It.t list -> t
  (* [join l] computes the join iterator of the iterators [l].

     @raise Invalid_argument if the list is empty. *)
end

val to_seq : (module S with type elt = 'a and type t = 'b) -> 'b -> 'a Seq.t
