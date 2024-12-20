module type Ordered = sig
  type t

  val compare : t -> t -> int
end

module type S = sig
  type elt
  (** Type of elements of the collection. *)

  type t
  (** Type of immutable set. *)

  val of_seq : elt Seq.t -> t
  (** Build an immutable set from a sequence of elements. The sequence is
      entirely forced while building the set. *)

  val to_seq : t -> elt Seq.t
  (** [to_seq t] returns the sequence of elements of [t] in ascending
      ordering. *)

  val mem : elt -> t -> bool
  (** [mem e t] checks if the element [e] is present in the set [t]. *)

  val cardinal : t -> int
  (** [cardinal t] returns the cardinal of the set. *)

  module Iterator : sig
    type t
    (** Type of an iterator on an immutable set. *)

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

  val iterator : t -> Iterator.t
  (** [iterator t] creates a mutable iterator for the set [t] starting on the
      first element. This iterator cannot be invalidate as the set immutable. *)
end

module Make (O : Ordered) : S with type elt = O.t
