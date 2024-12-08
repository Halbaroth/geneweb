(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Type introspection.

    @since 5.1 *)
module Type : sig
  (** The purpose of [eq] is to represent type equalities that may not otherwise
      be known by the type checker (e.g. because they may depend on dynamic
      data).

      A value of type [(a, b) eq] represents the fact that types [a] and [b] are
      equal.

      If one has a value [eq : (a, b) eq] that proves types [a] and [b] are
      equal, one can use it to convert a value of type [a] to a value of type
      [b] by pattern matching on [Equal]:
      {[
        let cast (type a) (type b) (Equal : (a, b) Type.eq) (a : a) : b = a
      ]}

      At runtime, this function simply returns its second argument unchanged.
  *)
  type (_, _) eq = Equal : ('a, 'a) eq  (** *)

  (** {1:identifiers Type identifiers} *)

  (** Type identifiers.

    A type identifier is a value that denotes a type. Given two type
    identifiers, they can be tested for {{!Id.provably_equal}equality} to
    prove they denote the same type. Note that:

    - Unequal identifiers do not imply unequal types: a given type can be
      denoted by more than one identifier.
    - Type identifiers can be marshalled, but they get a new, distinct,
      identity on unmarshalling, so the equalities are lost.

    See an {{!Id.example}example} of use. *)
  module Id : sig
    (** {1:ids Type identifiers} *)

    type !'a t
    (** The type for identifiers for type ['a]. *)

    val make : unit -> 'a t
    (** [make ()] is a new type identifier. *)

    val uid : 'a t -> int
    (** [uid id] is a runtime unique identifier for [id]. *)

    val provably_equal : 'a t -> 'b t -> ('a, 'b) eq option
    (** [provably_equal i0 i1] is [Some Equal] if identifier [i0] is equal
      to [i1] and [None] otherwise. *)
  end
end
