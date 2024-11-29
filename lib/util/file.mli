exception File_error of string

val with_in_channel : string -> (in_channel -> 'a) -> 'a
(** [with_in_channel path f] opens the file [path] and calls [f] on an input
    channel of this file. The channel is properly closed after leaving the
    function [f]. *)

val with_out_channel : string -> (out_channel -> 'a) -> 'a
(** [with_out_channel path f] opens the file [path] and calls [f] on an output
    channel of this file. The channel is properly closed after leaving the
    function [f]. *)

val with_dir : string -> (Unix.dir_handle -> 'a) -> 'a
(** [with_open path f] opens the directory [path] and calls [f] on its
    directory descriptor. The directory descriptor is properly closed after
    leaving the function [f]. *)

val create_file : ?required_perm:int -> string -> unit
(** [create_file ?required_perm ~kind path] creates a file at [path] if the
    target does not exist.

    @raise File_error if the target exists but it is not a regular file or
                      the permission is wrong. *)

val create_dir : ?required_perm:int -> string -> unit
(** [create_dir ?required_perm ~kind path] creates a directory at [path] if
    the target does not exist.

    @raise File_error if the target exists but it is not a directory or the
                      permission is wrong. *)

val walk_folder :
  ?recursive:bool ->
  ([ `File of string | `Dir of string ] -> 'a -> 'a) ->
  string ->
  'a ->
  'a
(** [walk_folder ~recursive f dir] accumulates [f] on all the regular files or
    directories of [dir].

    The argument of [f] is the relative path of the file or or subdirectory
    in [dir].

    If [recursive] is [true], the iterator also explores subdirectories. [false]
    is the default.

    @raise Unix.Unix_error if the function cannot open a file or a
                           subdirectory in [dir]. *)
