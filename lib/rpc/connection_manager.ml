module H = Hashtbl.Make (struct
  type t = Unix.sockaddr

  let hash = Hashtbl.hash

  let equal (s1 : Unix.sockaddr) (s2 : Unix.sockaddr) =
    match (s1, s2) with
    | ADDR_INET (a1, _), ADDR_INET (a2, _) -> a1 = a2
    | ADDR_UNIX s1, ADDR_UNIX s2 -> String.equal s1 s2
    | _ -> false
end)

type 'a limit = Unlimited | Limit of 'a

type t = {
  handles : Lwt_unix.file_descr list H.t;
  max_connection : int limit;
  idle : float limit;
}

let limit_of_option = function Some v -> Limit v | None -> Unlimited

let make ?max_connection ?idle () =
  let max_connection = limit_of_option max_connection in
  let idle = limit_of_option idle in
  { handles = H.create 17; max_connection; idle }

let add { handles; max_connection; _ } sockaddr fd =
  let fds =
    match H.find handles sockaddr with exception Not_found -> [] | fds -> fds
  in
  match max_connection with
  | Limit i when List.length fds >= i -> false
  | _ ->
      H.replace handles sockaddr (fd :: fds);
      true
