(* Download urls and cache them — especially during development, it
   slows down the rendering to download over and over again the same
   URL. *)

open Lwt
open Lwt_io
module Client = Cohttp_lwt_unix.Client

let age fn =
  let now = Unix.time () in (* in sec *)
  let modif = (Unix.stat fn).Unix.st_mtime in
  now -. modif

let time_of_secs s =
  let s = truncate s in
  let m = s / 60 and s = s mod 60 in
  let h = m / 60 and m = m mod 60 in
  Printf.sprintf "%i:%02im%is" h m s

let rec read_all_to_buffer b buf len fd =
  Lwt_unix.read fd buf 0 len >>= fun nread ->
  if nread = 0 then return()
  else (
    (* For OCaml >= 4.02 *)
    (* Buffer.add_subytes b buf 0 nread; *)
    Buffer.add_substring b buf 0 nread;
    read_all_to_buffer b buf len fd
  )

let read_all fd =
  let b = Buffer.create 4096 in
  let buf = Bytes.create 4096 in
  read_all_to_buffer b buf 4096 fd >>= fun () ->
  return(Buffer.contents b)

let cache_secs = 3600. (* 1h *)

let get ?(cache_secs=cache_secs) url =
  let url_s = Uri.to_string url in
  let md5 = Digest.to_hex(Digest.string url_s) in
  let fn = Filename.concat Filename.temp_dir_name ("ocamlorg-" ^ md5) in
  let get_from_cache () =
    Lwt_unix.(openfile fn [O_RDONLY] 0o600) >>= fun fd ->
    read_all fd >>= fun data ->
    Lwt_unix.close fd >>= fun () ->
    eprintf "Read %S from cache %s (updated %s ago)\n"
            url_s fn (time_of_secs(age fn))  >>= fun () ->
    return(`Ok data)
  in
  if Sys.file_exists fn && age fn <= cache_secs then get_from_cache()
  else (
    try
      Client.get url >>= fun (r, b) ->
      let status = Cohttp.Response.(r.status) in
      if Cohttp.Code.(is_success(code_of_status status)) then (
        Cohttp_lwt_body.to_string b >>= fun data ->
        eprintf "Downloaded %s\n  → %s\n" url_s fn >>= fun () ->
        Lwt_unix.(openfile fn [O_WRONLY; O_CREAT; O_TRUNC] 0o600) >>= fun fd ->
        Lwt_unix.write fd data 0 (String.length data) >>= fun _ ->
        Lwt_unix.close fd >>= fun () ->
        return(`Ok data)
      )
      else if Sys.file_exists fn then get_from_cache()
      else return(`Error status)
    with Failure msg ->
      (* This is how name resolution failure is signaled *)
      return(`Name_resolution_error msg)
  )
