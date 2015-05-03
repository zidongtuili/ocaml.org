(* Script preprocessing Markdown to enable special conventions. *)

open Core.Std
open Async.Std

(* Code evaluation
 ***********************************************************************)

(* FIXME: Once evaluated, the code blocks are rendered to HTML <pre>.
   These must necessarily start at the beginning of lines and thus
   cannot be, say, in blockquotes (this does not seem to be a problem
   with omd though). *)
let rec eval_code_blocks toplevel md =
  let open Omd in
  match md with
  | Code_block("tryocaml", code) :: tl
  | Code_block("ocamltop", code) :: tl ->
     Code.to_html toplevel code >>= fun html ->
     eval_code_blocks toplevel tl >>| fun tl ->
     Html_block("pre", [], [Html("code", ["class", Some "ocamltop"], html)])
     :: tl
  | Code_block("ocaml", code) :: tl ->
     let html = Code.highlight code in
     eval_code_blocks toplevel tl >>| fun tl ->
     Html_block("pre", [], [Html("code", ["class", Some "ocaml"], html)])
     :: tl
  | Blockquote t :: tl ->
     (* Order of evaluation is important: the code in [Blockquote] may
        have toplevel effects needed by later blocks. *)
     eval_code_blocks toplevel t >>= fun t ->
     eval_code_blocks toplevel tl >>| fun tl ->
     Blockquote t :: tl
  | Html(name, args, t) :: tl ->
     eval_code_blocks toplevel t >>= fun t ->
     eval_code_blocks toplevel tl >>| fun tl ->
     Html(name, args, t) :: tl
  | Html_block(name, args, t) :: tl ->
     eval_code_blocks toplevel t >>= fun t ->
     eval_code_blocks toplevel tl >>| fun tl ->
     Html_block(name, args, t) :: tl
  | e :: tl ->
     (* FIXME: do we want to recurse in other tags? *)
     eval_code_blocks toplevel tl >>| fun tl ->
     e :: tl
  | [] -> return []


(* Solutions toggle
 ***********************************************************************)

let toggle_js =
  let open Omd in
  let js = "function toggleContent(id) {
            // Get the DOM reference
            var el = document.getElementById(id);
            // Toggle
            el.style.display == \"block\" ? el.style.display = \"none\"
            : el.style.display = \"block\";
            }
            \n" in
  Html_block("script", ["type", Some "text/javascript"], [Raw js])

let n_button = ref 0

let add_toggle_button title html doc : Omd.t =
  incr n_button;
  let attr_button =
    ["onclick", Some(sprintf "toggleContent('ocamlorg%i')" !n_button);
     "class", Some "solution"] in
  let attr_div = ["id", Some(sprintf "ocamlorg%i" !n_button);
                  "class", Some "solution";
                  "media:type", Some "text/omd" ] in
  let open Omd in
  Html("button", attr_button, [Raw(Code.html_encode title)])
  :: NL :: Html_block("div", attr_div, html)
  :: doc

(** Finds "SOLUTION" directly followed by a Blockquote and transform
    those to HTML. *)
let rec toggle_solutions any_change md =
  let open Omd in
  match md with
  | Text "SOLUTION" :: NL :: Blockquote sol :: md
  | Text "SOLUTION" :: NL :: NL :: Blockquote sol :: md
  | Paragraph [Omd.Text "SOLUTION"] :: Blockquote sol :: md
  | Paragraph [Omd.Text "SOLUTION"] :: NL :: Blockquote sol :: md ->
     any_change := true;
     add_toggle_button "Solution" sol (toggle_solutions any_change md)
  | e :: md ->

     e :: toggle_solutions any_change md
  | [] -> []


let main () =
  let ch = new Netchannels.input_channel stdin in
  let md = Netchannels.string_of_in_obj_channel ch in
  let md = Omd.of_string md in
  (* Enable custom transformations. *)
  Oloop.create Oloop.Outcome.separate >>=? fun toplevel ->
  eval_code_blocks toplevel md >>| fun md ->
  let any_change = ref false in
  let md = toggle_solutions any_change md in
  let md = if !any_change then toggle_js :: md else md in
  print_string(Omd.to_markdown md);
  shutdown 0;
  Ok()

let () =
  ignore(main() >>| fun _ -> shutdown 1);
  never_returns (Scheduler.go ())

(* Local Variables: *)
(* compile-command: "make --no-print-directory -k -C .. script/md_preprocess" *)
(* End: *)
