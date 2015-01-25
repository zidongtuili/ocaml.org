(** Render an RSS feed to HTML, for the headlines or the actual posts. *)

open Printf
open Nethtml
open Syndic

let planet_url = Uri.of_string "/community/planet/"
let planet_full_url = Uri.of_string "//ocaml.org/community/planet/"

let planet_feeds_file = "planet_feeds.txt"

(* Utils
 ***********************************************************************)

(* Qualify [name] with an empty namespace. *)
let n name : Xmlm.name = ("", name)

(* Remove all tags *)
let rec syndic_to_buffer b = function
  | XML.Node (_, _, subs) -> List.iter (syndic_to_buffer b) subs
  | XML.Data (_, d) -> Buffer.add_string b d

let syndic_to_string x =
  let b = Buffer.create 1024 in
  List.iter (syndic_to_buffer b) x;
  Buffer.contents b

let string_of_text_construct : Atom.text_construct -> string = function
  (* FIXME: we probably would like to parse the HTML and remove the tags *)
  | Atom.Text s | Atom.Html(_,s) -> s
  | Atom.Xhtml(_, x) -> syndic_to_string x

let attr_is name = fun ((ns,t), _) ->
  ns = "" && t = name

let rec resolve ?xmlbase (html: Cow.Html.t) =
  List.map (resolve_links_el ~xmlbase) html
and resolve_links_el ~xmlbase = function
  | `El((("","a"), attrs), sub) ->
     let attrs = match List.partition (attr_is "href") attrs with
       | [], _ -> attrs
       | (_, h) :: _, attrs ->
          let src = Uri.to_string(XML.resolve xmlbase (Uri.of_string h)) in
          (n"href", src) :: attrs in
     `El((n"a", attrs), resolve ?xmlbase sub)
  | `El((("","img"), attrs), sub) ->
     let attrs = match List.partition (attr_is "src") attrs with
       | [], _ -> attrs
       | (_, src) :: _, attrs ->
          let src = Uri.to_string(XML.resolve xmlbase (Uri.of_string src)) in
          (n"src", src) :: attrs in
     `El((n"img", attrs), sub)
  | `El(tag, sub) ->
     `El(tag, resolve ?xmlbase sub)
  | `Data _ as d -> d


(* Things that posts should not contain *)
let undesired_tags = [n "style"; n "script"]
let undesired_attr = [n "id"]

let remove_undesired_attr =
  List.filter (fun (a,_) -> not(List.mem a undesired_attr))

let rec remove_undesired_tags (html: Cow.Html.t) =
  Utils.filter_map html remove_undesired_tags_el
and remove_undesired_tags_el = function
  | `El((t, a), sub) ->
     if List.mem t undesired_tags then None
     else Some(`El((t, remove_undesired_attr a),
                   remove_undesired_tags sub))
  | `Data _ as d -> Some d

let html_of_text ?xmlbase s =
  try Cow.Html.of_string s
      |> resolve ?xmlbase
      |> remove_undesired_tags
  with _ ->
    [`Data s]

(* Do not trust sites using XML for HTML content.  Convert to string
   and parse back.  (Does not always fix bad HTML unfortunately.) *)
let rec html_of_syndic =
  let ns_prefix _ = Some "" in
  fun ?xmlbase h ->
  html_of_text ?xmlbase
               (String.concat "" (List.map (XML.to_string ~ns_prefix) h))


(* Feeds
 ***********************************************************************)

type feed =
  | Atom of Atom.feed
  | Rss2 of Rss2.channel
  | Broken of string (* the argument gives the reason *)

let classify_feed ~xmlbase (xml: string) =
  try Atom(Atom.parse ~xmlbase (Xmlm.make_input (`String(0, xml))))
  with Atom.Error.Error _ ->
          try Rss2(Rss2.parse ~xmlbase (Xmlm.make_input (`String(0, xml))))
          with Rss2.Error.Error _ ->
                Broken "Neither Atom nor RSS2 feed"

type contributor = {
    name  : string;
    title : string;
    url   : Uri.t option;
    feed  : feed;
  }

let feed_of_url ~name url_s =
  let url = Uri.of_string url_s in
  try
    let feed = classify_feed ~xmlbase:url (Http.get url_s) in
    let title = match feed with
      | Atom atom -> string_of_text_construct atom.Atom.title
      | Rss2 ch -> ch.Rss2.title
      | Broken _ -> "" in
    { name;  title;  url = Some url;  feed }
  with
  | Http_client.Http_protocol(Http_client.Timeout s)
  | Http_client.Http_protocol(Http_client.Name_resolution_error s) ->
     { name;  title = "";  url = Some url;  feed = Broken s }
  | Http_client.Http_protocol Http_client.Too_many_redirections ->
     { name;  title = "";  url = Some url;
       feed = Broken "Too many redirections" }
  | Http_client.Http_error(err, _) ->
     let msg = Nethttp.(string_of_http_status (http_status_of_int err)) in
     { name;  title = "";  url = Some url;  feed = Broken msg }

let planet_feeds =
  let add_feed acc line =
    try
      let i = String.index line '|' in
      let name = String.sub line 0 i in
      let url = String.sub line (i+1) (String.length line - i - 1) in
      feed_of_url ~name url :: acc
    with Not_found -> acc in
  lazy(List.fold_left add_feed [] (Utils.lines_of_file planet_feeds_file))

let html_contributors () =
  let feeds = Lazy.force planet_feeds in
  let contributors =
    List.sort (fun c1 c2 -> String.compare c1.name c2.name) feeds in
  let contrib_html c =
    let attr = match c.feed with
      | Broken s -> [n"class", "broken";  n"title", s]
      | _ -> [] in
    let li = match c.url with
      | Some u -> [`El((n"a", (n"href", Uri.to_string u) :: attr),
                      [`Data c.name])]
      | None -> [`Data c.name] in
    `El((n"li", []), li) in
  [`El((n"ul", []), List.map contrib_html contributors)]


let to_opml feeds =
  let now = CalendarLib.Calendar.now() in
  let head = Syndic.Opml1.head ~date_modified:now
                               ~owner_name:"ocaml.org"
                               ~owner_email:"infrastructure@lists.ocaml.org"
                               "OCaml Planet" in
  let outline f =
    Syndic.Opml1.outline ~typ:"rss"
                         ~attrs:[n "title", f.title]
                         ?xml_url:f.url
                         f.name in
  { Syndic.Opml1.version = "1.1";  head;  body = List.map outline feeds }

let opml fname =
  let fh = open_out fname in
  Syndic.Opml1.output (to_opml(Lazy.force planet_feeds)) (`Channel fh);
  close_out fh



(* Blog feed
 ***********************************************************************)

(** Our representation of a "post". *)
type post = {
  title  : string;
  link   : Uri.t option;   (* url of the original post *)
  date   : Syndic.Date.t option;
  contributor: contributor;
  author : string;
  email  : string;    (* the author email, "" if none *)
  desc   : Cow.Html.t;
}

(* Email on the forge contain the name in parenthesis *)
let forge_name_re =
  Str.regexp ".*(\\([^()]*\\))"

let special_processing (p: post) =
  if p.contributor.name = "Caml Weekly News" then
    { p with title = "Weekly News" }
  else if p.contributor.name = "OCamlCore Forge News" then
    (* Extract the author from email. *)
    let author = if Str.string_match forge_name_re p.email 0 then
                   Str.matched_group 1 p.email
                 else p.author in
    { p with author = author }
  else if String.contains p.email '@' then p
  else (* consider p.email is actually a name (as GaGallium does) *)
    let author =
      if p.author = "" || p.author = p.contributor.name then p.email
      else p.author in
    { p with email = ""; author }

let post_compare p1 p2 =
  (* Most recent posts first.  Posts with no date are always last *)
  match p1.date, p2.date with
  | Some d1, Some d2 -> Syndic.Date.compare d2 d1
  | None, Some _ -> 1
  | Some _, None -> -1
  | None, None -> 1

let digest_post p = match p.link with
  | None -> Digest.to_hex (Digest.string (p.title))
  | Some u -> Digest.to_hex (Digest.string (p.title ^ Uri.to_string u))

let string_of_option = function None -> "" | Some s -> s

let post_of_atom ~contributor (e: Atom.entry) =
  let open Atom in
  let link = try Some (List.find (fun l -> l.rel = Alternate) e.links).href
             with Not_found -> match e.links with
                              | l :: _ -> Some l.href
                              | [] -> None in
  let date = match e.published with
    | Some _ -> e.published
    | None -> Some e.updated in
  let desc = match e.content with
    | Some(Text s) -> html_of_text s
    | Some(Html(xmlbase, s)) -> html_of_text ?xmlbase s
    | Some(Xhtml(xmlbase, h)) -> html_of_syndic ?xmlbase h
    | Some(Mime _) | Some(Src _)
    | None ->
       match e.summary with
       | Some(Text s) -> html_of_text s
       | Some(Html(xmlbase, s)) -> html_of_text ?xmlbase s
       | Some(Xhtml(xmlbase, h)) -> html_of_syndic ?xmlbase h
       | None -> [] in
  let author, _ = e.authors in
  special_processing { title = string_of_text_construct e.title;
                       link;  date;
                       contributor;
                       author = author.name;
                       email = "";
                       desc }

let post_of_rss2 ~(contributor: contributor) it =
  let open Syndic.Rss2 in
  let title, desc = match it.story with
    | All (t, xmlbase, d) ->
       t, (match it.content with
           | (_, "") -> html_of_text ?xmlbase d
           | (xmlbase, c) -> html_of_text ?xmlbase c)
    | Title t -> t, []
    | Description(xmlbase, d) ->
       "", (match it.content with
            | (_, "") -> html_of_text ?xmlbase d
            | (xmlbase, c) -> html_of_text ?xmlbase c) in
  let link = match it.guid, it.link with
    | Some u, _ when u.permalink -> Some u.data
    | _, Some _ -> it.link
    | Some u, _ ->
       (* Sometimes the guid is indicated with isPermaLink="false" but
          is nonetheless the only URL we get (e.g. ocamlpro). *)
       Some u.data
    | None, None -> None in
  special_processing { title; link; contributor;
                       author = contributor.name;
                       email = string_of_option it.author;
                       desc;
                       date = it.pubDate }

let posts_of_contributor c =
  match c.feed with
  | Atom f -> List.map (post_of_atom ~contributor:c) f.Atom.entries
  | Rss2 ch -> List.map (post_of_rss2 ~contributor:c) ch.Rss2.items
  | Broken _ -> []


(* Limit the length of the description presented to the reader. *)

let rec length_html html =
  List.fold_left (fun l h -> l + length_html_el h) 0 html
and length_html_el = function
  | `El(_, content) -> length_html content
  | `Data d -> String.length d

let rec text_of_html html =
  String.concat "" (List.map text_of_el html)
and text_of_el = function
  | `El(_, content) -> text_of_html content
  | `Data d -> d

let rec len_prefix_of_html (html: Cow.Html.t) len =
  if len <= 0 then 0, []
  else match html with
       | [] -> len, []
       | el :: tl -> let len, prefix_el = len_prefix_of_el el len in
                    let len, prefix_tl = len_prefix_of_html tl len in
                    len, prefix_el :: prefix_tl
and len_prefix_of_el el len =
  match el with
  | `Data d ->
     let len' = len - String.length d in
     len', (if len' >= 0 then el else `Data(String.sub d 0 len ^ "…"))
  | `El((tag, args), content) ->
     (* Remove "id" and "name" to avoid duplicate anchors with the
        whole post. *)
     let args = List.filter (fun ((_,n),_) -> n <> "id" && n <> "name") args in
     let len, prefix_content = len_prefix_of_html content len in
     len, `El((tag, args), prefix_content)

let rec prefix_of_html html len =
  snd(len_prefix_of_html html len)


let new_id =
  let id = ref 0 in
  fun () -> incr id; sprintf "ocamlorg-post%i" !id

(* [toggle html1 html2] return some piece of html with buttons to pass
   from [html1] to [html2] and vice versa. *)
let toggle ?(anchor="") html1 html2 =
  let button id1 id2 text =
    `El((n"a", [n"onclick", sprintf "switchContent('%s','%s')" id1 id2;
                n"class", "btn planet-toggle";
                n"href", "#" ^ anchor]),
        [`Data text])
  in
  let id1 = new_id() and id2 = new_id() in
  [`El((n"div", [n"id", id1]),
       html1 @ [button id1 id2 "Read more..."]);
   `El((n"div", [n"id", id2;  n"style", "display: none"]),
       html2 @ [button id2 id1 "Hide"])]

let toggle_script : Cow.Html.t =
  let script =
    "function switchContent(id1,id2) {
     // Get the DOM reference
     var contentId1 = document.getElementById(id1);
     var contentId2 = document.getElementById(id2);
     // Toggle
     contentId1.style.display = \"none\";
     contentId2.style.display = \"block\";
     }\n" in
  [`El((n"script", [n"type", "text/javascript"]), [`Data script])]


(* In addition to the feed name, print the author name (general feed
   used by several authors). *)
let want_contributor_and_author p =
  p.author <> "" && p.author <> p.contributor.name
  && not(String.contains p.author '.')
  && not(String.contains p.author '@')
  (* FIXME: maybe we want to be more subtle by checking for word boundaries: *)
  && not(Utils.KMP.is_substring p.author p.contributor.name)

let html_author_of_post p =
  if want_contributor_and_author p then
    let author =
      if p.email = "" then `Data p.author
      else `El((n"a", [n"href", "mailto:" ^ p.email]), [`Data p.author]) in
    [`El((n"span", []),
         [`Data p.contributor.name; `Data " ("; author; `Data ")" ])]
  else
    if p.contributor.name = "" then []
    else if p.email = "" then [`Data p.contributor.name]
    else [`El((n"a", [n"href", "mailto:" ^ p.email]),
              [`Data p.contributor.name])]

let html_date_of_post p =
  match p.date with
  | None -> []
  | Some d ->
     let date =
       let open Syndic.Date in
       sprintf "%s %02d, %d" (string_of_month(month d)) (day d) (year d) in
     [`Data date]

let google_plus = Uri.of_string "https://plus.google.com/share"
let facebook = Uri.of_string "https://www.facebook.com/share.php"
let twitter = Uri.of_string "https://twitter.com/intent/tweet"

(* Transform a post [p] (i.e. story) into HTML. *)
let html_of_post p : Cow.Html.t =
  let open Cow in
  let title_anchor = digest_post p in
  let html_title, share = match p.link with
    | None -> [`Data p.title], []
    | Some u ->
       let link_orig html = Html.link html ~href:u ~target:`blank
                                      ~title:"Go to the original post" in
       let post = Uri.with_fragment planet_full_url (Some title_anchor) in
       let google = Uri.with_query' google_plus ["url", Uri.to_string u] in
       let fb = Uri.with_query' facebook ["u", Uri.to_string post;
                                          "t", p.title] in
       let tw = Uri.with_query' twitter ["url", Uri.to_string post;
                                         "text", p.title] in
       let rss =
         match p.contributor.url with
         | Some feed ->
            [Html.link ~cls:"rss" ~target:`blank
                       ~href:feed ~title:"Original RSS feed"
                       [Html.img (Uri.of_string "/img/rss.svg")
                                 ~alt:"RSS" ~cls:"svg";
                        Html.img (Uri.of_string "/img/rss.png")
                                 ~alt:"RSS" ~cls:"png" ]]
         | None -> [] in
       [link_orig [`Data p.title]],
       [`El((n"span", [n"class", "share"]),
            link_orig [Html.img (Uri.of_string "/img/chain-link-icon.png")]
            :: Html.link ~href:google ~title:"Share on Google+"
                         ~cls:"googleplus" ~target:`blank
                         [Html.img (Uri.of_string "/img/googleplus.png")
                                   ~alt:"Google+"]
            :: Html.link ~href:fb ~title:"Share on Facebook"
                         ~cls:"facebook" ~target:`blank
                         [Html.img (Uri.of_string "/img/facebook.png")
                                   ~alt:"FB"]
            :: Html.link ~href:tw ~title:"Share on Twitter"
                         ~cls:"twitter" ~target:`blank
                         [Html.img (Uri.of_string "/img/twitter.png")
                                   ~alt:"Twitter"]
            :: rss) ] in
  let sep = `Data " — " in
  let additional_info = match html_author_of_post p, html_date_of_post p with
    | [], [] -> []
    | html_author, [] -> sep :: html_author
    | [], date -> sep :: date
    | html_author, date -> sep :: (html_author @ (`Data ", " :: date)) in
  let additional_info =
    [`El((n"span", [n"class", "additional-info"]), additional_info)] in
  let desc =
    if length_html p.desc < 500 then p.desc
    else toggle (prefix_of_html p.desc 500) p.desc ~anchor:title_anchor
  in
  [`Data "\n";
   `El((n"a", [n"name", title_anchor]), []);
   `El((n"section", [n"style", "clear: both"]),
       [`El((n"h1", [n"class", "ruled planet"]),
            share @ html_title @ additional_info);
        `El((n"div", [n"class", "planet-post"]), desc)]);
   `Data "\n"]


let li_of_post p : Cow.Html.element =
  let sep = `Data " — " in
  let title = match p.link with
    | None -> [`Data p.title]
    | Some u -> [`El((n"a", [n"href", Uri.to_string u;
                            n"target", "_blank";
                            n"title", "Go to the original post"]),
                    [`Data p.title]) ] in
  let line = match html_author_of_post p, html_date_of_post p with
    | [], [] -> title
    | html_author, [] -> title @ (sep :: html_author)
    | [], date -> date @ (`Data "," :: title)
    | html_author, date ->
       date @ (`Data ", " :: title @ (sep :: html_author)) in
  `El((n"li", []), line)

let netdate_of_calendar d =
  let month =
    let open Syndic.Date in
    match month d with
    | Jan -> 1 | Feb -> 2 | Mar -> 3 | Apr -> 4 | May -> 5 | Jun -> 6
    | Jul -> 7 | Aug -> 8 | Sep -> 9 | Oct -> 10 | Nov -> 11 | Dec -> 12 in
  { Netdate.year = Syndic.Date.year d;
    month;
    day = Syndic.Date.day d;
    hour = Syndic.Date.hour d;
    minute = Syndic.Date.minute d;
    second = truncate(Syndic.Date.second d);
    nanos = 0;  zone = 0;  week_day = -1 }

(* Similar to [html_of_post] but tailored to be shown in a list of
   news (only titles are shown, linked to the page with the full story). *)
let headline_of_post ?(planet=false) ?(img_alt="") ~l9n ~img p : Cow.Html.t =
  let open Cow in
  let link =
    if planet then Some(Uri.with_fragment planet_url (Some(digest_post p)))
    else p.link in
  let html_icon = match link with
    | Some href ->
       [Html.link ~href
                  [Html.img (Uri.of_string(img ^ ".svg")) ~cls:"svg"
                            ~alt:img_alt;
                   Html.img (Uri.of_string(img ^ ".png")) ~cls:"png"
                            ~alt:img_alt] ]
    | None -> [] in
  let html_date = match p.date with
    | None -> html_icon
    | Some d ->
       (* Netdate internationalization functions are more developed. *)
       let d =
         let d = netdate_of_calendar d in
         if Netdate.format ~fmt:"%x" d = Netdate.format ~fmt:"%x" d ~l9n then
           (* English style *)
           Netdate.format ~fmt:"%B %e, %Y" d ~l9n
         else
           Netdate.format ~fmt:"%e %B %Y" d ~l9n in
       `El((n"p", []), [`Data d]) :: html_icon in
  let html_title =
    `El((n"h1", []),
        match link with
        | None -> [`Data p.title]
        | Some href -> [Html.link ~href ~title:p.title [`Data p.title]] ) in
  [`El((n"li", []), [`El((n"article", []), html_title :: html_date)]);
   `Data "\n"]

let rec remove n l =
  if n <= 0 then l
  else match l with [] -> []
                  | _ :: tl -> remove (n - 1) tl

let rec take n = function
  | [] -> []
  | e :: tl -> if n > 0 then e :: take (n-1) tl else []

let get_posts ?n ?(ofs=0) () =
  let feeds = Lazy.force planet_feeds in
  let posts = List.concat (List.map posts_of_contributor feeds) in
  let posts = List.sort post_compare posts in
  let posts = remove ofs posts in
  match n with
  | None -> posts
  | Some n -> take n posts

let headlines ?n:n_posts ?ofs ?planet ~l9n () : Cow.Html.t =
  let posts = get_posts ?n:n_posts ?ofs () in
  let img = "/img/news" in
  [`El((n"ul", [n"class", "news-feed"]),
       List.concat(List.map (headline_of_post ?planet ~l9n ~img) posts))]

let posts ?n:n_posts ?ofs () : Cow.Html.t =
  let posts = get_posts ?n:n_posts ?ofs () in
  [`El((n"div", []), List.concat(List.map html_of_post posts))]

let nposts () = List.length (get_posts ())


let en_string_of_month =
  let open Syndic.Date in
  function
  | Jan -> "January"
  | Feb -> "February"
  | Mar -> "March"
  | Apr -> "April"
  | May -> "May"
  | Jun -> "June"
  | Jul -> "July"
  | Aug -> "August"
  | Sep -> "September"
  | Oct -> "October"
  | Nov -> "November"
  | Dec -> "December"

module Year_Month = struct
  type t = int * Syndic.Date.month (* year, month *)

  let compare ((y1, m1): t) ((y2, m2): t) =
    let dy = compare y1 y2 in
    if dy = 0 then compare m1 m2 else dy
end
module DMap = Map.Make(Year_Month)

let list_of_posts ?n:n_posts ?ofs () : Cow.Html.t =
  let posts = get_posts ?n:n_posts ?ofs () in
  (* Split posts per year/month *)
  let classify m p =
    match p.date with
    | None -> m (* drop *)
    | Some d ->
       let key = (Syndic.Date.year d, Syndic.Date.month d) in
       let posts = try p :: DMap.find key m with Not_found -> [p] in
       DMap.add key posts m in
  let m = List.fold_left classify DMap.empty posts in
  let add_html (year, month) posts html =
    let title = en_string_of_month month ^ " " ^ string_of_int year in
    let posts = List.sort post_compare posts in
    `El((n"h2", [n"id", title]), [`Data title])
    :: `El((n"ul", []), List.map li_of_post posts)
    :: html in
  (* Older date considered first => final HTML has more recent dates first *)
  DMap.fold add_html m []


(* Aggregation of posts
 ***********************************************************************)

let aggregate ?n fname =
  let feeds = Lazy.force planet_feeds in
  let to_atom (c: contributor) =
    match c.feed with
    | Atom a -> Some(c.url, a)
    | Rss2 ch -> Some(c.url, Rss2.to_atom ch)
    | Broken _ -> None in
  let atoms = Utils.filter_map feeds to_atom in
  let feed = Atom.aggregate atoms ~title:(Atom.Text "OCaml Planet") in
  let feed = match n with
    | Some n ->
       (* Sort the entries by date and the the [n] most recent ones. *)
       let by_date (e1: Atom.entry) (e2: Atom.entry) =
         Syndic.Date.compare e2.Atom.updated e1.Atom.updated in
       let entries = List.sort by_date feed.Atom.entries in
       { feed with Atom.entries = take n entries }
    | None -> feed in
  let fh = open_out fname in
  Atom.output feed (`Channel fh);
  close_out fh


(* Email threads
 ***********************************************************************)

(* The author is put at the end of the title: " - author name".
   Beware that the name may contain "-" (assumed without spaces
   around). *)
let delete_author title =
  let rec seek_dash pos =
  try
    let i = String.rindex_from title pos '-' in
    if i > 0 && i < pos then
      if title.[i-1] = ' ' && title.[i+1] = ' ' then
        String.trim(String.sub title 0 i)
      else (* maybe a correct dash before ? *)
        seek_dash (i-1)
    else title
  with Not_found -> title in
  seek_dash (String.length title - 1)

(* Remove the "[Caml-list]" and possible "Re:". *)
let caml_list_re =
  Str.regexp_case_fold "^\\(Re: *\\)*\\(\\[[a-zA-Z0-9-]+\\] *\\)*"

(** [email_threads] does basically the same as [headlines] but filter
    the posts to have repeated subjects.  It also presents the subject
    better. *)
let email_threads ?n:n_posts ~l9n url =
  (* Do not use [n] yet because posts are filtered. *)
  let posts = posts_of_contributor (feed_of_url ~name:"" url) in
  let posts = List.sort post_compare posts in
  let normalize_title p =
    let title = Str.replace_first caml_list_re "" p.title in
    let title = delete_author title in
    { p with title } in
  let posts = List.map normalize_title posts in
  (* Keep only the more recent post of redundant subjects. *)
  let module S = Set.Make(String) in
  let seen = ref S.empty in
  let must_keep p =
    if S.mem p.title !seen then false
    else (seen := S.add p.title !seen;  true) in
  let posts = List.filter must_keep posts in
  let posts = (match n_posts with
               | Some n -> take n posts
               | None -> posts) in
  let img = "/img/mail-icon" in
  [`El((n"ul", [n"class", "news-feed"]),
       List.concat(List.map (fun p -> headline_of_post ~l9n ~img p) posts))]


(* Main
 ***********************************************************************)

let () =
  let url = ref "" in
  let action = ref `Posts in
  let n_posts = ref None in (* means unlimited *)
  let ofs_posts = ref 0 in
  let l9n = ref Netdate.posix_l9n in
  let specs = [
    ("--headlines", Arg.Unit(fun () -> action := `Headlines),
     " RSS feed to feed summary (in HTML)");
    ("--emails", Arg.Unit(fun () -> action := `Emails),
     " RSS feed of email threads to HTML");
    ("--subscribers", Arg.Unit(fun () -> action := `Subscribers),
     " list of subscribers (rendered to HTML if alone)");
    ("--posts", Arg.Unit(fun () -> action := `Posts),
     " RSS feed to HTML (default action)");
    ("--list", Arg.Unit(fun () -> action := `List),
     " RSS feed to a single HTML");
    ("--nposts", Arg.Unit(fun () -> action := `NPosts),
     " number of posts in the RSS feed");
    ("--opml", Arg.String(fun fn -> action := `Opml fn),
     "fname write an OMPL document to the given file");
    ("--aggregate", Arg.String(fun fn -> action := `Aggregate fn),
     "fname write the aggregated feed to the given file");
    ("-n", Arg.Int(fun n -> n_posts := Some n),
     "n limit the number of posts to n (default: all of them)");
    ("--ofs", Arg.Set_int ofs_posts,
     "n start at the n th post (first is numbered 0)");
    ("--locale",
     Arg.String(fun l -> l9n := Netdate.(l9n_from_locale l)),
     "l Translate dates for the locale l")  ] in
  let anon_arg s = url := s in
  Arg.parse (Arg.align specs) anon_arg "rss2html <URL>";
  let l9n = Netdate.compile_l9n !l9n in
  let output html = Cow.Html.output (`Channel stdout) html in
  (match !action with
   | `Headlines ->
      output (headlines ~planet:true ?n:!n_posts ~ofs:!ofs_posts ~l9n ())
   | `Emails -> output (email_threads ?n:!n_posts ~l9n !url)
   | `Posts -> output (toggle_script
                      @ posts ?n:!n_posts ~ofs:!ofs_posts ())
   | `List -> output (list_of_posts ?n:!n_posts ~ofs:!ofs_posts ())
   | `NPosts -> printf "%i" (nposts())
   | `Subscribers -> output (html_contributors())
   | `Opml fn -> opml fn (* output to file [fn]. *)
   | `Aggregate fn -> aggregate fn ?n:!n_posts
  );;


(* Local Variables: *)
(* compile-command: "make --no-print-directory -k -C .. script/rss2html" *)
(* End: *)
