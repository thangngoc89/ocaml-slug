module Charmap = struct
  type t = (string, string) Hashtbl.t

  let mk_charmap maps =
    let charmap_hash : t = Hashtbl.create 600 in
    let () =
      maps
      |> List.iter
           (List.iter (fun (key, value) -> Hashtbl.add charmap_hash key value))
    in
    charmap_hash

  let base = mk_charmap [ Slug_data.base ]
end

let non_alphaNum = Re.Pcre.regexp {|[^A-Za-z0-9\s]|}

let slugify ?(sep = "-") ?(charmap = Charmap.base) ?(lowercase = true) str =
  let str =
    Uunf_string.normalize_utf_8 `NFD str
    |> Uuseg_string.fold_utf_8 `Grapheme_cluster
         (fun acc cluster ->
           match Hashtbl.find_opt charmap cluster with
           | None -> acc ^ cluster
           | Some replace -> acc ^ replace)
         ""
    |> String.trim in
  let str = Re.replace_string ~all:true non_alphaNum ~by:"" str in
  let str = Re.replace_string ~all:true (Re.Pcre.regexp "\\s+") ~by:sep str in
  let str = if lowercase then String.lowercase_ascii str else str in
  str

(* Replace whitespaces with sep *)
let%test _ = slugify "foo bar baz" = "foo-bar-baz"

let%test _ = slugify ~sep:"_" "foo bar baz" = "foo_bar_baz"

(* No duplicated seps *)
let%test _ = slugify "foo , bar" = "foo-bar"

(* Remove not allowed chars *)
let%test _ = slugify "foo] - -- [ bar" = "foo-bar"

(* Retain uppercases *)
let%test _ = slugify ~lowercase:false "Foo Bar" = "Foo-Bar"

(* Custom charmap *)
let%test _ =
  slugify
    ~charmap:(Charmap.mk_charmap [ Slug_data.base; Slug_data.vi ])
    "Việt Nam"
  = "viet-nam"
