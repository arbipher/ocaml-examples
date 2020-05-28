(* https://gist.github.com/keleshev/5af7502e6810b8986d9f5332e7e8a795 *)

(* #! /usr/bin/env ocaml *)

let fprintf = Format.fprintf

type t =
  | Tag of {name: string; attributes: (string * string) list; body: t list}
  | String of string

let format_attribute f (key, value) = fprintf f " %s=\"%s\"" key value

let rec format f = function
  | Tag {name; attributes=[]; body} ->
    let format_body = Format.pp_print_list format in
    fprintf f "@[<hv 3><%s>@,%a@;<0 -3></%s>@]" name format_body body name
  | Tag {name; attributes; body} ->
    let format_body = Format.pp_print_list format in
    let format_attributes = Format.pp_print_list format_attribute in
    fprintf f "@[<hv 3><%s@[<hv>%a@]>@,%a@;<0 -3></%s>@]"
      name format_attributes attributes format_body body name
  | String text -> fprintf f "%s" text

let tag ?(attributes=[]) name body = Tag {name; attributes; body}
let bar s = tag "bar" [String s]

let () =
  Format.set_margin 50;

  let xml = tag "foo" ~attributes:["bar", "baz"; "qux", "mux"] [
      tag "foo" ~attributes:[
        "one", "very long attribute";
        "two", "very long attribute";
        "three", "very long attribute";
        "four", "very long attribute";
      ] [bar "x"];
      tag "foo" [
        bar "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"; bar "b"; bar "c";
      ];
      tag "foo" [bar "a"; bar "b"; bar "c"];
      tag "foo" [bar "a"; bar "b"];
    ] in
  format Format.std_formatter xml