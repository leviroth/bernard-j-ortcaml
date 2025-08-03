open! Core
open Import

let username = Username.of_string "ADefiniteDescription"

let spec : Usernote_page.Note.Spec.t =
  { text = "text"
  ; context = Some (Link (Thing.Link.Id.of_string "asdf"))
  ; time = Time_ns.epoch
  ; moderator = username
  ; warning = None
  }
;;

let via_jsonaf page_text =
  let json = Jsonaf.of_string page_text in
  let page = [%of_jsonaf: Usernote_page.t] json in
  Usernote_page.add_note page ~username ~spec;
  [%jsonaf_of: Usernote_page.t] page |> Jsonaf.to_string
;;

let via_jsont s =
  Jsonaf.of_string s
  |> Usernote_page2.t_of_jsonaf
  |> Usernote_page2.add_note ~username ~spec
  |> Usernote_page2.jsonaf_of_t
  |> Jsonaf.to_string
;;

let make_command converter =
  Command.basic
    ~summary:""
    (let%map_open.Command () = return () in
     fun () -> In_channel.input_all In_channel.stdin |> converter |> printf "%s\n")
;;

let decode =
  Command.basic
    ~summary:""
    (let%map_open.Command () = return () in
     fun () ->
       In_channel.input_all In_channel.stdin
       |> Jsonaf.of_string
       |> Usernote_page.t_of_jsonaf
       |> Usernote_page.sexp_of_t
       |> print_s)
;;

let command =
  Command.group
    ~summary:""
    [ "jsonaf", make_command via_jsonaf
    ; "jsont", make_command via_jsont
    ; "print", decode
    ]
;;
