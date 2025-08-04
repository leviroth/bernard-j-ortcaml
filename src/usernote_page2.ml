open! Core
open! Import
open Jsonaf.Export

module Index_list (Param : sig
    type t [@@deriving sexp_of]

    include Comparable with type t := t
    include Stringable with type t := t
  end) =
struct
  type t = Param.t option Queue.t [@@deriving sexp_of]

  let t_of_jsonaf json =
    let element_of_jsonaf json =
      match json with
      | `Null -> None
      | `String s -> Some (Param.of_string s)
      | _ -> raise_s [%message "Unexpected usernote constant" (json : Jsonaf.t)]
    in
    [%of_jsonaf: element list] json |> Queue.of_list
  ;;

  let jsonaf_of_t t =
    `Array
      (Queue.to_list t
       |> List.map ~f:(function
         | None -> `Null
         | Some v -> `String (Param.to_string v)))
  ;;

  let index t element =
    match Queue.findi t ~f:(fun _ -> [%equal: Param.t option] element) with
    | Some (i, _) -> i
    | None ->
      let result = Queue.length t in
      Queue.enqueue t element;
      result
  ;;
end

module Moderators = Index_list (Username)
module Warnings = Index_list (String)

module Note = struct
  module Context = Usernote_page.Note.Context
  module Spec = Usernote_page.Note.Spec

  type t = Jsont.Json.t

  let create ({ text; context; time; moderator; warning } : Spec.t) ~moderators ~warnings =
    let moderator_index = Moderators.index moderators (Some moderator) in
    let warning_index = Warnings.index warnings warning in
    Jsont.Json.object'
      (List.map
         ~f:(fun (a, b) -> Jsont.Json.name a, b)
         ((match context with
           | None -> []
           | Some context -> [ "l", Jsont.Json.string (Context.to_string context) ])
          @ [ "n", Jsont.Json.string text
            ; ( "t"
              , Jsont.Json.int
                  (Time_ns.to_span_since_epoch time |> Time_ns.Span.to_int_sec) )
            ; "m", Jsont.Json.int moderator_index
            ; "w", Jsont.Json.int warning_index
            ]))
  ;;
end

type t =
  { moderators : Moderators.t
  ; warnings : Warnings.t
  ; notes : string
  }
[@@deriving sexp_of]

let decompress_blob blob =
  match Base64.decode_exn blob |> Ezgzip.Z.decompress ~header:true with
  | Error error ->
    let error = Format.asprintf "%a" Ezgzip.Z.pp_zlib_error error in
    raise_s [%message "Error decompressing blob" (error : string)]
  | Ok s -> s
;;

let t_of_jsonaf json =
  let () =
    match Jsonaf.member_exn "ver" json |> Jsonaf.int_exn with
    | 6 -> ()
    | version -> raise_s [%message "Unexpected usernotes version" (version : int)]
  in
  let rec member_nested fields json =
    match fields with
    | [] -> json
    | field :: rest -> member_nested rest (Jsonaf.member_exn field json)
  in
  let moderators =
    member_nested [ "constants"; "users" ] json |> [%of_jsonaf: Moderators.t]
  in
  let warnings =
    member_nested [ "constants"; "warnings" ] json |> [%of_jsonaf: Warnings.t]
  in
  let notes = Jsonaf.member_exn "blob" json |> Jsonaf.string_exn in
  { moderators; warnings; notes }
;;

let compress_blob json = Ezgzip.Z.compress ~header:true json |> Base64.encode_exn

let jsonaf_of_t { moderators; warnings; notes } =
  `Object
    [ "ver", [%jsonaf_of: int] 6
    ; ( "constants"
      , `Object
          [ "users", [%jsonaf_of: Moderators.t] moderators
          ; "warnings", [%jsonaf_of: Warnings.t] warnings
          ] )
    ; "blob", `String notes
    ]
;;

let update_list note =
  Jsont.recode ~dec:Jsont.(list json) (fun l -> note :: l) ~enc:Jsont.(list json)
;;

let add_note username note =
  Jsont.(update_mem ~absent:[ note ] username (update_list note))
;;

let add_note { moderators; warnings; notes } ~username ~spec =
  let username = Username.to_string username in
  let note = Note.create spec ~moderators ~warnings in
  let notes = Base64.decode_exn notes in
  let notes_reader = Bytesrw.Bytes.Reader.of_string notes in
  let decompressed = Bytesrw_zlib.Zlib.decompress_reads () notes_reader in
  let buffer = Buffer.create (String.length notes) in
  let writer =
    Bytesrw_zlib.Zlib.compress_writes () (Bytesrw.Bytes.Writer.of_buffer buffer) ~eod:true
  in
  Debug.am [%here];
  Jsont_bytesrw.recode (add_note username note) decompressed writer ~eod:true
  |> Stdlib.Result.get_ok;
  let notes = Base64.encode_exn (Buffer.contents buffer) in
  { moderators; warnings; notes }
;;
