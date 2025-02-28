open! Core
open! Async

let callback req _body =
  let open Cohttp in
  let uri = Request.uri req in
  match Request.meth req, Uri.path uri with
  | `GET, "/metrics" ->
    let data = Prometheus.CollectorRegistry.(collect_sync default) in
    let body = Fmt.to_to_string Prometheus_app.TextFormat_0_0_4.output data in
    let headers = Header.init_with "Content-Type" "text/plain; version=0.0.4" in
    Cohttp_async.Server.respond_string ~status:`OK ~headers body
  | _ -> Cohttp_async.Server.respond_string ~status:`Bad_request "Bad request"
;;
