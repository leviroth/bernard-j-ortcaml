open! Core
open! Async

val callback
  :  Cohttp.Request.t
  -> Cohttp_async.Body.t
  -> Cohttp_async.Server.response Deferred.t
