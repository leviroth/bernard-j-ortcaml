open! Core
open! Async
open! Import

module Trigger : sig
  (** A rule [{commands; kinds}] is triggered when the bot sees a {e moderator
      report} with its reason in [reasons] on a target with whose kinds is in
      [kinds].

      So, for example, if there is a rule whose [commands] include ["rule 1"]
      and whose [kinds] include {!constructor:Action.Target.Kind.Comment}, a
      moderator can trigger the rule by reporting a comment with the reason
      "rule 1".
  *)
  type t =
    { commands : String.Caseless.Set.t
    ; kinds : Action.Target.Kind.Set.t
    }
  [@@deriving sexp, compare]
end

type t =
  { info : string
  ; trigger : Trigger.t
  ; actions : Action.t list
  }
[@@deriving sexp, fields]

val find_matching_report : t -> target:Action.Target.t -> Moderator_report.t option
val will_remove : t -> bool
