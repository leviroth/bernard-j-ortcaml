open! Core
open! Async
open! Import

type t = Pgx_async.t

val target_fullname_params : Action.Target.t -> Pgx.Value.t list
val already_acted : t -> target:Action.Target.t -> moderator:Username.t -> bool Deferred.t

val record_contents
  :  t
  -> target:Action.Target.t
  -> [ `Ok | `Already_recorded ] Deferred.t

val log_rule_application
  :  t
  -> target:Action.Target.t
  -> action_summary:string
  -> author:Username.t option
  -> moderator:Username.t
  -> subreddit:Thing.Subreddit.Id.t
  -> time:Time_ns.t
  -> unit Deferred.t

val update_subscriber_counts : t -> subreddits:Thing.Subreddit.t list -> unit Deferred.t

val update_moderator_table
  :  t
  -> moderators:Username.t list
  -> subreddit:Thing.Subreddit.Id.t
  -> unit Deferred.t
