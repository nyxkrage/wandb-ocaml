(* Module re-exports *)

module Error = Error
module Mode = Types.Mode
module Resume = Types.Resume
module Reinit = Types.Reinit
module Value = Value
module Settings = Settings
module Config = Config
module Alert = Types.Alert
module Run = Run

(* Top-level types *)

type 'a result = ('a, Error.t) Stdlib.result
type path = string

let ( let* ) = Result.bind

let version = "0.0.0-dev"

(* Current run tracking *)

let current_run_ref : Run.t option ref = ref None

let current_run () =
  !current_run_ref

let init ?entity ?project ?dir ?id ?name ?notes ?tags ?config
    ?config_exclude_keys ?config_include_keys ?group
    ?job_type ?mode ?force ?(reinit = Reinit.Return_previous) ?resume ?resume_from ?fork_from () =
  let new_run () =
    let* run = Run.init ?entity ?project ?dir ?id ?name ?notes ?tags ?config
      ?config_exclude_keys ?config_include_keys ?group
      ?job_type ?mode ?force ?resume ?resume_from ?fork_from () in
    current_run_ref := Some run;
    Ok run
  in
  let is_active r =
    match Run.status r with
    | Ok Run.Status.Finished | Ok Run.Status.Failed | Error _ -> false
    | _ -> true
  in
  match !current_run_ref with
  | Some prev when is_active prev ->
    begin match reinit with
      | Reinit.Return_previous -> Ok prev
      | Reinit.Finish_previous ->
        let* () = Run.finish prev in
        new_run ()
    end
  | _ -> new_run ()

let with_run ?entity ?project ?dir ?id ?name ?notes ?tags ?config
    ?config_exclude_keys ?config_include_keys ?group
    ?job_type ?mode ?force ?resume ?resume_from ?fork_from ~f () =
  let* run = Run.init ?entity ?project ?dir ?id ?name ?notes ?tags ?config
    ?config_exclude_keys ?config_include_keys ?group
    ?job_type ?mode ?force ?resume ?resume_from ?fork_from () in
  let result = f run in
  let* () = Run.finish run in
  result

let log ?step ?commit values =
  match !current_run_ref with
  | Some run -> Run.log ?step ?commit run values
  | None -> Ok ()

let finish ?exit_code () =
  match !current_run_ref with
  | Some run ->
      let* () = Run.finish ?exit_code run in
      current_run_ref := None;
      Ok ()
  | None -> Ok ()
