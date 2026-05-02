let ( let* ) = Result.bind

module Status = struct
  type t =
    | Running
    | Finished
    | Failed
    | Crashed
    | Killed
    | Offline
    | Disabled
    | Unknown of string
end

type t =
  { id : string
  ; name : string option
  ; entity : string option
  ; project : string option
  ; root_dir : string
  ; sync_dir : string option
  ; tags : string list
  ; mutable config : Config.t
  ; mode : Types.Mode.t
  ; base_url : string
  ; mutable status_value : Status.t
  ; mutable core : Core.t option
  ; mutable partial_history : (string * Value.t) list
  }

let initial_status = function
  | Types.Mode.Online | Types.Mode.Shared -> Status.Running
  | Types.Mode.Offline -> Status.Offline
  | Types.Mode.Disabled -> Status.Disabled

let create ~id ?name ?entity ?project ~root_dir ?sync_dir ?(tags = [])
    ?(config = Config.empty) ~mode ~base_url () =
  { id
  ; name
  ; entity
  ; project
  ; root_dir
  ; sync_dir
  ; tags
  ; config
  ; mode
  ; base_url
  ; status_value = initial_status mode
  ; core = None
  ; partial_history = []
  }

let id t = t.id
let name t = t.name
let entity t = t.entity
let project t = t.project

let tags t = t.tags
let config t = t.config

let set_core t core =
  t.core <- core

let app_url base_url =
  if base_url = Util.default_base_url then
    "https://wandb.ai"
  else if Util.starts_with ~prefix:"https://api." base_url then
    "https://" ^ String.sub base_url 12 (String.length base_url - 12)
  else if Util.starts_with ~prefix:"http://api." base_url then
    "http://" ^ String.sub base_url 11 (String.length base_url - 11)
  else
    base_url

let project_url t =
  match t.mode, t.entity, t.project with
  | (Types.Mode.Offline | Types.Mode.Disabled), _, _ -> Ok None
  | _, Some entity, Some project ->
      Ok (Some (String.concat "/" [ app_url t.base_url; entity; project ]))
  | _ -> Ok None

let url t =
  match project_url t with
  | Ok (Some project_url) -> Ok (Some (project_url ^ "/runs/" ^ t.id))
  | Ok None -> Ok None
  | Error _ as e -> e

let sweep_url _t =
  Ok None

let status t =
  Ok t.status_value

let log ?step ?commit run values =
  match run.core with
  | None -> Ok ()
  | Some core ->
    let should_flush =
      match step, commit with
      | None, None -> true
      | _, None -> true
      | _, Some c -> c
    in
    run.partial_history <- run.partial_history @ values;
    if should_flush then begin
      Core.log core ~stream_id:run.id
        ~values:run.partial_history ~step ~flush:true;
      run.partial_history <- [];
      Ok ()
    end else
      Ok ()

let finish ?exit_code run =
  let exit_code = Option.value exit_code ~default:0 in
  (* Flush any pending history *)
  (match run.core, run.partial_history with
   | Some core, _ :: _ ->
     Core.log core ~stream_id:run.id
       ~values:run.partial_history ~step:None ~flush:true;
     run.partial_history <- []
   | _ -> ());
  let finish_result =
    match run.core with
    | Some core ->
      begin match Core.finish_run core ~stream_id:run.id ~exit_code with
      | Ok () ->
          Core.finish core ~exit_code ~stream_id:run.id;
          run.core <- None;
          Ok ()
      | Error (`Msg msg) ->
          Core.finish core ~exit_code ~stream_id:run.id;
          run.core <- None;
          Error (Error.make Error.Communication msg)
      end
    | None -> Ok ()
  in
  run.status_value <- Status.Finished;
  finish_result

(* Init helpers *)

let ends_with ~suffix value =
  let suffix_len = String.length suffix in
  let value_len = String.length value in
  value_len >= suffix_len
  && String.sub value (value_len - suffix_len) suffix_len = suffix

let strip_trailing_slashes value =
  let rec loop index =
    if index > 0 && value.[index - 1] = '/' then
      loop (index - 1)
    else
      String.sub value 0 index
  in
  loop (String.length value)

let expand_user path =
  if path = "~" then
    Option.value (Sys.getenv_opt "HOME") ~default:path
  else if String.length path >= 2 && String.sub path 0 2 = "~/" then
    match Sys.getenv_opt "HOME" with
    | Some home -> Filename.concat home (String.sub path 2 (String.length path - 2))
    | None -> path
  else
    path

let string_is_empty_or_whitespace value =
  String.trim value = ""

let string_has_outer_whitespace value =
  String.length value <> String.length (String.trim value)

let string_contains_any value chars =
  String.exists (fun ch -> String.contains chars ch) value

let normalize_base_url value =
  let value = strip_trailing_slashes (String.trim value) in
  match Auth.url_netloc value with
  | None ->
      Error
        (Error.make Error.Usage
           (Printf.sprintf "invalid W&B base URL: %S" value))
  | Some netloc ->
      let host =
        match String.split_on_char ':' netloc with
        | host :: _ -> host
        | [] -> netloc
      in
      if ends_with ~suffix:"wandb.ai" host && not (Util.starts_with ~prefix:"api." host) then
        Error
          (Error.make Error.Usage
             (Printf.sprintf
                "%s is not a valid server address, did you mean https://api.wandb.ai?"
                value))
      else if ends_with ~suffix:"wandb.ai" host && not (Util.starts_with ~prefix:"https://" value) then
        Error
          (Error.make Error.Usage
             "http is not secure, please use https://api.wandb.ai")
      else
        Ok value

let rec mkdir_p path =
  let path = expand_user path in
  if path = "" || path = "." || path = Filename.dirname path then
    Ok ()
  else if Sys.file_exists path then
    if Sys.is_directory path then
      Ok ()
    else
      Error
        (Error.make Error.Io
           (Printf.sprintf "path exists and is not a directory: %s" path))
  else
    let parent = Filename.dirname path in
    let* () = mkdir_p parent in
    try
      Sys.mkdir path 0o755;
      Ok ()
    with
    | Sys_error message ->
        if Sys.file_exists path && Sys.is_directory path then
          Ok ()
        else
          Error (Error.make Error.Io message)

let write_file path contents =
  try
    let channel = open_out path in
    Fun.protect
      ~finally:(fun () -> close_out_noerr channel)
      (fun () ->
        output_string channel contents;
        Ok ())
  with
  | Sys_error message -> Error (Error.make Error.Io message)

let generate_id =
  let initialized = ref false in
  let chars = "abcdefghijklmnopqrstuvwxyz0123456789" in
  fun ?(length = 8) () ->
    if not !initialized then begin
      Random.self_init ();
      initialized := true
    end;
    String.init length (fun _ -> chars.[Random.int (String.length chars)])

type run_moment =
  { moment_run : string
  ; moment_step : float
  }

let parse_run_moment value =
  let parse_error () =
    Error
      (Error.make Error.Usage
         (Printf.sprintf
            "Could not parse passed run moment string %S, expected format '<run>?_step=<numeric_value>'."
            value))
  in
  match String.split_on_char '?' value with
  | [ run; query ] when run <> "" ->
      begin match String.split_on_char '=' query with
      | [ "_step"; step ] ->
          begin
            try Ok { moment_run = run; moment_step = float_of_string step }
            with Failure _ -> parse_error ()
          end
      | _ -> parse_error ()
      end
  | _ -> parse_error ()

let validate_project = function
  | None -> Ok ()
  | Some project ->
      if String.length project > 128 then
        Error
          (Error.make Error.Usage
             (Printf.sprintf "Invalid project name %S: exceeded 128 characters" project))
      else if string_contains_any project "/\\#?%:" then
        Error
          (Error.make Error.Usage
             (Printf.sprintf
                "Invalid project name %S: cannot contain characters %S"
                project "/\\#?%:"))
      else
        Ok ()

let validate_run_id = function
  | None -> Ok ()
  | Some run_id ->
      if run_id = "" then
        Error (Error.make Error.Usage "Run ID cannot be empty")
      else if string_has_outer_whitespace run_id then
        Error (Error.make Error.Usage "Run ID cannot start or end with whitespace")
      else if string_is_empty_or_whitespace run_id then
        Error (Error.make Error.Usage "Run ID cannot contain only whitespace")
      else if string_contains_any run_id ":;,#?/'" then
        Error (Error.make Error.Usage "Run ID cannot contain the characters: :;,#?/'")
      else
        Ok ()

let validate_tags tags =
  let rec loop index = function
    | [] -> Ok ()
    | tag :: rest ->
        let length = String.length tag in
        if length = 0 then
          Error
            (Error.make Error.Usage
               (Printf.sprintf
                  "Tag at index %d is empty. Tags must be between 1 and 64 characters"
                  index))
        else if length > 64 then
          Error
            (Error.make Error.Usage
               (Printf.sprintf
                  "Tag %S is %d characters. Tags must be between 1 and 64 characters"
                  tag length))
        else
          loop (index + 1) rest
  in
  loop 0 tags

let validate_http_url setting_name = function
  | None -> Ok ()
  | Some value ->
      let value = strip_trailing_slashes (String.trim value) in
      match Auth.url_netloc value with
      | Some _ -> Ok ()
      | None ->
          Error
            (Error.make Error.Usage
               (Printf.sprintf "invalid %s URL: %S" setting_name value))

let validate_service_wait = function
  | None -> Ok ()
  | Some value when value < 0.0 ->
      Error (Error.make Error.Usage "Service wait time cannot be negative")
  | Some _ -> Ok ()

let parse_settings_file path =
  let path = expand_user path in
  if not (Sys.file_exists path) then
    Settings.empty
  else
    match Auth.read_file path with
    | Error _ -> Settings.empty
    | Ok contents ->
        let lines = String.split_on_char '\n' contents in
        let in_default = ref false in
        let pairs = ref [] in
        List.iter
          (fun line ->
            let line = String.trim line in
            if line = "" || Util.starts_with ~prefix:"#" line || Util.starts_with ~prefix:";" line then
              ()
            else if Util.starts_with ~prefix:"[" line && ends_with ~suffix:"]" line then
              in_default := line = "[default]"
            else if !in_default then
              match String.split_on_char '=' line with
              | key :: rest when rest <> [] ->
                  pairs := (String.trim key, String.trim (String.concat "=" rest)) :: !pairs
              | _ -> ())
          lines;
        let set_string key value settings =
          match Settings.update settings [ key, Value.String value ] with
          | Ok settings -> settings
          | Error _ -> settings
        in
        List.fold_left
          (fun settings (key, value) ->
            match key with
            | "api_key" | "base_url" | "mode" | "root_dir" | "entity"
            | "project" | "run_id" | "run_name" | "run_notes"
            | "run_group" | "run_job_type" | "resume" | "http_proxy"
            | "https_proxy" -> set_string key value settings
            | "run_tags" -> { settings with Settings.run_tags = Some (Util.split_comma value) }
            | "ignore_globs" -> { settings with Settings.ignore_globs = Some (Util.split_comma value) }
            | "quiet" ->
                begin match Settings.parse_bool value with
                | Some quiet -> { settings with Settings.quiet = Some quiet }
                | None -> settings
                end
            | _ -> settings)
          Settings.empty
          !pairs

let system_settings () =
  let global =
    match Sys.getenv_opt "WANDB_CONFIG_DIR" with
    | Some dir -> Filename.concat (expand_user dir) "settings"
    | None -> Filename.concat (Filename.concat (expand_user "~") ".config/wandb") "settings"
  in
  let cwd = Sys.getcwd () in
  let local =
    let dot_wandb = Filename.concat cwd ".wandb" in
    let dir = if Sys.file_exists dot_wandb && Sys.is_directory dot_wandb then dot_wandb else Filename.concat cwd "wandb" in
    Filename.concat dir "settings"
  in
  Settings.merge (parse_settings_file global) (parse_settings_file local)

let find_git_root start =
  let rec loop dir =
    let git = Filename.concat dir ".git" in
    if Sys.file_exists git then
      Some dir
    else
      let parent = Filename.dirname dir in
      if parent = dir then None else loop parent
  in
  loop start

let auto_project_name () =
  match find_git_root (Sys.getcwd ()) with
  | None -> "uncategorized"
  | Some root -> Filename.basename root

type run_layout =
  { root_dir : string
  ; wandb_dir : string
  ; sync_dir : string
  ; sync_file : string
  ; files_dir : string
  ; logs_dir : string
  ; log_internal : string
  ; log_user : string
  }

let wandb_workspace_dir root_dir =
  let dot_wandb = Filename.concat root_dir ".wandb" in
  if Sys.file_exists dot_wandb && Sys.is_directory dot_wandb then
    dot_wandb
  else
    Filename.concat root_dir "wandb"

let timespec () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d%02d%02d_%02d%02d%02d"
    (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday
    tm.Unix.tm_hour
    tm.Unix.tm_min
    tm.Unix.tm_sec

let prepare_run_layout ~root_dir ~run_mode ~run_id =
  let* root_dir =
    match mkdir_p root_dir with
    | Ok () -> Ok root_dir
    | Error _ ->
        let temp_dir = Filename.get_temp_dir_name () in
        let* () = mkdir_p temp_dir in
        Ok temp_dir
  in
  let wandb_dir = wandb_workspace_dir root_dir in
  let* () = mkdir_p wandb_dir in
  let sync_dir =
    let base = Printf.sprintf "%s-%s-%s" run_mode (timespec ()) run_id in
    let rec candidate suffix =
      let dirname =
        match suffix with
        | 0 -> base
        | value -> base ^ "-" ^ string_of_int value
      in
      let path = Filename.concat wandb_dir dirname in
      if Sys.file_exists path then candidate (suffix + 1) else path
    in
    candidate 0
  in
  let sync_file = Filename.concat sync_dir (Printf.sprintf "run-%s.wandb" run_id) in
  let files_dir = Filename.concat sync_dir "files" in
  let logs_dir = Filename.concat sync_dir "logs" in
  let log_internal = Filename.concat logs_dir "debug-internal.log" in
  let log_user = Filename.concat logs_dir "debug.log" in
  let* () = mkdir_p files_dir in
  let* () = mkdir_p logs_dir in
  Ok
    { root_dir
    ; wandb_dir
    ; sync_dir
    ; sync_file
    ; files_dir
    ; logs_dir
    ; log_internal
    ; log_user
    }

(* Init *)

let init ?entity ?project ?dir ?id ?name ?notes ?tags ?config
    ?config_exclude_keys ?config_include_keys ?group
    ?job_type ?mode ?force ?resume ?resume_from ?fork_from () =
  let explicit_settings =
    Settings.make
      ?mode
      ?root_dir:dir
      ?run_id:id
      ?run_group:group
      ?run_job_type:job_type
      ?resume
      ()
  in
  let settings =
    Settings.default ()
    |> fun base -> Settings.merge base (system_settings ())
    |> fun base -> Settings.merge base (Settings.from_env ())
    |> fun base -> Settings.merge base explicit_settings
  in
  let mode = Option.value settings.mode ~default:Types.Mode.Online in
  let force = Option.value force ~default:false in
  let base_url = Option.value settings.base_url ~default:Util.default_base_url in
  let root_dir = Option.value settings.root_dir ~default:(Sys.getcwd ()) |> expand_user in
  let entity =
    match entity with
    | Some _ -> entity
    | None -> settings.entity
  in
  let name =
    match name with
    | Some _ -> name
    | None -> settings.run_name
  in
  let notes =
    match notes with
    | Some _ -> notes
    | None -> settings.run_notes
  in
  let tags =
    match tags with
    | Some tags -> tags
    | None -> Option.value settings.run_tags ~default:[]
  in
  let config = Option.value config ~default:Config.empty in
  let project =
    match project with
    | Some _ -> project
    | None -> settings.project
  in
  let project = Option.value project ~default:(auto_project_name ()) in
  let resume_from_moment =
    match resume_from with
    | Some value -> Result.map Option.some (parse_run_moment value)
    | None -> Ok None
  in
  let fork_from_moment =
    match fork_from with
    | Some value -> Result.map Option.some (parse_run_moment value)
    | None -> Ok None
  in
  let* resume_from_moment = resume_from_moment in
  let* fork_from_moment = fork_from_moment in
  let resume_count =
    (match settings.resume with Some _ -> 1 | None -> 0)
    + (match resume_from_moment with Some _ -> 1 | None -> 0)
    + (match fork_from_moment with Some _ -> 1 | None -> 0)
  in
  if resume_count > 1 then
    Error
      (Error.make Error.Usage
         "`fork_from`, `resume`, or `resume_from` are mutually exclusive. Please specify only one of them.")
  else if Option.is_some config_exclude_keys && Option.is_some config_include_keys then
    Error
      (Error.make Error.Usage
         "Expected at most only one of exclude or include")
  else begin
    let* base_url = normalize_base_url base_url in
    let* () = validate_project (Some project) in
    let* () = validate_run_id settings.run_id in
    let* () = validate_tags tags in
    let* () = validate_http_url "http_proxy" settings.http_proxy in
    let* () = validate_http_url "https_proxy" settings.https_proxy in
    let* () = validate_service_wait settings.service_wait_s in
    let* () =
      match settings.api_key with
      | Some key when string_has_outer_whitespace key ->
          Error (Error.make Error.Usage "API key cannot start or end with whitespace")
      | Some _ | None -> Ok ()
    in
    let* () =
      match fork_from_moment, settings.run_id with
      | Some moment, Some run_id when moment.moment_run = run_id ->
          Error
            (Error.make Error.Usage
               "Provided `run_id` is the same as the run to `fork_from`. Please provide a different `run_id` or remove the `run_id` argument. If you want to rewind the current run, please use `resume_from` instead.")
      | _ -> Ok ()
    in
    let* () =
      match resume_from_moment, settings.run_id with
      | Some moment, Some run_id when moment.moment_run <> run_id ->
          Error
            (Error.make Error.Usage
               "Both `run_id` and `resume_from` have been specified with different ids.")
      | _ -> Ok ()
    in
    let config =
      match config_include_keys with
      | Some keys ->
          Config.of_list
            (List.filter (fun (key, _) -> List.mem key keys) (Config.bindings config))
      | None ->
          match config_exclude_keys with
      | Some keys ->
              Config.of_list
                (List.filter (fun (key, _) -> not (List.mem key keys)) (Config.bindings config))
          | None -> config
    in
    let* auth = Auth.authenticate ~base_url ~mode ~force ~explicit_api_key:settings.api_key in
    let run_id =
      match settings.run_id, resume_from_moment with
      | Some run_id, _ -> run_id
      | None, Some moment -> moment.moment_run
      | None, None -> generate_id ()
    in
    let settings = { settings with Settings.run_id = Some run_id } in
    let resume_file =
      Filename.concat (wandb_workspace_dir root_dir) "wandb-resume.json"
    in
    let run_id =
      match settings.resume with
      | Some Types.Resume.Auto when Sys.file_exists resume_file ->
          begin match Auth.read_file resume_file with
          | Ok contents ->
              if String.contains contents '"' && Util.starts_with ~prefix:"{" (String.trim contents)
              then
                let pieces = String.split_on_char '"' contents in
                let rec find = function
                  | key :: _sep :: value :: _ when key = "run_id" -> Some value
                  | _ :: rest -> find rest
                  | [] -> None
                in
                Option.value (find pieces) ~default:run_id
              else
                run_id
          | Error _ -> run_id
          end
      | _ -> run_id
    in
    let* () = validate_run_id (Some run_id) in
    let entity = entity in
    let project = project in
    let name = name in
    let run_mode =
      match mode with
      | Types.Mode.Offline -> "offline-run"
      | Types.Mode.Online | Types.Mode.Shared | Types.Mode.Disabled -> "run"
    in
    if mode = Types.Mode.Disabled then begin
      let run =
        create
          ~id:run_id
          ~name:("dummy-" ^ run_id)
          ~entity:"dummy"
          ~project:"dummy"
          ~root_dir:(Filename.get_temp_dir_name ())
          ~tags:[]
          ~config
          ~mode:Types.Mode.Disabled
          ~base_url
          ()
      in
      Ok run
    end else begin
      let* layout = prepare_run_layout ~root_dir ~run_mode ~run_id in
      let resume_file = Filename.concat layout.wandb_dir "wandb-resume.json" in
      let* () =
        match settings.resume with
        | Some Types.Resume.Auto ->
            let* () = mkdir_p (Filename.dirname resume_file) in
            write_file resume_file
              (Printf.sprintf "{\"run_id\":\"%s\"}\n" run_id)
        | _ -> Ok ()
      in
      let core_result =
        match mode, auth with
        | (Types.Mode.Online | Types.Mode.Shared), Some api_key ->
            let conn_result =
              Core.start
                ~base_url
                ~api_key
                ~entity
                ~project
                ~run_id
                ~root_dir:layout.root_dir
                ~wandb_dir:layout.wandb_dir
                ~sync_dir:layout.sync_dir
                ~sync_file:layout.sync_file
                ~log_dir:layout.logs_dir
                ~log_internal:layout.log_internal
                ~log_user:layout.log_user
                ?run_name:name
                ?notes
                ~tags
                ~run_mode
                ?mode:(Some (Types.Mode.to_string mode))
                ()
            in
            (match conn_result with
             | Ok conn -> Ok (Some conn)
             | Error (`Msg msg) ->
                 Error (Error.make Error.Communication msg))
        | (Types.Mode.Online | Types.Mode.Shared), None ->
            Error
              (Error.make Error.Unsupported
                 "WANDB_IDENTITY_TOKEN_FILE not supported by wandb-ocaml.")
        | (Types.Mode.Offline | Types.Mode.Disabled), _ -> Ok None
      in
      let* core = core_result in
      let* () =
        match core with
        | Some c ->
            begin match Core.init_run c ~id:run_id ?entity ~project ?name ?notes ~tags ~config ~stream_id:run_id () with
            | Ok () -> Ok ()
            | Error (`Msg msg) -> Error (Error.make Error.Communication msg)
            end
        | None -> Ok ()
      in
      let run =
        create
          ~id:run_id
          ?name
          ?entity
          ~project
          ~root_dir:layout.root_dir
          ~sync_dir:layout.sync_dir
          ~tags
          ~config
          ~mode
          ~base_url
          ()
      in
      set_core run core;
      Ok run
    end
  end
