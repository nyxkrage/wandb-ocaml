open Wandb_proto

module W = struct
  include Wandb_internal.Wandb_internal
end

module WS = struct
  include Wandb_server.Wandb_internal
end

module WSettings = struct
  include Wandb_settings.Wandb_internal
end

module WBase = struct
  include Wandb_base.Wandb_internal
end

module Wrappers = struct
  include Wrappers.Google.Protobuf
end

let default_core_path =
  match Sys.getenv_opt "WANDB_CORE" with
  | Some p -> p
  | None -> "wandb-core"

(* Lock-free SPSC record queue. *)

let queue_capacity = 8192

type queued_record =
  { record : W.Record.t
  ; request_id : string option
  }

type record_queue =
  { buffer : queued_record option array
  ; head : int Atomic.t
  ; tail : int Atomic.t
  }

let make_queue () =
  { buffer = Array.make queue_capacity None
  ; head = Atomic.make 0
  ; tail = Atomic.make 0
  }

let queue_push q record =
  let t = Atomic.get q.tail in
  let next = (t + 1) mod queue_capacity in
  if Atomic.get q.head = next then
    false
  else begin
    q.buffer.(t) <- Some record;
    Atomic.set q.tail next;
    true
  end

let queue_pop q =
  let h = Atomic.get q.head in
  if h = Atomic.get q.tail then None
  else begin
    let record = match q.buffer.(h) with Some r -> r | None -> assert false in
    q.buffer.(h) <- None;
    Atomic.set q.head ((h + 1) mod queue_capacity);
    Some record
  end

(* Connection state. *)

type t =
  { pid : int
  ; sock_fd : Unix.file_descr
  ; queue : record_queue
  ; responses : (string * WS.ServerResponse.t) list Atomic.t
  ; wake_wr : Unix.file_descr
  ; io_domain : unit Domain.t
  ; wake_rd : Unix.file_descr
  ; mutable closed : bool
  }

module Protocol = struct
  exception Timeout

  let magic_byte = Char.code 'W'
  let write_timeout_s = 5.0

  let write_all fd buf ofs len =
    let deadline = Unix.gettimeofday () +. write_timeout_s in
    let written = ref ofs in
    let end_pos = ofs + len in
    while !written < end_pos do
      (try
         let n = Unix.single_write fd buf !written (end_pos - !written) in
         written := !written + n
       with Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> ());
      if !written < end_pos then
        let remaining = deadline -. Unix.gettimeofday () in
        if remaining <= 0.0 then
          raise Timeout
        else
          let _, writable, _ = Unix.select [] [ fd ] [] remaining in
          if writable = [] then
            raise Timeout
    done

  let send_message fd bytes =
    let len = String.length bytes in
    let header = Bytes.create 5 in
    Bytes.set header 0 (Char.chr magic_byte);
    Bytes.set_int32_le header 1 (Int32.of_int len);
    write_all fd header 0 5;
    write_all fd (Bytes.unsafe_of_string bytes) 0 len

  let server_request ?request_id request_type =
    WS.ServerRequest.make ~server_request_type:request_type ?request_id ()

  let record_to_message ?request_id record =
    let request = server_request ?request_id (`Record_publish record) in
    request |> WS.ServerRequest.to_proto |> Ocaml_protoc_plugin.Writer.contents

  let inform_init_to_message inform_init =
    let request = server_request (`Inform_init inform_init) in
    request |> WS.ServerRequest.to_proto |> Ocaml_protoc_plugin.Writer.contents

  let inform_finish_to_message finish =
    let request = server_request (`Inform_finish finish) in
    request |> WS.ServerRequest.to_proto |> Ocaml_protoc_plugin.Writer.contents

  let inform_teardown_to_message teardown =
    let request = server_request (`Inform_teardown teardown) in
    request |> WS.ServerRequest.to_proto |> Ocaml_protoc_plugin.Writer.contents

  let read_uint32_le data offset =
    let byte index = Char.code data.[offset + index] in
    byte 0
    lor (byte 1 lsl 8)
    lor (byte 2 lsl 16)
    lor (byte 3 lsl 24)

  let take_frames pending =
    let rec loop acc data =
      let data_len = String.length data in
      if data_len < 5 then
        (List.rev acc, data)
      else if Char.code data.[0] <> magic_byte then
        (List.rev acc, "")
      else
        let frame_len = read_uint32_le data 1 in
        let total_len = 5 + frame_len in
        if data_len < total_len then
          (List.rev acc, data)
        else
          let frame = String.sub data 5 frame_len in
          let rest = String.sub data total_len (data_len - total_len) in
          loop (frame :: acc) rest
    in
    loop [] pending

  let response_of_message bytes =
    match WS.ServerResponse.from_proto (Ocaml_protoc_plugin.Reader.create bytes) with
    | Ok response -> Some response
    | Error _ -> None
end

let poll_port_file path timeout_s =
  let deadline = Unix.gettimeofday () +. timeout_s in
  let rec loop () =
    if Unix.gettimeofday () > deadline then
      Error (`Msg (Printf.sprintf "Timeout waiting for port file %s" path))
    else if not (Sys.file_exists path) then begin
      Unix.sleepf 0.1;
      loop ()
    end else begin
      let content =
        try Some (In_channel.with_open_text path In_channel.input_all)
        with _ -> None
      in
      match content with
      | None ->
          Unix.sleepf 0.1; loop ()
      | Some text ->
          let lines = String.split_on_char '\n' text in
          if List.mem "EOF" (List.map String.trim lines) then
            match
              List.find_map (fun line ->
                let line = String.trim line in
                if String.length line > 5 && String.sub line 0 5 = "unix=" then
                  Some (String.sub line 5 (String.length line - 5))
                else None
              ) lines
            with
            | Some sock_path -> Ok sock_path
            | None -> Error (`Msg (Printf.sprintf "No unix= line in port file: %s" text))
          else begin
            Unix.sleepf 0.1; loop ()
          end
    end
  in
  loop ()

let record_info stream_id = WBase.P_RecordInfo.make ~stream_id ()

let request_info stream_id = WBase.P_RequestInfo.make ~stream_id ()

let local_control () =
  W.Control.make ~local:true ()

let history_items values =
  List.map
    (fun (key, value) ->
      let value_json = Yojson.Safe.to_string (Util.value_to_yojson value) in
      W.HistoryItem.make ~key ~value_json ())
    values

let config_record config =
  let update =
    List.map
      (fun (key, value) ->
        W.ConfigItem.make
          ~key
          ~value_json:(Yojson.Safe.to_string (Util.value_to_yojson value))
          ())
      (Config.bindings config)
  in
  W.ConfigRecord.make ~update ()

let run_record ~id ?entity ~project ?name ?notes ~tags ~config () =
  W.RunRecord.make
    ~run_id:id
    ?entity
    ~project
    ~config:(config_record config)
    ?display_name:name
    ?notes
    ~tags
    ~_info:(record_info id)
    ()

let run_publish_record run_record stream_id =
  W.Record.make
    ~record_type:(`Run run_record)
    ~_info:(record_info stream_id)
    ()

let run_start_record run_record stream_id =
  let request =
    W.RunStartRequest.make
      ~run:run_record
      ~_info:(request_info stream_id)
      ()
  in
  let request = W.Request.make ~request_type:(`Run_start request) () in
  W.Record.make
    ~record_type:(`Request request)
    ~control:(local_control ())
    ~_info:(record_info stream_id)
    ()

let partial_history_record ~step ~flush ~stream_id values =
  let step =
    match step with
    | Some num -> Some (W.HistoryStep.make ~num ())
    | None -> None
  in
  let request =
    W.PartialHistoryRequest.make
      ~item:(history_items values)
      ?step
      ~action:(W.HistoryAction.make ~flush ())
      ~_info:(request_info stream_id)
      ()
  in
  let request = W.Request.make ~request_type:(`Partial_history request) () in
  W.Record.make
    ~record_type:(`Request request)
    ~control:(local_control ())
    ~_info:(record_info stream_id)
    ()

let exit_record ~exit_code ~stream_id =
  let exit_rec =
    W.RunExitRecord.make ~exit_code ~_info:(record_info stream_id) ()
  in
  W.Record.make
    ~record_type:(`Exit exit_rec)
    ~_info:(record_info stream_id)
    ()

let string_value value =
  Wrappers.StringValue.make ~value ()

let bool_value value =
  Wrappers.BoolValue.make ~value ()

let optional_bool enabled =
  if enabled then Some (bool_value true) else None

let optional_string = function
  | Some value -> Some (string_value value)
  | None -> None

let optional_string_list = function
  | Some (_ :: _ as values) -> Some (WSettings.ListStringValue.make ~value:values ())
  | Some [] | None -> None

let remove_file_noerr path =
  try Sys.remove path with _ -> ()

let stop_process_noerr pid =
  (try Unix.kill pid Sys.sigterm with _ -> ());
  (try ignore (Unix.waitpid [] pid) with _ -> ())

let connect_socket sock_path =
  let sock_fd = Unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  try
    Unix.connect sock_fd (Unix.ADDR_UNIX sock_path);
    Unix.set_nonblock sock_fd;
    Ok sock_fd
  with exn ->
    Unix.close sock_fd;
    Error
      (`Msg
        (Printf.sprintf "Could not connect to wandb-core socket %S: %s"
           sock_path (Printexc.to_string exn)))

let store_response responses response =
  let request_id = response.WS.ServerResponse.request_id in
  if request_id <> "" then
    let rec loop () =
      let current = Atomic.get responses in
      if not (Atomic.compare_and_set responses current ((request_id, response) :: current)) then
        loop ()
    in
    loop ()

let take_response responses request_id =
  let rec remove acc = function
    | [] -> None
    | (id, response) :: rest when id = request_id ->
        Some (response, List.rev_append acc rest)
    | item :: rest -> remove (item :: acc) rest
  in
  let rec loop () =
    let current = Atomic.get responses in
    match remove [] current with
    | None -> None
    | Some (response, next) ->
        if Atomic.compare_and_set responses current next then
          Some response
        else
          loop ()
  in
  loop ()

let wait_for_response responses request_id timeout_s =
  let deadline = Unix.gettimeofday () +. timeout_s in
  let rec loop () =
    match take_response responses request_id with
    | Some response -> Ok response
    | None ->
        if Unix.gettimeofday () >= deadline then
          Error (`Msg (Printf.sprintf "Timed out waiting for wandb-core response %s" request_id))
        else begin
          Unix.sleepf 0.01;
          loop ()
        end
  in
  loop ()

let response_error response =
  match response.WS.ServerResponse.server_response_type with
  | `Error_response message when message <> "" ->
      Some message
  | `Result_communicate result ->
      begin match result.result_type with
      | `Run_result run_result ->
          begin match run_result.error with
          | Some error when error.message <> "" -> Some error.message
          | _ -> None
          end
      | _ -> None
      end
  | _ -> None

let request_counter = Atomic.make 0

let next_request_id () =
  let index = Atomic.fetch_and_add request_counter 1 in
  Printf.sprintf "ocaml:%d:%d" (Unix.getpid ()) index

let with_response_mailbox request_id record =
  let control =
    match record.W.Record.control with
    | Some control -> { control with req_resp = true; mailbox_slot = request_id }
    | None -> W.Control.make ~req_resp:true ~mailbox_slot:request_id ()
  in
  { record with W.Record.control = Some control }

(* I/O domain. *)

let io_loop sock_fd wake_rd queue responses =
  let buf = Bytes.create 65536 in
  let pending = ref "" in
  try
    while true do
      let rec drain_queue () =
        match queue_pop queue with
        | None -> ()
        | Some { record; request_id } ->
            let bytes = Protocol.record_to_message ?request_id record in
            Protocol.send_message sock_fd bytes;
            drain_queue ()
      in
      drain_queue ();
      let rds, _, _ = Unix.select [sock_fd; wake_rd] [] [] (-1.0) in
      if List.mem sock_fd rds then
        (try
           while true do
             let read = Unix.read sock_fd buf 0 65536 in
             if read = 0 then
               raise Exit;
             pending := !pending ^ Bytes.sub_string buf 0 read;
             let frames, rest = Protocol.take_frames !pending in
             pending := rest;
             List.iter
               (fun frame ->
                 match Protocol.response_of_message frame with
                 | Some response -> store_response responses response
                 | None -> ())
               frames
           done
         with Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> ());
      if List.mem wake_rd rds then
        let n = Unix.read wake_rd buf 0 1 in
        if n = 0 then raise Exit
    done
  with
  | Exit -> ()
  | Protocol.Timeout -> ()
  | Unix.Unix_error _ -> ()

(* Start / connect. *)

let start ?core_path ?root_dir ?wandb_dir ?sync_dir ?sync_file ?log_dir
    ?log_internal ?log_user ~base_url ~api_key ~entity ~project ~run_id
    ?run_name ?tags ?notes ?run_mode ?mode () =
  let core_path = Option.value core_path ~default:default_core_path in
  let port_file = Filename.temp_file "wandb_port_" ".txt" in
  let pid = Unix.getpid () in
  let args =
    [| core_path; "--port-filename"; port_file; "--pid"; string_of_int pid; "--log-level"; "-4" |]
  in
  match
    try Ok (Unix.create_process core_path args Unix.stdin Unix.stdout Unix.stderr)
    with exn ->
      Error
        (`Msg
          (Printf.sprintf "Could not launch wandb-core at %S: %s" core_path
             (Printexc.to_string exn)))
  with
  | Error e ->
      remove_file_noerr port_file;
      Error e
  | Ok child_pid ->
      match poll_port_file port_file 30.0 with
      | Error e ->
          stop_process_noerr child_pid;
          remove_file_noerr port_file;
          Error e
      | Ok sock_path ->
          begin match connect_socket sock_path with
          | Error e ->
              stop_process_noerr child_pid;
              remove_file_noerr port_file;
              Error e
          | Ok sock_fd ->
              remove_file_noerr port_file;
              let queue = make_queue () in
              let responses = Atomic.make [] in
              let wake_rd, wake_wr = Unix.pipe ~cloexec:true () in
              let settings =
                WSettings.Settings.make
                  ~api_key:(string_value api_key)
                  ~base_url:(string_value base_url)
                  ?entity:(optional_string entity)
                  ~project:(string_value project)
                  ~run_id:(string_value run_id)
                  ?run_name:(optional_string run_name)
                  ?run_notes:(optional_string notes)
                  ?run_tags:(optional_string_list tags)
                  ?root_dir:(optional_string root_dir)
                  ?wandb_dir:(optional_string wandb_dir)
                  ?sync_dir:(optional_string sync_dir)
                  ?sync_file:(optional_string sync_file)
                  ?log_dir:(optional_string log_dir)
                  ?_offline:(optional_bool (mode = Some "offline"))
                  ?_shared:(optional_bool (mode = Some "shared"))
                  ?mode:(optional_string mode)
                  ?run_mode:(optional_string run_mode)
                  ?log_internal:(optional_string log_internal)
                  ?log_user:(optional_string log_user)
                  ~x_primary:(bool_value true)
                  ()
              in
              let inform_init =
                WS.ServerInformInitRequest.make ~settings
                  ~_info:(record_info run_id)
                  ()
              in
              Protocol.send_message sock_fd
                (Protocol.inform_init_to_message inform_init);
              let io_domain =
                Domain.spawn (fun () -> io_loop sock_fd wake_rd queue responses)
              in
              let conn =
                { pid = child_pid
                ; sock_fd
                ; queue
                ; responses
                ; wake_wr
                ; wake_rd
                ; io_domain
                ; closed = false
                }
              in
              Ok conn
          end

(* Send record (fire-and-forget). *)

let enqueue conn item =
  if not conn.closed then begin
    if queue_push conn.queue item then begin
      (try Unix.write conn.wake_wr (Bytes.make 1 'x') 0 1 |> ignore with _ -> ());
      true
    end else
      false
  end else
    false

let send_record conn (record : W.Record.t) =
  ignore (enqueue conn { record; request_id = None })

let send_record_and_wait ?(timeout_s = 30.0) conn (record : W.Record.t) =
  if conn.closed then
    Error (`Msg "wandb-core connection is closed")
  else
    let request_id = next_request_id () in
    let record = with_response_mailbox request_id record in
    if not (enqueue conn { record; request_id = Some request_id }) then
      Error (`Msg "wandb-core record queue is full")
    else
      let response = wait_for_response conn.responses request_id timeout_s in
      match response with
      | Error _ as error -> error
      | Ok response ->
          begin match response_error response with
          | Some message -> Error (`Msg message)
          | None -> Ok ()
          end

(* Finish. *)

let finish conn ~exit_code ~stream_id =
  if conn.closed then () else begin
    conn.closed <- true;
    (try Unix.close conn.wake_wr with _ -> ());
    Domain.join conn.io_domain;
    (try Unix.close conn.wake_rd with _ -> ());
    let finish_req =
      WS.ServerInformFinishRequest.make ~_info:(record_info stream_id) ()
    in
    (try
       Protocol.send_message conn.sock_fd
         (Protocol.inform_finish_to_message finish_req)
     with _ -> ());
    let teardown_req = WS.ServerInformTeardownRequest.make ~exit_code () in
    (try
       Protocol.send_message conn.sock_fd
         (Protocol.inform_teardown_to_message teardown_req)
     with _ -> ());
    (try Unix.close conn.sock_fd with _ -> ());
    (try ignore (Unix.waitpid [ Unix.WNOHANG ] conn.pid) with _ -> ())
  end

let init_run t ~id ?entity ~project ?name ?notes ~tags ~config ~stream_id () =
  let run_rec = run_record ~id ?entity ~project ?name ?notes ~tags ~config () in
  match send_record_and_wait t (run_publish_record run_rec stream_id) with
  | Ok () ->
      send_record t (run_start_record run_rec stream_id);
      Ok ()
  | Error _ as e -> e

let log t ~stream_id ~values ~step ~flush =
  send_record t (partial_history_record ~step ~flush ~stream_id values)

let finish_run t ~stream_id ~exit_code =
  send_record_and_wait t (exit_record ~exit_code ~stream_id)
