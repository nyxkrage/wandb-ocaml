let fail message =
  prerr_endline message;
  exit 1

let expect_ok = function
  | Ok value -> value
  | Error err -> fail (Wandb.Error.to_string err)

let expect_error_kind expected = function
  | Ok _ -> fail "expected an error"
  | Error err when Wandb.Error.kind err = expected -> ()
  | Error err ->
      fail
        (Printf.sprintf "unexpected error kind: %s" (Wandb.Error.to_string err))

let temp_dir name =
  Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "wandb-ocaml-%s-%d" name (Unix.getpid ()))

let test_mode_parsing () =
  begin match Wandb.Mode.of_string "dryrun" with
  | Ok Wandb.Mode.Offline -> ()
  | Ok _ -> fail "dryrun should parse as offline"
  | Error err -> fail (Wandb.Error.to_string err)
  end;
  expect_error_kind Wandb.Error.Usage (Wandb.Mode.of_string "invalid")

let test_offline_init () =
  let config =
    Wandb.Config.of_list
      [ "learning_rate", Wandb.Value.float 0.01
      ; "epochs", Wandb.Value.int 3
      ]
  in
  let run =
    expect_ok
      (Wandb.init
         ~mode:Wandb.Mode.Offline
         ~dir:(temp_dir "offline")
         ~project:"ocaml-test"
         ~id:"offline-run"
         ~name:"offline smoke"
         ~tags:[ "ocaml"; "test" ]
         ~config
         ())
  in
  assert (Wandb.Run.id run = "offline-run");
  assert (Wandb.Run.name run = Some "offline smoke");
  assert (Wandb.Run.project run = Some "ocaml-test");
  assert (Wandb.Run.tags run = [ "ocaml"; "test" ]);
  let run_path run =
    let id = Wandb.Run.id run in
    match Wandb.Run.entity run, Wandb.Run.project run with
    | Some entity, Some project -> Some (String.concat "/" [ entity; project; id ])
    | None, Some project -> Some (String.concat "/" [ project; id ])
    | _ -> None
  in
  assert (run_path run = Some "ocaml-test/offline-run");
  assert (Wandb.Run.status run = Ok Wandb.Run.Status.Offline);
  assert (Wandb.Run.url run = Ok None);
  expect_ok
    (Wandb.Run.log ~commit:false run
       [ "loss", Wandb.Value.float 1.0 ]);
  expect_ok (Wandb.Run.finish run);
  assert (Wandb.Run.status run = Ok Wandb.Run.Status.Finished)

let test_disabled_init () =
  let run =
    expect_ok
      (Wandb.init
         ~mode:Wandb.Mode.Disabled
         ~project:"ignored"
         ~id:"disabled-run"
         ())
  in
  assert (Wandb.Run.status run = Ok Wandb.Run.Status.Disabled);
  assert (Wandb.Run.url run = Ok None);
  expect_ok (Wandb.Run.finish run)

let test_validation () =
  expect_error_kind Wandb.Error.Usage
    (Wandb.init
       ~mode:Wandb.Mode.Offline
       ~project:"bad/project"
       ())

let () =
  test_mode_parsing ();
  test_offline_init ();
  test_disabled_init ();
  test_validation ()
