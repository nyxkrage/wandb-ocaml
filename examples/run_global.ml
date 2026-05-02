let ( let* ) = Result.bind

type batch =
  { features : float array
  ; labels : int array
  }

type model =
  { mutable step : int
  ; mutable train_loss_level : float
  ; mutable val_loss_level : float
  ; mutable accuracy_level : float
  }

let noise scale =
  (Random.float (2.0 *. scale)) -. scale

let next_batch () =
  { features = Array.init 128 (fun _ -> Random.float 1.0)
  ; labels = Array.init 128 (fun _ -> Random.int 10)
  }

let create_model () =
  { step = 0
  ; train_loss_level = 2.2
  ; val_loss_level = 2.35
  ; accuracy_level = 0.18
  }

let train_step model _batch =
  model.step <- model.step + 1;
  model.train_loss_level <- max 0.08 ((model.train_loss_level *. 0.992) +. noise 0.015);
  model.accuracy_level <- min 0.96 ((model.accuracy_level +. 0.0025) +. noise 0.004);
  ( model.train_loss_level +. noise 0.03
  , model.accuracy_level +. noise 0.01
  )

let validate model =
  model.val_loss_level <- max 0.12 ((model.val_loss_level *. 0.985) +. noise 0.025);
  let validation_loss = model.val_loss_level +. noise 0.04 in
  let validation_accuracy = min 0.94 (model.accuracy_level -. 0.025 +. noise 0.012) in
  (validation_loss, validation_accuracy)

let train () =
  let model = create_model () in
  let total_steps = 1000 in
  let validation_every = 100 in
  let rec loop () =
    if model.step >= total_steps then
      Ok ()
    else
      let batch = next_batch () in
      let train_loss, train_accuracy = train_step model batch in
      let step = model.step in
      let* () =
        Wandb.log ~step
          [ "trainer/global_step", Wandb.Value.int step
          ; "train/loss", Wandb.Value.float train_loss
          ; "train/accuracy", Wandb.Value.float train_accuracy
          ; "optimizer/learning_rate", Wandb.Value.float (0.001 *. (0.999 ** float_of_int step))
          ]
      in
      if step mod validation_every = 0 then
        let validation_loss, validation_accuracy = validate model in
        let* () =
          Wandb.log ~step
            [ "validation/loss", Wandb.Value.float validation_loss
            ; "validation/accuracy", Wandb.Value.float validation_accuracy
            ]
        in
        loop ()
      else
        loop ()
  in
  loop ()

let main () =
  Random.self_init ();
  let config =
    Wandb.Config.of_list
      [ "model", Wandb.Value.string "synthetic-mlp"
      ; "dataset", Wandb.Value.string "generated-batches"
      ; "total_steps", Wandb.Value.int 1_000
      ; "validation_every", Wandb.Value.int 100
      ; "optimizer", Wandb.Value.string "adamw"
      ; "learning_rate", Wandb.Value.float 0.001
      ; "batch_size", Wandb.Value.int 128
      ]
  in
  let* run =
    Wandb.init
      ~project:"ocaml-api-smoke-test"
      ~name:"ocaml-global-tracked"
      ~notes:"Run managed via global Wandb.log / Wandb.finish."
      ~tags:[ "ocaml"; "global" ]
      ~config
      ()
  in
  let result = train () in
  match result with
  | Ok () -> Wandb.finish ()
  | Error _ -> let _ = Wandb.finish ~exit_code:1 () in result

let () =
  match main () with
  | Ok () ->
      Printf.printf "Finished global-tracked training run.\n%!"
  | Error err ->
      Printf.eprintf "Global-tracked run failed: %s\n%!" (Wandb.Error.to_string err);
      exit 1
