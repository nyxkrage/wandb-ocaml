(* Train a small MLP on XOR with Kaun, logging metrics to W&B. *)

open Kaun
let ( let* ) = Result.bind

let () =
  let dtype = Nx.float32 in
  let x = Nx.create dtype [| 4; 2 |] [| 0.; 0.; 0.; 1.; 1.; 0.; 1.; 1. |] in
  let y = Nx.create dtype [| 4; 1 |] [| 0.; 1.; 1.; 0. |] in
  let peak_lr = 0.01 in
  let warmup_steps = 100 in
  let lr_schedule step =
    if step < warmup_steps then
      float_of_int step /. float_of_int warmup_steps *. peak_lr
    else
      let decay_steps = 1000 - warmup_steps in
      let ratio = float_of_int (step - warmup_steps) /. float_of_int decay_steps in
      let cosine = 0.5 *. (1. +. Float.cos (Float.pi *. ratio)) in
      peak_lr *. cosine
  in
  let model =
    Layer.sequential
      [ Layer.linear ~in_features:2 ~out_features:4 ()
      ; Layer.tanh ()
      ; Layer.linear ~in_features:4 ~out_features:1 ()
      ]
  in
  let trainer =
    Train.make ~model ~optimizer:(Optim.adam ~lr:lr_schedule ())
  in
  let result =
    Wandb.with_run
      ~project:"ocaml-kaun-xor"
      ~name:"xor-mlp"
      ~config:(Wandb.Config.of_list
        [ "model", Wandb.Value.string "mlp-2-4-1"
        ; "optimizer", Wandb.Value.string "adam"
        ; "learning_rate", Wandb.Value.float peak_lr
        ; "epochs", Wandb.Value.int 1000
        ])
      ~f:(fun run ->
        let st = Train.init trainer ~dtype in
        let st = Train.fit trainer st
          ~report:(fun ~step ~loss _st ->
            if step mod 5 = 0 then begin
              let lr = lr_schedule step in
              ignore (Wandb.Run.log ~step run
                [ "train/loss", Wandb.Value.float loss
                ; "train/lr", Wandb.Value.float lr
                ]);
              Printf.printf "step %4d  loss %.6f  lr %.5f\n%!" step loss lr
            end)
          (Data.repeat 1000 (x, fun pred -> Loss.binary_cross_entropy pred y))
        in
        let pred = Train.predict trainer st x |> Nx.sigmoid in
        for i = 0 to 3 do
          Printf.printf "  [%.0f, %.0f] -> %.3f\n%!"
            (Nx.item [ i; 0 ] x) (Nx.item [ i; 1 ] x) (Nx.item [ i; 0 ] pred)
        done;
        Ok ()) ()
  in
  match result with
  | Ok () -> print_endline "Training complete."
  | Error e ->
    Printf.eprintf "Training failed: %s\n%!" (Wandb.Error.to_string e);
    exit 1
