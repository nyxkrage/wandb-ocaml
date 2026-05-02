# wandb-ocaml

Unofficial [Weights & Biases](https://wandb.ai) SDK for OCaml.

**Status:** Early development. Currently supports logging scalar metrics with the wandb-core backend. The API is subject to change.

## Installation

1. Install the wandb-ocaml package:

```bash
opam install wandb-ocaml
```

2. Set up authentication:

- Option 1: Set your API key via environment variable:

```bash
export WANDB_API_KEY=your-api-key
```

- Option 2 (recommended): Log in with the official W&B CLI:

```bash
uv run -w wandb wandb login
```

wandb-ocaml will pick up the credentials from `~/.netrc` after logging in.

## Usage

wandb-ocaml provides three different ways to manage runs:

1. **Explicit Run Handle**: You create and manage the run explicitly using `Wandb.Run.init`, `Wandb.Run.log`, and `Wandb.Run.finish`. This gives you full control over the run lifecycle.

2. **Global Tracked Run**: You initialize a global run using `Wandb.init` and then use `Wandb.log` and `Wandb.finish` to log metrics and finish the run. This is useful when you have a single run throughout your program.

3. **Scoped Lifecycle**: You use `Wandb.with_run` to create a scoped run within a specific function. The run is automatically finished when the function completes. This is convenient for encapsulating the run within a specific context.

Choose the approach that best fits your use case and coding style.

### Explicit Run Handle

```ocaml
let* run = Wandb.Run.init ~project:"my-project" ~config () in
for step = 1 to 10 do
  let* () = Wandb.Run.log ~step run [ "loss", Value.float 0.1 ] in
  ()
done;
Wandb.Run.finish run
```

### Global Tracked Run

```ocaml
let* _run = Wandb.init ~project:"my-project" ~config () in
for step = 1 to 10 do
  let* () = Wandb.log ~step [ "loss", Value.float 0.1 ] in
  ()
done;
Wandb.finish ()
```

### Scoped Lifecycle

```ocaml
Wandb.with_run ~project:"my-project" ~config ~f:(fun run ->
  for step = 1 to 10 do
    let* () = Wandb.Run.log ~step run [ "loss", Value.float 0.1 ] in
    ()
  done;
  Ok ())
```

## Examples

### XOR Training with Kaun

The `run_xor.ml` example demonstrates a full training loop using the [Kaun](https://github.com/raven-ml/raven) neural network library. It trains a tiny MLP (2→4→1) on the XOR problem and logs the loss and learning rate every 5 steps using a warmup+cosine schedule.

To run the example:

```bash
dune exec examples/run_xor.exe
```

## Build

```bash
dune build
dune exec examples/run_explicit.exe
dune exec examples/run_global.exe 
dune exec examples/run_scope.exe
```

## Contributing

Contributions to wandb-ocaml are welcome! If you encounter any issues or have suggestions for improvements, please open an issue or submit a pull request on the [GitHub repository](https://github.com/your-repo/wandb-ocaml).

## License

wandb-ocaml is released under the [MIT License](LICENSE).