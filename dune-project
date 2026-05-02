(lang dune 3.22)

(name wandb)

(generate_opam_files true)

(source
 (github nyxkrage/wandb-ocaml))

(authors "Carsten Kragelund <carsten@kragelund.dev>")

(maintainers "Carsten Kragelund <carsten@kragelund.dev>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name wandb)
  (version 0.0.1)
 (synopsis "Unofficial Weights & Biases SDK for OCaml")
 (description "wandb-ocaml is an unofficial OCaml SDK for Weights & Biases, a platform for experiment tracking, dataset versioning, and model management. The SDK provides a simple and intuitive API for logging metrics, hyperparameters, and artifacts to Weights & Biases during the training process of machine learning models.")
 (depends
  (ocaml (>= 5.2))
  dune
  (yojson (>= 3.0.0))
  (ocaml-protoc-plugin (>= 6.2.0))
  (kaun :with-dev-setup))
 (tags
  ("machine learning" "experiment tracking" "wandb" "ocaml" "sdk" "metrics" "hyperparameters" "artifacts" "visualization")))