# 11. Versioning and packaging

`basicaml.opam` declares OCaml >= 4.14, Dune >= 3.18, `alcotest >= 1.7`
(with-test). The library is `public_name basicaml`; the executable is also
published as `basicaml`. Version is `0.1.0` (also hard-coded in `main.ml` for
`--version`). Bump both when cutting releases.
