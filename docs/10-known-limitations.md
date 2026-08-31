# 10. Known limitations and pitfalls

## Language/VM

- **Leaked FOR frames.** Jumping out of a `FOR ... NEXT` (e.g. `GOTO` before
  the `NEXT`) leaves the frame on the `loops` stack; a later `NEXT` may then
  miscount. The loop-body model (`loop_body` = line after FOR line) also makes
  the loop re-enter after the body; this is a documented limitation, not
  intended to grow arbitrarily.
- **One-dimensional numeric arrays only**; `DIM` required before use;
  dimensions fixed at `DIM` time; whole-array assignment not allowed.
- **`Call` vs `ArrayAccess` by name.** Any identifier in `builtins` used with
  parentheses is treated as a function call — you cannot name an array
  `LEN`, `ABS`, `INT`, `RND` or `MOD`.
- **`int_of_float` truncation** (used by `MOD`, `DIM` sizes and array
  indices). Values outside the OCaml `int` range would raise; not handled.
- **Immediate-mode limits** in the REPL (see [8. The REPL](08-repl.md)).
- **Case sensitivity**: BASIC keywords must be uppercase; identifiers are
  case-sensitive.

## Design/process

- Warnings compile as errors: extended sum types require updating all
  matches and `printer.ml`.
- `Printer` output is asserted by the parser tests; keep it canonical.
- `.mli` files gate the public surface; a function used only internally
  becomes a warning (unused) if it disappears from the `.mli` — remove or
  expose intentionally.
- Error messages live in the same style across `lexer`, `parser`, `eval`,
  `machine`; keep them in English and prefixed by their kind/line through
  `Error.to_string`.
- The test suite shares names with library modules (`Machine`, `Parser`,
  `Printer`, ...) via `open Basicaml`; when adding modules, remember they
  become available to tests and to the executable without extra wiring beyond
  `src/dune`.
