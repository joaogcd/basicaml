# 7. Conventions for adding a feature

Every new language feature must touch, in order (mirrors
`basicaml-improvement-plan.md` section 6):

1. **`token.ml`** — a token variant if a new keyword/symbol is needed.
2. **`lexer.ml`** — a rule in `tokenize`, plus a `keyword_or_ident` entry.
3. **`ast.ml`** — new `expr` / `command` constructors (and therefore every
   `match` downstream, including `printer.ml`).
4. **`parser.ml`** — the parse rule; if it changes expression syntax, extend
   the precedence chain; commands must consume the whole token stream.
5. **`eval.ml` / `machine.ml`** — the evaluation/execution semantics.
6. **`printer.ml`** — update the pretty-printer to the canonical format.
7. **Tests** — at least one case per layer: lexer, parser, eval, machine
   (and REPL if user-facing). See [9. Testing](09-testing.md).
8. **`.mli`** — the interface must be updated if you expose a new value or
   change a signature.

When you extend a **sum type**, fix every `match` that becomes
non-exhaustive; with warnings-as-errors the build will fail until you do.

Miscellaneous conventions:

- Identifiers and keywords are uppercase; keep error messages in English,
  consistent with the code.
- Comments in code are minimal by design; prefer an `.mli` doc comment for
  public invariants.
- Keep modules acyclic and the layer-like dependency order.
