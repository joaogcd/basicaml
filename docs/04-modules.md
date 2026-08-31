# 4. Module-by-module

## 4.1 `token.ml`

Defines `type token`. Values carry their content: `IDENT of string`,
`STRING of string`, `INT of int`, `FLOAT of float`, `COMMA` (used by
multi-argument function calls such as `MOD(a, b)`), operator tokens, and the
keyword tokens (`LET`, `PRINT`, `IF`, `THEN`, `ELSE`, `GOTO`, `END`, `INPUT`,
`FOR`, `TO`, `STEP`, `NEXT`, `GOSUB`, `RETURN`, `DIM`, `AND`, `OR`, `NOT`).

## 4.2 `lexer.ml`

`Lexer.lex` scans a single string. Important details:

- Whitespace (`' '`) is skipped; other characters are not treated as
  separators (so `"10 LETX=1"` lexes `LETX` as an identifier).
- Numbers: `read_number` reads digits and an optional fraction; integers
  become `INT`, decimals become `FLOAT`. A bare `3.` (dot without digits)
  stays `INT 3` and the leftover `.` then fails as an invalid character.
- Identifiers are letters only (`is_letter`); keywords are matched by
  `keyword_or_ident`. **Keywords are uppercase**: identifiers are
  case-sensitive, so `run` is not `RUN`.
- Strings are read with `read_string`; an unterminated quote returns a
  `Lex_error`.
- Two-character operators `<=`, `>=`, `<>` are matched strictly before the
  single-character ones.

Exposed helpers `is_digit` / `is_letter` are used by the parser (line-number
detection, `REM` detection) and by the REPL.

## 4.3 `ast.ml`

The abstract syntax:

```ocaml
type value =
  | Number of float
  | String of string
  | Array of float array        (* mutable, numeric arrays *)

type expr =
  | Const of float | Str of string | Var of string
  | ArrayAccess of string * expr (* A(i) *)
  | Call of string * expr list   (* ABS(x) etc. *)
  | Binop of binop * expr * expr
  | Unop of unop * expr

type target = Var of string | ArrayElement of string * expr
type command = Let of target * expr | Print of expr | Goto of int
  | If of expr * command * command option
  | Input of string
  | For of string * expr * expr * expr option  (* var, start, limit, step *)
  | Next of string option | Gosub of int | Return
  | Dim of string * expr | Rem | End
type line = int * command
type program = line list
```

## 4.4 `parser.ml`

Expression parsing is a classic precedence-climbing chain, each level defined
in a mutually recursive group (`parse_primary` .. `parse_expr`, plus
`parse_call_args`):

```
parse_expr       = parse_or
parse_or         = parse_and (OR parse_and)*
parse_and        = parse_not (AND parse_not)*
parse_not        = NOT parse_not | parse_cmp
parse_cmp        = parse_add (< = > <= >= <> parse_add)*
parse_add        = parse_mul (+ - parse_mul)*
parse_mul        = parse_unary (* / parse_unary)*
parse_unary      = - parse_unary | parse_primary
parse_primary    = literal | variable | parenthesized expr
                 | builtin call | array access
```

`Call` vs `ArrayAccess` in `parse_primary` is decided by name: a fixed list
`builtins = ["ABS"; "INT"; "RND"; "LEN"; "MOD"]` becomes `Call (*, args)`;
any other `IDENT (...)` becomes `ArrayAccess (*, index)`. This is why `LEN`
as a variable/array name is shadowed — the builtin names win.

Commands are parsed by `parse_command_raw`, and `parse_command` additionally
requires the token stream to be fully consumed (`expect_end`): trailing
tokens are a syntax error, never silently dropped.

`REM` lines are detected at the source-text level *before* lexing
(`rem_of_line` + `is_rem_command`): a line whose body starts with `REM`
followed by a non-letter (or is exactly `REM`) becomes `Rem`. `REMX` is **not**
a comment (`is_rem_command` requires the char after `REM` to be non-letter).

`parse_line` parses a single line, attaches the line number to any error it
returns, and handles REM lines. `parse_program`:

- skips blank lines,
- sorts lines ascending,
- rejects duplicate line numbers ("duplicate line number N").

## 4.5 `env.ml`

The environment is an immutable association list `(string, value) list`
(`value` comes from `Ast`). `update` replaces an existing key (in place in
the list) or prepends, so later bindings shade earlier ones and `lookup`
finds the most recent. Because the `.mli` exposes `t` as **abstract**, the
only ways to touch the environment are `Env.empty`, `Env.lookup`, `Env.update`
— correct-by-construction.

## 4.6 `eval.ml`

`Eval.eval_expr : Env.t -> expr -> (value, Error.t) result` evaluates one
expression. Semantics:

- `+` (`Add`) overloaded: `Number + Number` sums; if **either** operand is a
  `String`, it concatenates (numbers are rendered with
  `string_of_number`); if either operand is an `Array`, it is a type error.
- `- * /` require numbers; division by zero produces a typed error.
- Comparisons work on `Number/Number` and `String/String`; mixed or arrays
  produce a type error.
- `AND` / `OR` / `NOT` treat 0.0 as falsy (results are `1.0`/`0.0`).
- `ArrayAccess`: the array must exist (in env), the index must be an integer
  within `[0, len-1]`.
- Builtins (`eval_call`):
  - `ABS x` -> `Float.abs x`
  - `INT x` -> `Float.floor x` (so `INT(-2.7)` = `-3`)
  - `RND x` -> `Random.float 1.0`; the argument is evaluated and ignored
  - `LEN s` -> string length (string required)
  - `MOD a b` -> truncation modulo (`int_of_float`), division by zero -> error
  - wrong arity -> "wrong number of arguments for 'NAME'"

Helpers exposed by the `.mli`: `string_of_number`, `string_of_value`,
`eval_expr`, `number_of`, `element_index`, `type_error`.

## 4.7 `machine.ml`

The execution machine. Internal state:

```ocaml
type loop = { loop_var; loop_limit : float; loop_step : float; loop_body : int }
type state = { env : Env.t; loops : loop list; rets : int list }
type step  = Next of state | Jump of state * int | Halt | Fail of Error.t
```

`eval_command` interprets one `command` and returns a `step`. `run_impl`
advances the program counter:

- `Next st` -> continue at the following line (falling off the end returns
  `Ok st`).
- `Jump (st, n)` -> continue at line `n`.
- `Halt` (from `END`) -> return the final state.
- `Fail err` -> abort with the error.

Public API (`.mli`): `run`, `env : state -> Env.t`, `string_of_number`,
`string_of_value`. `state` is abstract; `env` is the only accessor, which the
REPL uses to persist variables between runs.

Execution semantics worth knowing:

- **`FOR ... NEXT` is NEXT-driven.** At `For`, a `loop` frame is pushed with
  `loop_body` = the line following the FOR line, and the loop variable is set
  to `start`. The body therefore **always runs at least once** (classic BASIC
  behaviour; a loop like `FOR X = 1 TO 0` prints `1`). At `Next`, the
  variable is advanced by `step`; if the limit has not yet been passed
  (`<=`/`>=` depending on step sign), the machine `Jump`s back to
  `loop_body`; otherwise the frame is popped. `STEP 0` is rejected; `NEXT`
  with a mismatched variable name is an error.
- **`GOSUB` pushes the return address** (the line after the GOSUB line) onto
  `rets`; `RETURN` pops it. `RETURN` without `GOSUB` is an error.
- **Arrays are mutable in place.** `DIM A(n)` creates `Array (Array.make
  (n+1) 0.0)` (0-based). Writing `A(i)` mutates the shared `float array`
  held in the environment, so no env update is needed. `DIM` on an already
  defined name is an error, as are non-integer and negative sizes.
- **Type guards:** `PRINT`, `IF`, loop bounds and array indices reject `Array`
  values with clear messages; assigning a whole array (`LET B = A`) is
  rejected; assigning a string to an array element is rejected.
- `IF c THEN cmd [ELSE cmd]` executes `cmd` as a nested command
  (`eval_command` recursion); the condition must be `Number` (0 falsy) and the
  ELSE binds to the innermost IF.

## 4.8 `printer.ml`

Pretty-printing for humans: `Printer.string_of_expr`, `string_of_command`,
`string_of_line`, `string_of_program`. Used by the REPL's `LIST`. The format
is the single canonical representation — the test suite asserts against it
via `Printer.string_of_program`, so **do not change the format without
updating the parser test expectations**.

## 4.9 `repl.ml`

The interactive loop. See [8. The REPL](08-repl.md).

## 4.10 `main.ml`

CLI entry point. Argument handling:

- no arguments -> `Repl.run ~prompt:(Some "basicaml> ") ()`;
- `-h` / `--help` -> help text; `--version` -> version;
- exactly one path -> run the `.bas` file; file-read errors (`Sys_error`) are
  caught and printed as `basicaml: cannot read '<path>': <msg>` (exit 1);
- anything else -> usage, exit 1.

`Random.self_init ()` is called here so `RND` is well-seeded.

## 4.11 `error.ml`

```ocaml
type kind = Lex_error | Parse_error | Runtime_error
type t = { kind : kind; message : string; line : int option }
```

`Error.to_string` renders `Line N: <kind>: <message>` when a line is known.
Errors are created with `make ~kind`, and the line number is attached with
`with_line`. The type is exposed concretely in the `.mli` because
`machine.ml` and the tests pattern-match on `kind`.
