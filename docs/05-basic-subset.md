# 5. The BASIC subset

Supported statements:

```
LET <var|var(i)> = <expr>
PRINT <expr>
INPUT <var>
GOTO <line>
IF <expr> THEN <command> [ELSE <command>]
FOR <var> = <expr> TO <expr> [STEP <expr>]
NEXT [<var>]
GOSUB <line>
RETURN
DIM <var>(<size-expr>)
REM ...
END
```

Supported expressions: literals (integers, decimals, strings), variables,
arrays `NAME(index)`, builtin calls (`ABS`, `INT`, `RND`, `LEN`, `MOD`),
binary operators `+ - * / = <> < <= > >= AND OR`, unary `-` and `NOT`, and
parentheses. `+` concatenates strings.

Numbers are stored as OCaml `float`; `PRINT` renders integers without a
decimal part (`10` not `10.0`) via `string_of_number`.
