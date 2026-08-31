val is_digit : char -> bool

val is_letter : char -> bool

val lex : string -> (Token.token list, Error.t) result