{
  open Parser
}

let white = [' ' '\t']+
let num = ['0'-'9']|['1'-'9']['0'-'9']*

rule read_token =
  parse
  | white { read_token lexbuf }
  | ['U''u''-'] { U }
  | ['S''s''+'] { S }
  | [','';'':''.'] { SEP }
  | num { NUM (Lexing.lexeme lexbuf |> int_of_string) }
  | eof { EOF }
