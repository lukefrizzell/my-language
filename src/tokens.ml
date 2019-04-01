type operator = 
  | Plus
  | Minus
  | Multiply
  | Divide

type token = 
  | Operator of operator
  | Number of int64
  | Scope of token list

let op_to_str (op: operator) : string =
  match op with
  | Plus -> "+"
  | Minus -> "-"
  | Multiply -> "*"
  | Divide -> "/"

let is_num s =
  Str.string_match (Str.regexp "[0-9]+") s 0

let rec str_to_token (str: string): token option =
  match str with
  | "+" -> Some (Operator Plus)
  | "*" -> Some (Operator Multiply)
  | "/" -> Some (Operator Divide)
  | "-" -> Some (Operator Minus)
  | str when is_num str -> Some (Number (Int64.of_string str))
  | _ -> None

let rec token_to_string(tok : token) : string =
  match tok with
  | Operator op -> op_to_str op
  | Number i -> Int64.to_string i
  | Scope is -> "( " ^ String.concat " " (List.map token_to_string is) ^ " )"

exception SyntaxError of string

let rec process_buffer (buffer : char list) : token = 
  let st = String.concat "" (List.map (String.make 1) buffer) in
  match str_to_token st with
    | Some v -> v
    | None -> raise (SyntaxError st)

let rec lex_inner (chars : char list) (buffer : char list) : token list * char list = 
  match chars with
  | [] -> if buffer = [] 
    then [], [] 
    else (process_buffer buffer :: []), []
  | ')' :: chars -> if buffer = [] 
    then [], chars 
    else (process_buffer buffer :: []), chars
  | ' ' :: chars -> let (tokens, chars) = lex_inner chars [] in if buffer  = [] 
    then tokens, chars 
    else process_buffer buffer :: tokens, chars
  | '(' :: chars -> let (tokens, chars) = lex_inner chars [] in 
    let (tokens', chars') = lex_inner chars [] in  if buffer = [] 
    then Scope tokens :: tokens', chars' 
    else process_buffer buffer :: Scope tokens :: tokens', chars'
  | c :: chars -> lex_inner chars (c :: buffer)

let lex (st: string) : token list =   
  let chars = List.init (String.length st) (String.get st) in
  fst (lex_inner chars [])

let _ = 
  let test_string = "5 * ( 4 + ( 1 + 2 )) + 3" in
  let test_tokens = lex test_string in
  print_string (String.concat " " (List.map token_to_string test_tokens));
  print_string "\n"