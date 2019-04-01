type operator = 
  | Plus
  | Minus
  | Multiply
  | Divide

type token = 
  | Operator of operator
  | Number of int64

let type_order (tok : token) : int = 
  match tok with
  | Number _ -> 0
  | Operator _ -> 1 

let compare_token (tok1 : token) (tok2 : token) =
  match (tok1, tok2) with
  | (Operator op1, Operator op2) -> compare op1 op2
  | (Number i1, Number i2) -> compare i1 i2
  | (t1, t2) -> compare (type_order t1) (type_order t2)

module TokenCompare = struct
  type t = token
  let compare = compare_token
end

module TokenMap = Map.Make(TokenCompare)

let tokens = ref TokenMap.empty

let add_token (tok : token) (st : string) : unit =
  tokens := TokenMap.add tok st !tokens

let init_token_map = 
  let _ = add_token (Operator Plus) "+";
  add_token (Operator Minus) "-";
  add_token (Operator Multiply) "*";
  add_token (Operator Divide) "/"; 
  in ()

let token_to_string(tok : token) : string =
  match tok with
  | Operator _ -> TokenMap.find tok !tokens
  | Number i -> Int64.to_string i

let rec string_to_token (st_match : string) (token_list : (token * string) list) : token option =
  if Str.string_match (Str.regexp "[0-9]+") st_match 0 then Some (Number (Int64.of_string st_match)) else
  match token_list with
  | [] -> None
  | (tok, st) :: rest -> if st_match = st then Some tok 
      else string_to_token st_match rest

exception SyntaxError of string

let rec process_buffer (buffer : char list) : token = 
  let st = String.concat "" (List.map (String.make 1) buffer) in
    let token_list = TokenMap.bindings !tokens in
    (match string_to_token st token_list with
    | Some v -> v
    | None -> raise (SyntaxError st))

and lex_inner (chars : char list) (buffer : char list) : token list = 
  match chars with
  | [] -> process_buffer buffer :: []
  | ' ' :: chars -> (process_buffer buffer) :: (lex_inner chars [])
  | c :: chars -> lex_inner chars (c :: buffer)

let lex (st: string) : token list =   
  let chars = List.init (String.length st) (String.get st) in
  lex_inner chars []

let _ = 
  let test_string = "1 + 2" in
  let test_tokens = lex test_string in
  List.iter print_string (List.map token_to_string test_tokens)