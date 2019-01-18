(* 
Haoning Hu
CSC 454
Assignment 8
12/15/2018
*)

open List;;
open Printf;;

type state = int;;
type 'a dfa = {
  cur_state : state;
  transition : (state * 'a * state) list;
  final_states : state list;
};;
type decision = Accept | Reject;;

type tok_type =
  Comment
  | Reserved
  | Symbol
  | Ident
  | Number
  | Quote;;
type token = {
  tok_type : tok_type;
  tok_value : string;
};;

let dfa_init () : 'a dfa =
  {
    cur_state = 0;
    transition = [
      (0, "space", 0); (1, "space", 1); (4, "space", 4); (17, "space", 17);
      (0, "newln", 0); (1, "newln", 2); (4, "newln", 5); (17, "newln", 17);
      (0, "#", 1); (1, "#", 1); (4, "#", 4); (17, "#", 17);
      (0, "/", 3); (1, "/", 1); (3, "/", 4); (4, "/", 4); (17, "/", 17);
      (0, "sym1", 6); (1, "sym1", 1); (4, "sym1", 4); (17, "sym1", 17);
      (0, "!", 7); (1, "!", 1); (4, "!", 4); (17, "!", 17);
      (0, "=", 9); (1, "=", 1); (4, "=", 4); (7, "=", 8); (9, "=", 10); (17, "=", 17);
      (0, "sym2", 9); (1, "sym2", 1); (4, "sym2", 4); (17, "sym2", 17);
      (0, "&", 11); (1, "&", 1); (4, "&", 4); (11, "&", 12); (17, "&", 17);
      (0, "|", 13); (1, "|", 1); (4, "|", 4); (13, "|", 14); (17, "|", 17);
      (0, "letter", 15); (1, "letter", 1); (4, "letter", 4); (15, "letter", 15); (16, "letter", 19); (17, "letter", 17); (18, "letter", 19);
      (0, "digit", 16); (1, "digit", 1); (4, "digit", 4); (15, "digit", 15); (16, "digit", 16); (17, "digit", 17); (18, "digit", 19);
      (0, "\"", 17); (1, "\"", 1); (4, "\"", 4); (15, "\"", 19); (16, "\"", 19); (17, "\"", 18); (18, "\"", 19);
      (1, "other", 1); (4, "other", 4); (17, "other", 17)
    ];
    final_states = [2; 3; 5; 6; 8; 9; 10; 11; 12; 13; 14; 15; 16; 18];
  };;

let dfa_update (d: 'a dfa) (x: 'a) : 'a dfa =
  {
    cur_state =
      (let (_, _, q) =
        find (fun (s, c, _) -> s = d.cur_state && c = x)
          d.transition in q);
    transition = d.transition;
    final_states = d.final_states;
  };;

let get_in c = match c with
    ' ' | '\t' -> "space"
    | '\n' | '\r' -> "newln"
    | '(' | ')' | '{' | '}' | '[' | ']' | ',' | ';' | '+' | '-' | '*' -> "sym1"
    | '>' | '<' -> "sym2"
    | 'A' .. 'Z' | 'a' .. 'z' | '_' -> "letter"
    | '0' .. '9' -> "digit"
    | '#' | '/' | '!' | '=' | '&' | '|' | '"' -> String.make 1 c
    | _ -> "other";;

let dfa_run (d: 'b dfa) (input: 'a list) : (state option * decision * 'a list * 'a list) =
  let rec helper (d2: 'b dfa) (remaining_input: 'a list) (acc: 'a list): (state option * 'a list * 'a list) =
    match remaining_input with
    | [] -> (Some d2.cur_state, [], acc)
    | x :: xs ->
      try helper (dfa_update d2 (get_in x)) xs (x :: acc)
      with Not_found -> if mem d2.cur_state d2.final_states then
        (Some d2.cur_state, remaining_input, acc) else (None, remaining_input, acc) in
  match helper d input [] with
    | (None, remaining_input, acc) -> (None, Reject, remaining_input, rev acc)
    | (Some last_state, remaining_input, acc) ->
        (Some last_state, (if mem last_state d.final_states then Accept else Reject), remaining_input, rev acc);;

let chr_list_to_str chl =
  String.concat "" (map (String.make 1) chl);;

let str_list_to_str strl =
  String.concat "" strl;;

let rec is_only_whitespace (input: char list) : bool = match input with
  [] -> true
  | x :: xs -> if x = ' ' || x = '\t' || x = '\n' || x = '\r' then is_only_whitespace xs
    else false;;

let rec strip str =
  let l = String.length str in 
  if l=0 then str
  else if str.[0]=' ' || str.[0]='\t' || str.[0]='\n' || str.[0]='\r' then
    strip (String.sub str 1 (l-1))
  else if str.[l-1]=' ' || str.[l-1]='\t' || str.[l-1]='\n' || str.[l-1]='\r' then
    strip (String.sub str 0 (l-1))
  else
    str;;

let get_token (tok_val: string) (state: state) : token = 
  {
    tok_value = strip tok_val;
    tok_type = match (strip tok_val) with
      "int" | "void" | "if" | "else" | "while" | "return" | "continue" | "break" | "scanf" | "printf" -> Reserved
      | _ -> match state with
        2 | 5 -> Comment
      | 3 | 6 | 8 | 9 | 10 | 11 | 12 | 13 | 14 -> Symbol
      | 15 -> Ident
      | 16 -> Number
      | 18 -> Quote
      | _ -> Comment;
  };;

let read_file fname : char list =
  let input = ref [] in
    let stream = open_in fname in
    try
      while true do
        input := input_char stream :: !input
      done;
      !input
    with End_of_file ->
      close_in stream;
      rev !input;;

let pp_tok_type oc = function
  | Comment -> fprintf oc "comment"
  | Reserved -> fprintf oc "reserved"
  | Symbol -> fprintf oc "symbol"
  | Ident -> fprintf oc "ident"
  | Number -> fprintf oc "number"
  | Quote -> fprintf oc "quote";;

let write_tokens fname (token_list: token list) = 
  let oc = open_out fname in
    map (fun token -> fprintf oc "< %a , %s >\n" pp_tok_type token.tok_type token.tok_value) token_list; 
  close_out oc;;

let tokenize (input: char list) : token list option =
  let rec helper input tokens : token list option =
    match dfa_run (dfa_init ()) input with
      (_, Reject, _, _) -> None
      | (Some last_state, Accept, remaining_input, acc) -> if (is_only_whitespace remaining_input) then
        Some ((get_token (chr_list_to_str acc) last_state) :: tokens)
      else
        match remaining_input with
          [] -> Some ((get_token (chr_list_to_str acc) last_state) :: tokens)
          | _ -> let r = helper remaining_input tokens in
            match r with
              Some xs -> Some ((get_token (chr_list_to_str acc) last_state) :: xs)
              | None -> None in
  helper input [];;

let scan fname =
  match tokenize (read_file fname) with
  Some x -> printf "Success\n"; write_tokens (fname ^ "_tokens.txt") x
  | None -> printf "Failure\n";;


(***************************************parselator***********************************************)
exception PE of string;;

let output = ref [];;
let local = ref [];;
let global = ref [];;
let funcs = ref [];;
let caller_remain = ref [];;
let local_count = ref 0;;   
let global_count = ref 0;;
let label_count = ref 0;;
let para_count = ref 0;;
let while_count = ref 1;;
let base = ref 0;;
let top = ref 0;;
let func_para = Hashtbl.create 100;;
let in_while_bool = ref false;;
let jump_bool = ref false;;
let call_bool = ref false;;

let rec index x lst =       
    match lst with
    | [] -> raise (Failure "Not Found")
    | h :: t -> if x = h then 0 else (1 + (index x t))     

let remove_first lst =     
  match lst with
  |e::l -> l

let rec print_list (tokenList: token list) =
  match tokenList with 
  [] -> ()
  | e :: l -> Printf.printf "%s" (e.tok_value ^ ",") ; print_list l
  | _ -> print_endline "done"

  let rec print_list_output (outputList: string list) =
  match outputList with
  [] -> ()
  | e :: l -> Printf.printf "%s" (e ^ "") ; print_list_output l
  | _ -> print_endline "done"


let peek (tokens : token list) =
  match tokens with
  |e :: l -> e
  |[] -> raise (PE "None")
  

let rec printComments (tokens : token list) =
  match tokens with
  |e::l when e.tok_type = Comment -> output := (e.tok_value ^ "\n")::!output; printComments l 
  | _ -> tokens


let match_token (tokens : token list) (token : token) =  
  if peek (printComments tokens) = token then List.tl (printComments tokens)
  else if tokens = [] then tokens
  else raise (PE "no match")

let match_token_string (tokens: token list) (tokenString : string) =
  if (peek (printComments tokens)).tok_value = tokenString then List.tl (printComments tokens)
  else if tokens = [] then tokens
  else raise (PE ("no match in token string with" ^ tokenString ^ " and not " ^ (peek tokens).tok_value))

let match_token_type (tokens: token list) (tokenType : tok_type) =
  if (peek (printComments tokens)).tok_type = tokenType then List.tl (printComments tokens)
  else if tokens = [] then tokens
  else raise (PE "no match in token type")

let type_name (tokens : token list) = 
  match tokens with 
  | e :: l when e.tok_value = "int" || e.tok_value = "void"  -> (*output := (e.tok_value ^ " ")::!output;*) match_token tokens e 
  | _ -> raise (PE (peek tokens).tok_value)

let rec compiler (tokens : token list) = 
  let x1 =  printComments tokens in
  output := "#define top mem[0]\n#define base mem[1]\n#define jumpReg mem[2]\n#define membase 3\nint const N=2000;\nint mem[N];\nint main() {\nbase=membase;\n" :: !output;
  base := 3;top:=3;
  output := "top=membase;\n" :: !output;
  output := "goto mainFunc;\n" :: !output;
  program x1

  and program (tokens : token list) =
    let rec helper tokensLength moreTokens=
          let x1 = printComments moreTokens in
          let x2 = type_name x1 in
          let x3 = identifier x2 in
          output := (peek x2).tok_value::!output;
          output := "Func:\n"::!output;
          funcs := (peek x2).tok_value::!funcs; 
          let newTokens = program_tail x3 in
          match List.length newTokens with
    |0 -> []
    |_ -> helper (List.length (newTokens)) newTokens in
    helper (List.length tokens) tokens

  and program_tail (tokens: token list) = 
  match tokens with
  | e:: l when e.tok_value = "," || e.tok_value = ";" -> funcs := remove_first !funcs; output := remove_first !output; global := (List.hd !output)::!global; output := remove_first !output; output := remove_first !output; output := remove_first !output; global_count := !global_count + 1; let x1 = global_id_list_tail tokens in
  output := ("top=membase+"^(string_of_int !global_count)^";\ngoto mainFunc;\n") :: !output; top:=3 + !global_count;
                                   let x2 = match_token_string x1 ";" in
                                   program x2

  | e :: l when e.tok_value = "(" -> func_list tokens
  | _ -> raise (PE (peek tokens).tok_value)


  and func_list (tokens : token list) =   
  match tokens with
  | e :: l when e.tok_value = "(" -> func_list (func_tail (func_decl tokens))
  | _ -> tokens

  and func_tail (tokens : token list) =
  match tokens with
  |e :: l when e.tok_value = ";" -> output := ";\n" :: !output; match_token_string tokens ";"  
  |e :: l when e.tok_value = "{" -> (*output := "{\n" :: !output;*) let x1 = match_token_string tokens "{" in
                                                                    let x2 = data_decls x1 in
                                                                    local := List.rev !local; global := List.rev !global;  (*data_decls is over, reverse variable list*)
                                                                    let x3 = statements x2 in
                                                                     (*   output := "}\n" :: !output; *)
                                                                    local_count := 0; local := [];
                                                                    Hashtbl.add func_para (List.hd !funcs) !para_count; para_count := 0;
                                                                    call_bool := false;
                                                                    match_token_string x3 "}"(*end of function, local_count reset to 0,clear local list*)

  | _ -> raise (PE (peek tokens).tok_value)

  and func_decl (tokens: token list) =
  match tokens with
  |e :: l when e.tok_value = "(" -> match_token_string (parameter_list (match_token_string tokens "(")) ")" 
  | _ -> raise (PE (peek tokens).tok_value)

  and parameter_list (tokens: token list) =
  match tokens with
  |e :: l when e.tok_value = "void" -> parameter_list_tail (match_token tokens e)
  |e :: l when e.tok_value = "int" -> parameter_list_tail (match_token (tokens) e)
  | _ -> tokens

  and parameter_list_tail (tokens : token list) =
  match tokens with
  |e :: l when e.tok_type = Ident ->para_count := !para_count + 1; local_count := !local_count + 1; local := e.tok_value :: !local; non_empty_list_tail (identifier tokens)  
  | _ -> tokens

  and non_empty_list (tokens : token list) =
  let x1 = type_name tokens in 
  para_count := !para_count + 1; local_count := !local_count + 1; local := (peek x1).tok_value :: !local;
  let x2 = identifier x1 in
  non_empty_list_tail x2 

  and non_empty_list_tail (tokens : token list) =
  match tokens with
  |e :: l when e.tok_value = "," -> let x1 = match_token_string tokens "," in 
                                    non_empty_list  x1  
  | _ -> tokens

  and data_decls (tokens: token list) =
  match tokens with
  |e :: l when e.tok_value = "int" -> let x1 = type_name tokens in 
                                      let x2 = local_id_list x1 in   
                                      output := ("base=top;\ntop=base+"^(string_of_int !local_count)^";\n") :: !output;
                                      base := !top;top := !base + !local_count;
                                      if !para_count=1 then output := "mem[base+0]=mem[base-3-1];\n"::!output
                                      else if !para_count=2 then output := "mem[base+0]=mem[base-3-2];\nmem[base+1]=mem[base-3-1]"::!output; base:=!base + 1; 
                                      data_decls (match_token_string x2 ";")
  | _ -> tokens

  and local_id_list (tokens : token list) = 
  local_count := !local_count + 1;
  local := (peek tokens).tok_value :: !local;
  local_id_list_tail (identifier tokens);
  
  and local_id_list_tail (tokens : token list) =
  match tokens with
  |e :: l when e.tok_value = "," -> let x1 = match_token_string tokens "," in
                                    local := (peek x1).tok_value :: !local;  
                                    let x2 = identifier x1 in 
                                    local_count := !local_count + 1;      (*here is exactly where local_count add 1 happens*)
                                    local_id_list_tail x2;
  | _ -> tokens

  and global_id_list (tokens : token list) =                         (*pretty much the same as local_id_list*)
  global_count := !global_count + 1;
  global := (peek tokens).tok_value :: !global;
  global_id_list_tail (identifier tokens);

  and global_id_list_tail (tokens : token list) = 
  match tokens with
  |e :: l when e.tok_value = "," -> let x1 = match_token_string tokens "," in
                                    global := (peek x1).tok_value :: !global;
                                    let x2 = identifier x1 in
                                    global_count := !global_count + 1;
                                    global_id_list_tail x2;
  | _ -> tokens

  and identifier (tokens : token list) =
  match tokens with
  | e :: l when e.tok_type = Ident -> identifier_tail (match_token_type tokens Ident)
  | _ -> raise (PE (peek tokens).tok_value)

  and identifier_tail (tokens : token list) = 
  match tokens with                        (*no need to change this, because there will be no array in a7*)
  | e :: l when e.tok_value = "[" -> match_token_string (expression (match_token_string tokens "[")) "]"
  | _ -> tokens

  and block_statements (tokens : token list) = 
  output := "{" :: !output;
  let x1 = statements (match_token_string tokens "{") in
  match_token_string x1 "}"

  and statements (tokens : token list) =   
  match tokens with
  |e :: l when e.tok_type = Ident || e.tok_value = "printf" || e.tok_value = "scanf" || e.tok_value = "if" || e.tok_value = "while" || e.tok_value = "return" || e.tok_value = "break" || e.tok_value = "continue" -> statements (statement tokens)
  | _ -> tokens

  and statement (tokens : token list) =
  match tokens with                       
  |e :: l when e.tok_type = Ident -> if (List.mem e.tok_value !local) then output := ("mem[base+"^(string_of_int (index e.tok_value !local))^"]") :: !output 
                                     else if (List.mem e.tok_value !global) then output := ("mem["^(string_of_int (index e.tok_value !global))^"]") :: !output
                                   (*  else if (List.mem e.tok_value !funcs) then output := "mem[top+0]="::!output; call_bool:=true; caller_remain := (List.hd !output)::!caller_remain; output := remove_first !output; caller_remain := (List.hd !output)::!caller_remain; end *)
                                     else output := e.tok_value :: !output;  
                                     general_assignment (identifier tokens);
  |e :: l when e.tok_value = "printf" -> printf_func_call tokens
  |e :: l when e.tok_value = "scanf" -> scanf_func_call tokens
  |e :: l when e.tok_value = "if" -> if_statement tokens
  |e :: l when e.tok_value = "while" -> while_statement tokens
  |e :: l when e.tok_value = "return" -> return_statement tokens
  |e :: l when e.tok_value = "break" -> break_statement tokens
  |e :: l when e.tok_value = "continue" -> continue_statement tokens
  | _ -> raise (PE (peek tokens).tok_value)

  and general_assignment (tokens : token list) =
  match tokens with
  |e :: l when e.tok_value = "(" -> output := "(" :: !output;      
                                    let x1 = (expr_list (match_token_string tokens "(")) in
                                    output := ")" :: !output;
                                    let x2 = match_token_string x1 ")" in
                                    output := ";\n" :: !output;
                                    match_token_string x2 ";"    
  |e :: l when e.tok_value = "=" -> output := "=" :: !output;
                                    let x3 = expression (match_token_string tokens "=") in
                                    output := ";\n" :: !output;
                                    match_token_string x3 ";"
  | _ -> raise (PE (peek tokens).tok_value)
  
  
  and printf_func_call (tokens: token list) =
  match tokens with
  |e :: l when e.tok_value = "printf" -> output := "printf" :: !output;    
                             let x1 = match_token_string tokens "printf" in
                             output := "(" :: !output;
                             let x2 = match_token_string x1 "(" in
                             output := (peek x2).tok_value :: !output;
                             let x3 = match_token_type x2 Quote in
                             let x4 = printf_func_call_tail x3 in
                             output := ")" :: !output;
                             let x5 =  match_token_string x4 ")" in
                             output := ";\n" :: !output;
                             match_token_string x5 ";"
  | _ -> raise (PE (peek tokens).tok_value)

  and printf_func_call_tail (tokens: token list) =
  match tokens with
  |e :: l when e.tok_value = "," -> output := "," :: !output; expression (match_token_string tokens ",")  
  | _ -> tokens

  and scanf_func_call (tokens: token list) =
  output := "scanf" :: !output;                      
  let x1 = match_token_string tokens "scanf" in
  output := "(" :: !output;
  let x2 = match_token_string x1 "(" in
  output := (peek x2).tok_value :: !output;
  let x3 = match_token_type x2 Quote in
  output := "," :: !output;
  let x4 = match_token_string x3 "," in
  output := "&" :: !output;
  let x5 = match_token_string x4 "&" in 
  let x6 = expression x5 in
  output := ")" :: !output;
  let x7 = match_token_string x6 ")" in
  output := ";\n" :: !output;
  match_token_string x7 ";"

  and expr_list (tokens : token list) =     
  match tokens with
  | e :: l when e.tok_type = Ident -> non_empty_expr_list tokens
  | _ -> tokens

  and non_empty_expr_list (tokens : token list) =    
  non_empty_expr_list_tail (expression tokens)

  and non_empty_expr_list_tail (tokens : token list) =    
  match tokens with
  |e :: l when e.tok_value = "," -> output := "," :: !output; non_empty_expr_list_tail (expression (match_token tokens e))
  | _ -> tokens


  and if_statement (tokens : token list) =
  output := "if" :: !output;
  let x1 = match_token_string tokens "if" in
  output := "(" :: !output;
  let x2 = match_token_string x1 "(" in
  let x3 = condition_expression x2 in
  output := ")" :: !output;
  let x4 = match_token_string x3 ")" in
  output := ("{goto label_" ^ (string_of_int !label_count) ^ "; }") :: !output; label_count := !label_count + 1; output := ("\ngoto label_" ^ (string_of_int !label_count) ^ ";\n") :: !output;
  output := ("\nlabel_" ^ (string_of_int (!label_count - 1)) ^ ":\n") :: !output;
  let x5 = block_statements x4 in
  if_statement_tail x5
  

  and if_statement_tail (tokens : token list) =     
  match tokens with 
  |e :: l when e.tok_value = "else" -> output := ("} label_" ^ string_of_int !label_count ^ ":\n") :: !output; label_count := !label_count + 1; block_statements (match_token_string tokens "else")
  | _ -> output := ("label_" ^ string_of_int !label_count ^ ":0;\n") :: !output; label_count := !label_count + 1; tokens

  and condition_expression (tokens : token list) =     
  condition_expression_tail (condition tokens)

  and condition_expression_tail (tokens : token list) =   
  match tokens with 
  |e :: l when e.tok_value = "&&" || e.tok_value = "||" -> condition (condition_op tokens)
  | _ -> tokens

  and condition_op (tokens : token list) =   
  match tokens with                                           
  |e:: l when e.tok_value = "&&" || e.tok_value = "||" -> output := e.tok_value :: !output; match_token tokens e
  | _ -> raise (PE (peek tokens).tok_value)

  and condition (tokens: token list) =     
  expression (comparison_op (expression tokens))

  and comparison_op (tokens: token list) =        
  match tokens with
  |e :: l when e.tok_value = "==" || e.tok_value = "!=" || e.tok_value = "<" || e.tok_value = ">" || e.tok_value = "<=" || e.tok_value = ">=" -> output := e.tok_value :: !output; comparison_op (match_token tokens e)
  | _ -> tokens

  and while_statement (tokens : token list) =  
  let x1 = match_token_string tokens "while" in
  let x2 = match_token_string x1 "(" in
  if(!in_while_bool = true) then while_count := !while_count + 1;
  output := ("cond_" ^(string_of_int !while_count ^ ":\n") ^ "if(") :: !output;

  let x3 = condition_expression x2 in 
  output := ("){goto while_" ^ (string_of_int !while_count) ^ ";}\nelse{goto endwhile_" ^ (string_of_int !while_count)^ ";}\n") :: !output;
  let x4 = match_token_string x3 ")" in
  output := ("while_" ^ (string_of_int !while_count) ^ ":\n") :: !output; 
  in_while_bool := true;
  let x5 = block_statements x4 in
  while_statement_tail x5

  and while_statement_tail (tokens: token list) =
          output := (("goto cond_" ^ ((string_of_int (!while_count)) ^ ";}\n")) ^ ("endwhile_" ^ (string_of_int (!while_count)) ^ ":0;\n")) :: !output;
  if(!in_while_bool = true) then while_count := !while_count - 1 else while_count := !while_count + 1;
  in_while_bool := false;
  tokens

  and return_statement (tokens : token list) =
  output := "mem[base-1]=" :: !output;
  let x1 = match_token_string tokens "return" in
  let x2 = return_statement_tail x1 in
  output := ";\n" :: !output;
  output := "top=base;\n" :: !output;top:=!base;
  if List.hd !funcs = "main" then output:="return mem[base-1];\n"::!output
  else begin output:="jumpReg=mem[base-2]; goto jumpTable;\n"::!output; jump_bool:=true end;
  if !jump_bool then output:="jumpTable:\n switch(jumpReg){\n  case 1:\n    goto jumplabel_1; break;\n}\n"::!output;  
  match_token_string x2 ";"

  and return_statement_tail (tokens : token list) =
  match tokens with
  |e :: l when e.tok_type = Ident || e.tok_type = Number || e.tok_value = "-" || e.tok_value = "(" -> expression tokens
  | _ -> tokens

  and break_statement (tokens : token list) =   
  output := ("goto endwhile_" ^ (string_of_int !while_count)) :: !output;
  let x1 = match_token_string tokens "break" in
  output := ";\n" :: !output;
  match_token_string x1 ";"

  and continue_statement (tokens : token list) =     
  output := "continue" :: !output;
  let x1 = match_token_string tokens "continue" in
  output := ";\n" :: !output;
  match_token_string x1 ";"

  and expression (tokens : token list) =      
  expression_tail (term tokens)

  and expression_tail (tokens : token list) =    
  match tokens with
  | e :: l when e.tok_value  = "+" || e.tok_value = "-" -> expression_tail (term (addop tokens))
  | _ -> tokens

  and addop (tokens : token list) =          
  match tokens with
  |e :: l when e.tok_value = "+" || e.tok_value = "-" -> output := e.tok_value :: !output; match_token tokens e
  | _ -> raise (PE (peek tokens).tok_value)

  and term (tokens :token list) =       
  term_tail (factor tokens)

  and term_tail (tokens : token list) =   
  match tokens with
  |e :: l when e.tok_value = "*" || e .tok_value = "/" -> term_tail (factor (mulop tokens))
  | _ -> tokens

  and mulop (tokens: token list) =   
  match tokens with
  |e :: l when e.tok_value = "*" || e.tok_value = "/" -> output := e.tok_value :: !output; match_token tokens e
  | _ -> raise (PE (peek tokens).tok_value)

  and factor (tokens : token list) =
  match tokens with                
  | e :: l when e.tok_type = Ident -> if (List.mem e.tok_value !local) then output := ("mem[base+"^(string_of_int (index e.tok_value !local))^"]") :: !output   
                                      else if (List.mem e.tok_value !global) then output := ("mem["^(string_of_int (index e.tok_value !global))^"]") :: !output
                                      else if e.tok_value = List.hd !funcs then raise(PE "stack overflow")
                                      else if (List.mem e.tok_value !funcs) then begin call_bool:=true; caller_remain := (List.hd !output)::!caller_remain; output := remove_first !output; caller_remain := (List.hd !output)::!caller_remain; output:= remove_first !output; output := "mem[top+0]="::!output; end
                                      else output :=e.tok_value :: !output;
                                      factor_tail (identifier tokens)
  | e :: l when e.tok_type = Number -> output := e.tok_value :: !output; match_token tokens e
  | e :: l when e.tok_value = "-" -> output := "-" :: !output; factor (match_token tokens e)
  | e :: l when e.tok_value = "(" -> output := "(" :: !output; factor (expression (match_token tokens e))
  | e :: l when e.tok_value = ")" -> output := ")" :: !output; match_token tokens e
  | _ -> raise (PE (peek tokens).tok_value)

  and factor_tail (tokens : token list) =
  match tokens with
  |e :: l when e.tok_value = "[" -> output := "[" :: !output;        
                                    let x1 = expression (match_token_string tokens "[") in
                                    output := "]" :: !output;
                                    match_token_string x1 "]" 
  |e :: l when e.tok_value = "(" -> if not !call_bool then output := "(" :: !output; 
                                    let x2 = expr_list (match_token_string tokens "(") in
                                    if not !call_bool then output := ")" :: !output
                                    else begin output := (";\ntop=top+1;\nmem[top]=base;\nmem[top+1]=1;\ntop=top+3;\ngoto "^(List.hd (rev !funcs))^"Func;\njumplabel_1:\nbase=mem[top-3];\nmem[base+2]=mem[top-1];\ntop=base+"^(string_of_int !local_count)^";\n")::!output;top:=!top + 1;top:=!top + 3;top:=!base + !local_count;
                                    output:=(List.hd !caller_remain)::!output; caller_remain:= remove_first !caller_remain;
                                    output:=((List.hd !caller_remain)^"mem[base+2]")::!output;
                                    end;
                                    match_token_string x2 ")";
  | _ -> tokens

let test =
  if Array.length Sys.argv > 1 then
    match tokenize (read_file Sys.argv.(1)) with
    Some x -> printf "" ; compiler x
    | None -> printf "None"; []
  else [];;
  output := "exit(0);\n}\n" :: !output;
  if (!top>1500 || !base>1500) then raise(PE "Stack overflow");
  print_list_output (rev !output); 
  
