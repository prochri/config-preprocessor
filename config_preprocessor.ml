
open Printf

let parenthesies = ref "{}"
let comment_string = ref "#"

let parameter_map = [
  ("c", comment_string);
  ("p", parenthesies);
]

let input_filename = ref ""
let output_filename = ref ""
let disclaimer = ref ""
let keyref = ref ""
let varlistref = ref []
let use_env = ref true
let usage = "\
usage: " ^ Sys.argv.(0) ^ " [--help] [-d disclaimer] [-p parenthesies] [-c comment_string] [-s var val]* infile [outfile]

Write configs with additional syntax. Examples:
  set $a $b\t\t\t-- recursive assingments possible
  include $p\t\t\t-- include other files as if they are in this file
  {ifdef a}some text{else}other text{endif}
  \t\t\t\t-- check if variable is assigned
  {ifmatch $a=value}...{endif}\t-- simple string comparisons
  $mod+{a,b} {do_a,do_b}\t-- List expansion
  #!ifdef\\nline\\n#!endif\t-- Multi Line if
  ${shellcommand}\t\t-- execute shell command and put output here
  $${a + $c}\t\t\t-- calculate with basic integer operations

The parameters -p and -c can also be given in the first line of the input:
  // build_conf: p \"<>\" c \"//\"

Parameters:"

let speclist = [
  ("-s", Arg.Tuple [
      Arg.Set_string keyref;
      Arg.String (fun s -> varlistref := (!keyref,s)::(!varlistref); keyref := "");
    ], "var val : set variable to value, like \"set $var val\" inside of the file");
  ("-p", Arg.Set_string parenthesies, ": set the parenthesis of the logic blocks -- default: \"{}\"");
  ("-d", Arg.Set_string disclaimer,": disclaimer written on top (plain text)");
  ("-c", Arg.Set_string comment_string,": prefix to indicate comment");
  ("--ignore-env", Arg.Clear use_env, ": do not use environment variables");
]

module List = struct include List
  let heads ll = map hd ll
  let tails ll = map tl ll
  let empty = function [] -> true | _ -> false
end

module String = struct include String
  let firstn n str = if length str > n then sub str 0 n else str
  let from n str = if n < length str then sub str n ((length str)-n) else ""
  let compare_to n s1 s2 = String.compare (firstn n s1) (firstn n s2)
  let non_empty = function "" -> false | _ -> true
end

module Util = struct
  let absolute_path path = let l = String.split_on_char '/' path in
    let resolve_path_component str =
      if str = "~" then Sys.getenv "HOME"
      else if String.length str = 0 then ""
      else if String.get str 0 = '$' then String.sub str 1 ((String.length str) - 1) |> Sys.getenv
      else str
    in
    List.map resolve_path_component l |> String.concat "/"

  let external_command_output cmd =
    let in_ch = Unix.open_process_in cmd in
    let rec aux rl = try let s = input_line in_ch in
        aux (s::rl)
      with End_of_file -> Unix.close_process_in in_ch |> ignore; rl |> List.rev |> String.concat "\n"
    in aux []
end


module Math = struct

  type operand = Add | Sub | Mul | Div
  type expression = Number of int
                  | Calculation of  operand * expression * expression

  let level = function
    | Add | Sub -> 0
    | Mul -> 1
    | Div -> 2
  let level_list = Div::Mul::Add::Sub::[]

  let resolve_number : string -> (string * string) list -> expression = fun string varlist ->
    if Str.string_match (Str.regexp "^\\${\\(.*\\)}$") string 0 then
      let command = (Str.matched_group 1 string) in
      let x = Util.external_command_output command |> int_of_string
      in Number x
    else
    if Str.string_match (Str.regexp "^\\$\\(.*\\)$") string 0 then
      let variable = (Str.matched_group 1 string) in
      match List.assoc_opt variable varlist
      with Some x -> Number (int_of_string x)
         | None -> failwith (sprintf "variable %s not found in variablelist" variable)
    else  Number(int_of_string string)

  let tokenize string = let re = Str.regexp "+\\|-\\|*\\|/\\|(\\|)" in
    Str.full_split re string |> List.filter_map
      (function Str.Delim x -> Some x
              | Str.Text x -> let x = (String.trim x) in
                if x = "" then None else Some x)

  let parse string varlist =
    let symbols = tokenize string in
    let rec build_expression_tree symbols =
      let rec aux l (exl, opl) =
        match l with
        | [] -> (exl, opl, [])
        | "+"::rl -> aux rl (exl, Add::opl)
        | "-"::rl -> aux rl (exl, Sub::opl)
        | "*"::rl -> aux rl (exl, Mul::opl)
        | "/"::rl -> aux rl (exl, Div::opl)
        | ")"::rl -> (exl, opl, rl)
        | "("::rl -> let expr, rl = build_expression_tree rl in aux rl (expr::exl, opl)
        | x::rl -> aux rl ((resolve_number x varlist)::exl, opl)
      in
      let expressions, operands, rl = aux symbols ([],[]) in
      let expressions, operands = List.rev expressions, List.rev operands in
      let apply_operand (expl, opl) operand =
        let f operand =
          (fun (last_exp, expl, opl) exp op ->
             if op = operand then
               (Calculation (op, last_exp, exp), expl, opl)
             else
               (exp, last_exp::expl, op::opl))
        in
        let last_exp, expl, opl = List.fold_left2 (f operand)
            (List.hd expl, [],[]) (List.tl expl) opl
        in List.rev (last_exp::expl), List.rev opl
      in
      let expl,_ = List.fold_left apply_operand (expressions, operands) level_list in
      List.hd expl, rl
    in let expr, _ = build_expression_tree symbols in expr

  let rec eval expression = match expression with
    | Number x -> x
    | Calculation (op, a, b) -> let a, b = eval a, eval b in
      match op with
      | Add -> a + b
      | Sub -> a - b
      | Mul -> a * b
      | Div -> a / b

  let eval_string string varlist = parse string varlist |> eval
end


module Preprocessor
  : sig
    val resolve_file : string -> out_channel -> (string * string) list ->
      (string * string) list
    val resolve_string : string -> out_channel -> (string * string) list ->
      (string * string) list
  end
= struct
  (* defined types *)
  type cond_binop = string -> string -> bool
  type logic_block = Listing of string list | Ifdef of string | Ifmatch of string * cond_binop * string
                   | Else | Endif | Content of string

  (* regular expressions for matching patterns *)
  (* "\\" is used for a simple backslash (\) before it is processed by the string processor *)
  (* so for an escaped backslash in the pattern you have to write \\\\ *)

  (* patterns dependant on the parenthesis used *)
  let lp () = String.get !parenthesies 0
  let rp () = String.get !parenthesies 1
  let re_exist_logic_block () = let l,r = lp (), rp () in
    sprintf  "^.*%c[^%c]*%c.*$" l r r |> Str.regexp
  let re_logic_block () = let l,r = lp (), rp () in
    sprintf "%c[^%c]*%c" l r r |> Str.regexp
  let re_external () = let l,r = lp (), rp () in
    sprintf "\\$\\$%c\\|\\$%c\\|%c\\|%c" l l l r |> Str.regexp
  (* other patterns *)
  let re_line_command () = sprintf  "^[ \t]*%s!\\(.*\\)$" !comment_string |> Str.regexp
  let re_ignore () = sprintf "^[ \t]*%s\\([^!].*$\\|$\\)\\|^[ \t]*$" !comment_string |> Str.regexp
  let re_line_expand = Str.regexp "^\\(.*\\)\\\\$"
  let re_include = Str.regexp "^[ \t]*include[ \t]+\\([^ \t]+\\)[ \t]*$"
  let re_set () = sprintf "^[ \t]*\\(%s!\\)?set[ \t]*\\$\\([a-zA-Z_]+\\)[ \t]+\\([^ \t].*\\)$" !comment_string |> Str.regexp
  let re_var = Str.regexp "\\$\\([a-zA-Z_]+\\)"
  let re_paramater_heading = Str.regexp ".*build_conf:[ \t]\\(.*\\)"

  let integer_binop op = fun a b -> let a, b = int_of_string a, int_of_string b in op a b
  let cond_binop_map = [
    ("=", (=));
    ("!=", (<>));
    (">", integer_binop (>));
    (">=", integer_binop (>=));
    ("<", integer_binop (<));
    ("<=", integer_binop (<=));
  ]
  let re_binop = let binop_group = "\\(=\\|!=\\|>\\|>=\\|<\\|<=\\)" in let val_group = "\\([-a-zA-Z0-9_$]+\\)" in
    Str.regexp (val_group ^ binop_group ^ val_group)

  let logic_map = [
    ("ifdef", fun s -> Ifdef s);
    ("ifmatch", fun s -> if Str.string_match re_binop s 0
       then try Ifmatch (Str.matched_group 1 s,
                         List.assoc (Str.matched_group 2 s) cond_binop_map,
                         Str.matched_group 3 s)
         with Not_found -> failwith (s ^ "is no conditional binop")
       else failwith "ifmatch requires binop"
    );
    ("else", fun s -> Else);
    ("endif", fun s -> Endif);
    ("", fun s -> Listing (String.split_on_char ',' s));
  ]

  let resolve_variable varlist value =
    let vlist = Str.full_split re_var value in
    let lookup key = (match List.assoc_opt key varlist with None -> "" | Some x -> x) in
    List.fold_left (fun acc s -> acc ^ (match s with Str.Delim key -> lookup (String.from 1 key) | Str.Text t -> t))
      "" vlist

  let resolve_listings l =
    let stringify contentlist = contentlist |> List.map (function Content x -> x | _ ->
        fprintf stderr "Error in resolve listings: not only content\n"; failwith "fck")
                                |> String.concat "" in
    let rec aux l = try
        let contentlist = List.map (function Listing x -> Content (List.hd x) | x -> x) l in
        let rlistings = List.map (function Listing x -> Listing (List.tl x) | x -> x) l in
        let block = stringify contentlist in
        block::aux rlistings
      with Failure x -> if x = "hd" || x = "tl" then [] else failwith x
    in if List.exists (function Listing _ -> true | _ -> false) l
    then aux l else [stringify l]

  let resolve_control_flow varlist lblocks =
    let rec resolve_lblock lb l = match lb with
      | Ifdef _ | Ifmatch _ -> resolve_if lb l
      | Content s -> [Content s], l
      | Listing x -> [Listing x], l
      | _ -> failwith "no valid block"
    and resolve_if lb l = let truthvalue = match lb with
        | Ifdef s -> List.mem_assoc s varlist
        | Ifmatch (s1,c,s2) -> c (resolve_variable varlist s1) (resolve_variable varlist s2)
        | _ -> failwith "not if block given to resolve_if" in
      let rec aux l truthv =
        match l with [] -> failwith "no endif"
                   | Endif::l -> [], l
                   | Else::l -> if truthv = truthvalue then aux l (not truthv)
                     else failwith "unresolved else"
                   | x::l -> let b,l = resolve_lblock x l in let myblock, l = aux l truthv in
                     (if truthv then b else []) @ myblock, l
      in aux l truthvalue
    in
    let rec aux = function [] -> []
                         | x::l -> let b,l = resolve_lblock x l in b @ aux l
    in aux lblocks

  let find_logic_block content =
    let rec aux = function [] -> failwith "Logic Block not valid"
                         | (keyword,f)::rl -> if String.compare_to (String.length keyword) keyword content == 0
                           then f (String.from (String.length keyword) content |> String.trim)
                           else aux rl
    in aux logic_map


  (* produces a list of lines respecting escaped new line characters and ignoring real comments *)
  let filter_comments_escaped_lines : string list -> string list = fun linelist ->
    let add_non_empty str l = (if str = "" then l else str::l) in
    let rec aux blocks restlines current_block =
      match restlines with [] -> add_non_empty current_block blocks |> List.rev
                         | line::restlines ->
                           if Str.string_match (re_ignore ()) line 0 then
                             aux (add_non_empty current_block blocks) restlines ""
                           else
                           if Str.string_match re_line_expand line 0 then
                             Str.global_replace re_line_expand "\\1" line |>
                             (^) current_block |> aux blocks restlines
                           else aux ((current_block^line)::blocks) restlines ""
    in aux [] linelist ""

  let set_parameters lines = match lines with
    | [] -> []
    | first::restlines -> if Str.string_match re_paramater_heading first 0 |> not then lines
      else ((Str.matched_group 1 first |> String.split_on_char ' ' |> List.filter String.non_empty |>
             List.iter (fun x -> let map = String.split_on_char ':' x in match map with
               | par::value::[] -> (List.assoc par parameter_map) := (String.split_on_char '"' value |> List.filter String.non_empty|> List.hd)
               | _ -> failwith "parameter heading not valid"
               )) ;
            restlines)


  let read_lines_file ifile =
    let ic = open_in ifile in
    let rec aux linelist = try (input_line ic)::linelist |> aux
      with End_of_file -> close_in ic; List.rev linelist
    in aux []

  (* filter the lines by comment controlflow (lines starting with #! or comment_string !) *)
  let filter_control_flow varlist lines =
    let line_to_lb line = if Str.string_match (re_set ()) line 0 then Content line else
      if Str.string_match (re_line_command ()) line 0 then
        Str.matched_group 1 line |> find_logic_block else Content line
    in
    lines |> List.map line_to_lb |> resolve_control_flow varlist
    |> List.map (fun x -> match x with Content l -> l | _ -> failwith "expected content")

  (* execute external command or calculation and print output *)
  let resolve_external line varlist =
    let string_list = Str.full_split (re_external ()) line
                      |> List.map (function Str.Delim x -> x | Str.Text x -> x) in
    let rec aux f acc strl =
      if List.empty strl then (match f with Some f -> f acc | None -> acc), [] else
        let x, rl = List.hd strl, List.tl strl in
        let l,r = String.get !parenthesies 0, String.get !parenthesies 1 in
        if x = (sprintf "$$%c" l) then
          let str, rl = aux (Some (fun x -> Math.eval_string x varlist |> string_of_int)) "" rl
          in aux f (acc ^ str) rl
        else if x = (sprintf "%c" l) then
          let str, rl = aux None "{" rl in aux f (acc ^ str) rl
        else if x = (sprintf "$%c" l) then
          let str, rl = aux (Some Util.external_command_output) "" rl in aux f (acc ^ str) rl
        else if x = (sprintf "%c" r) then
          (match f with Some f -> f acc | None -> acc ^ "}"), rl
        else aux f (acc ^ x) rl
    in
    let line, _ = aux None "" string_list in line

  (* process a file name *)
  let rec resolve_file ifile oc varlist = let ifile = Util.absolute_path ifile in
    let parenths,comment_str = !parenthesies, !comment_string in
    let lines = read_lines_file ifile |> set_parameters |> filter_comments_escaped_lines |> filter_control_flow varlist in
    let varlist = resolve_lines lines oc varlist in
    parenthesies := parenths; comment_string := comment_str;
    varlist

  (* process a list of lines *)
  and resolve_lines lines oc varlist =
    let resolve_block_fold varlist block = resolve_line block oc varlist in
    List.fold_left resolve_block_fold varlist lines

  (* process a single line *)
  and resolve_line line oc varlist =
    let line = resolve_external line varlist in
    if Str.string_match re_include line 0 then
      let path = Str.matched_group 1 line in
      let varlist = resolve_file (Util.absolute_path path) oc varlist in
      varlist
    else if Str.string_match (re_exist_logic_block ()) line 0 then
      handle_logic_blocks line oc varlist
    else if Str.string_match (re_set ()) line 0 then
      let comment = try Str.matched_group 1 line with Not_found -> "" in
      let key = Str.matched_group 2 line in
      let value = (Str.matched_group 3 line) in
      let value = resolve_variable varlist value in
      fprintf oc "%sset $%s %s\n" comment key value;
      (key,value)::varlist
    else
      (fprintf oc "%s\n" line; varlist)
  (* handle lines containing logic blocks {...} *)
  and handle_logic_blocks line oc varlist =
    let logic_list  = Str.full_split (re_logic_block ()) line |> List.map
                        (function Str.Text s -> Content s
                                | Str.Delim s -> let c = String.sub s 1 ((String.length s)-2) in
                                  find_logic_block c)
    in
    let new_lines = logic_list |> resolve_control_flow varlist |> resolve_listings in
    resolve_lines new_lines oc varlist

  let resolve_string string oc varlist =
    let lines = String.split_on_char '\n' string |> set_parameters |> filter_comments_escaped_lines |> filter_control_flow varlist
    in resolve_lines lines oc varlist
end

(* main *)
let () =
  Arg.parse
    speclist
    (fun x -> match !input_filename, !output_filename with "",_ -> input_filename := x
                                                         | _,"" -> output_filename := x | _ -> raise (Arg.Bad "all files already specified"))
    usage;
  let out_channel = let o = !output_filename in if o = "" then stdout else open_out o in
  if !input_filename="" then raise (Arg.Bad "no input file specified") else ();
  if (String.length !parenthesies) != 2 then raise (Arg.Bad "bad argument to -p") else ();
  fprintf out_channel "%s\n" !disclaimer;

  let envlist = (Unix.environment () |> Array.map (String.split_on_char '=') |> Array.map
                   (fun l -> List.hd l, List.tl l |> String.concat "=") |> Array.to_list) in
  let varlist = if !use_env then !varlistref @  envlist else !varlistref in

  try Preprocessor.resolve_file !input_filename out_channel varlist |> ignore;
    close_out out_channel
  with x -> close_out out_channel; raise x
