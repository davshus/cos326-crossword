open Xword.Puzzle
open Xword.Crossword

(* type direction = Down | Across *)

type region =
  | Cell of int * int
  | Word of int * direction
  | Board

type command =
  | Guess of int * int * char
  | Erase of int * int
  | Check of region
  | Reveal of region

let parse_region (args: string list) : region option =
  match args with
  | "cell" :: row_str :: col_str :: [] ->
    (try Some (Cell (int_of_string row_str, int_of_string col_str))
    with _ -> None)
  | "word" :: n :: "a" :: [] ->
    (try Some (Word (int_of_string n, Across)) with _ -> None)
  | "word" :: n :: "d" :: [] ->
    (try Some (Word (int_of_string n, Down)) with _ -> None)  
  | "board" :: [] -> Some Board
  | _ -> None

let parse_command (s: string) : command option =
  let cmd = String.split_on_char ' ' s in
  try
    match cmd with
    | "guess" :: row_str :: col_str :: guess :: [] ->
      Some (Guess (int_of_string row_str, int_of_string col_str, String.get guess 0))
    | "erase" :: row_str :: col_str :: [] ->
      Some (Erase (int_of_string row_str, int_of_string col_str))
    | "check" :: args ->
      (match parse_region args with
      | Some region -> Some (Check region)
      | None -> None)
    | "reveal" :: args ->
      (match parse_region args with
      | Some region -> Some (Reveal region)
      | None -> None)
    | _ -> None
  with
    | Failure _ -> None (* handle int_of_string *)
    | Invalid_argument _ -> None (* handle String.get *)

let guess (xword: crossword) (row: int) (col: int) (ch: char) =
  let ch = Char.uppercase_ascii ch in
  match Crossword.get_cell xword row col with
  | Some cell ->
      Crossword.set_cell xword row col (Some {cell with contents = Some (String.make 1 ch)});
      print_endline (render_crossword xword)
  | None -> print_endline "Cannot guess empty cell."
let erase (xword: crossword) (row: int) (col: int) =
  match Crossword.get_cell xword row col with
  | Some cell ->
      Crossword.set_cell xword row col (Some {cell with contents = None});
      print_endline (render_crossword xword)
  | None -> print_endline "Cannot erase empty cell."

let cell_checker (cell: cell option) : cell option =
  match cell with
  | Some cell -> Some ({
    cell with verified =
      match cell.contents with
      | None -> false
      | Some contents -> contents = cell.solution;
  })
  | None -> None

let cell_revealer (cell: cell option) : cell option = 
  match cell with
  | Some cell -> Some ({
    cell with verified = true; contents = Some (cell.solution);
  })
  | None -> None

let check (xword: crossword) (region: region) =
  (match region with
  | Cell (row, col) -> Crossword.set_cell xword row col (cell_checker (Crossword.get_cell xword row col))
  | Word (num, dir) -> Crossword.fold_replace_word xword num dir () (fun _ _ _ _ cell -> (cell_checker cell, ()))
  | Board -> Crossword.fold_replace_board xword () (fun _ _ _ _ cell -> (cell_checker cell, ())));
  print_endline (render_crossword xword); ()

let reveal (xword: crossword) (region: region) =
  (match region with
  | Cell (row, col) -> Crossword.set_cell xword row col (cell_revealer (Crossword.get_cell xword row col))
  | Word (num, dir) -> Crossword.fold_replace_word xword num dir () (fun _ _ _ _ cell -> (cell_revealer cell, ()))
  | Board -> Crossword.fold_replace_board xword () (fun _ _ _ _ cell -> (cell_revealer cell, ())));
  print_endline (render_crossword xword)

let repl (xword : crossword) =
  let handle_command (s : string) =
    match parse_command s with
    | Some (Guess (row, col, ch)) -> guess xword row col ch; ()
    | Some (Erase (row, col)) -> erase xword row col; ()
    | Some (Reveal region) -> reveal xword region; ()
    | Some (Check region) -> check xword region; ()
    | None -> ()
    (* | _ -> () TODO: remove *)
  in
  let rec loop () =
    print_string "> ";
    let input = read_line () in
    match input with
    | "quit" -> ()
    | _ ->
        handle_command input;
        loop ()
  in
  loop ()

let () =
  let puzzle_path =
    if Array.length Sys.argv > 1 then
      Sys.argv.(1)
    else
      failwith "No puzzle provided"
  in
  try
    let puzzle = read_puzzle puzzle_path in
    Printf.printf "Title: %s\n" puzzle.title;
    Printf.printf "Author: %s\n" puzzle.author;
    Printf.printf "Copyright: %s\n" puzzle.copyright;
    Printf.printf "Size: %d x %d\n" puzzle.width puzzle.height;
    Printf.printf "Number of clues: %d\n" puzzle.num_clues;
    let _ = List.map (fun clue -> print_endline clue) puzzle.clues in ();
    if puzzle.notes <> "" then
      Printf.printf "Notes: %s\n" puzzle.notes;
    Printf.printf "\nSolution grid:\n";
    for row = 0 to puzzle.height - 1 do
      for col = 0 to puzzle.width - 1 do
        let idx = row * puzzle.width + col in
        let ch = puzzle.solution.[idx] in
        if ch = '.' then
          print_string "#  "
        else
          Printf.printf "%c  " ch
      done;
      print_endline ""
    done;
    let xword = puzzle_to_crossword puzzle in
    print_endline (render_crossword xword);
    repl xword;
  with
  | Invalid_puz_file msg -> Printf.eprintf "Error reading PUZ file: %s\n" msg
  | Sys_error msg -> Printf.eprintf "File error: %s\n" msg
  | e -> Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string e)
