open Puzzle

type direction = Down | Across

module IntMap = Map.Make(struct type t = int let compare = compare end)

type cell = {
  solution: string;
  contents: string option;
  number: int option;
  verified: bool;
}

type clue_set = {
  down: string IntMap.t;
  across: string IntMap.t;
}

type cell_grid = cell option array

type crossword = {
  n_rows: int;
  n_cols: int;
  clues: clue_set;
  (* None cells are black cells. Some cells are white cells with a clue. *)
  grid: cell_grid;
}

module Crossword = struct
  let empty (n_rows: int) (n_cols: int) : crossword = {
    n_rows = n_rows;
    n_cols = n_cols;
    clues = {down = IntMap.empty; across = IntMap.empty};
    grid = Array.make (n_rows * n_cols) (Some {solution = ""; contents = None; number = None; verified = false;});
  }
  let get_cell (crossword: crossword) (row: int) (col: int) : cell option =
    let row_out_of_bounds = row < 0 || row >= crossword.n_rows in
    let col_out_of_bounds = col < 0 || col >= crossword.n_cols in
    if row_out_of_bounds || col_out_of_bounds then
      None
    else
      crossword.grid.(row * crossword.n_cols + col)

  let set_cell (crossword: crossword) (row: int) (col: int) (cell: cell option) : unit =
    let row_in_bounds = row >= 0 && row < crossword.n_rows in
    let col_in_bounds = col >= 0 && col < crossword.n_cols in
    if col_in_bounds && row_in_bounds then
      crossword.grid.(row * crossword.n_cols + col) <- cell
    else
      raise (Invalid_argument "Row or column out of bounds")

  let find_cell_by_number (crossword: crossword) (number: int) : (int * int) option =
    let rec aux idx =
      if idx >= Array.length crossword.grid then
        None
      else
        match crossword.grid.(idx) with
        | Some cell when cell.number = Some number ->
            let row = idx / crossword.n_cols in
            let col = idx mod crossword.n_cols in
            Some (row, col)
        | _ -> aux (idx + 1)
    in
    aux 0

  let next_position direction row col =
    match direction with
    | Across -> (row, col + 1)
    | Down -> (row + 1, col)

  let fold_replace_word (crossword: crossword) (number: int) (direction: direction)
      (init: 'acc)
      (f: 'acc -> crossword -> int -> int -> cell option -> (cell option * 'acc)) : 'acc =
    let in_bounds row col =
      row >= 0 && row < crossword.n_rows && col >= 0 && col < crossword.n_cols
    in
    let rec aux row col acc =
      if not (in_bounds row col) then
        acc
      else
        let idx = row * crossword.n_cols + col in
        let cell = crossword.grid.(idx) in
        let cell, acc = f acc crossword row col cell in
        match cell with
        | Some _ ->
            crossword.grid.(idx) <- cell;
            let next_row, next_col = next_position direction row col in
            aux next_row next_col acc
        | None -> acc
    in
    match find_cell_by_number crossword number with
    | None -> init
    | Some (row, col) -> aux row col init

  let fold_replace_board (crossword: crossword) (init: 'acc) (f: 'acc -> crossword -> int -> int -> cell option -> (cell option * 'acc)) : 'acc =
    (* so many side effects... *)
    let acc, _ = Array.fold_left (fun acc cell ->
      let (inner_acc, idx) = acc in
      let col, row = idx mod crossword.n_cols, idx / crossword.n_cols in
      let new_cell, inner_acc = f inner_acc crossword row col cell in
      crossword.grid.(idx) <- new_cell;
      (inner_acc, idx + 1)
    ) (init, 0) crossword.grid in acc

  let set_numbers (crossword: crossword) : unit = 
    let number_handler number crossword row col cell =
      match cell with
      | None -> (None, number)
      | Some cell ->
        let start_across = col = 0 || (get_cell crossword (col - 1) row = None) in
        let start_down = row = 0 || (get_cell crossword (col - 1) row = None) in
        if start_across || start_down then
          (Some {cell with number = Some number}, number + 1)
        else
          (Some {cell with number = None}, number)
    in
    let _ = fold_replace_board crossword 1 number_handler in
    ()
end
let puzzle_to_crossword (puzzle: puzzle) : crossword =
  let xword = {
    n_rows = puzzle.height;
    n_cols = puzzle.width;
    clues = {down = IntMap.empty; across = IntMap.empty};
    grid = Array.map (fun ch ->
      if ch = '.' then
        None
      else
        Some {
          solution = (String.make 1 ch);
          contents = None;
          number = None;
          verified = false;
        }
      ) (puzzle.solution |> String.to_seq |> Array.of_seq)
  } in
  Crossword.set_numbers xword;
  xword

type cell_render = string * string * string
let render_cell (cell: cell option) : cell_render =
  match cell with
  | Some cell -> 
    let verified_marker = if cell.verified then "/" else " "
    in (
      (match cell.number with
      | Some n -> (Printf.sprintf "%02d     " n)
      | None -> "       "),
      (match cell.contents with
      | Some c -> (Printf.sprintf "  %s%s%s  " verified_marker c verified_marker)
      | None -> "       "),
      "       "
    )
  | None -> ("###### ", "###### ", "###### ")


let render_crossword (xword: crossword) : string =
  let rendered_cells = Array.map render_cell xword.grid in
  let rendered_rows = Array.map
    (fun row ->
      let b1 = Buffer.create 128 in
      let b2 = Buffer.create 128 in
      let b3 = Buffer.create 128 in
      Array.iter (fun (row1, row2, row3) ->
        Buffer.add_string b1 row1;
        Buffer.add_string b2 row2;
        Buffer.add_string b3 row3) row;
      Buffer.add_char b1 '\n';
      Buffer.add_char b2 '\n';
      Buffer.add_buffer b1 b2;
      Buffer.add_buffer b1 b3;
      Buffer.contents b1
    )
    (Array.init_matrix xword.n_rows xword.n_cols (fun row col -> rendered_cells.(row * xword.n_cols + col))) in
  String.concat "\n" (Array.to_list rendered_rows)
