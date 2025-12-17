module LE = Cstruct.LE

(* PUZ file format reader based on the specification in puzfileformat.txt *)
(* TODO: check this *)

type puzzle = {
  width: int;
  height: int;
  num_clues: int;
  solution: string;
  grid: string;
  title: string;
  author: string;
  copyright: string;
  clues: string list;
  notes: string;
}

exception Invalid_puz_file of string

let file_magic = "ACROSS&DOWN\000"

(* Helper to split a list at a given index *)
let rec split_at n lst =
  if n <= 0 then
    ([], lst)
  else
    match lst with
    | [] -> ([], [])
    | h :: t ->
        let (first, rest) = split_at (n - 1) t in
        (h :: first, rest)

(* Read a null-terminated string from a cstruct, starting at offset *)
let read_null_string buf offset =
  let buf_len = Cstruct.length buf in
  let rec find_null pos =
    if pos >= buf_len then
      raise (Invalid_puz_file "String not null-terminated")
    else if Cstruct.get_char buf pos = '\000' then
      pos
    else
      find_null (pos + 1)
  in
  let end_pos = find_null offset in
  let len = end_pos - offset in
  let sub_buf = Cstruct.sub buf offset len in
  (Cstruct.to_string sub_buf, end_pos + 1)

(* Read all null-terminated strings from a cstruct, starting at offset *)
let read_all_strings buf offset =
  let rec read_strings acc pos =
    if pos >= Cstruct.length buf then
      List.rev acc
    else
      let (str, next_pos) = read_null_string buf pos in
      read_strings (str :: acc) next_pos
  in
  read_strings [] offset

(* Calculate CRC-16 checksum variant *)
let cksum_region buf offset len initial_cksum =
  let rec loop cksum i =
    if i >= len then
      cksum
    else
      let byte = Cstruct.get_uint8 buf (offset + i) in
      let cksum' = if cksum land 0x0001 <> 0 then
          (cksum lsr 1) + 0x8000
        else
          cksum lsr 1
      in
      loop (cksum' + byte) (i + 1)
  in
  loop initial_cksum 0

(* Verify file magic *)
let verify_magic buf =
  let magic_len = String.length file_magic in
  if Cstruct.length buf < 0x02 + magic_len then
    raise (Invalid_puz_file "File too short for magic")
  else
    let magic_sub = Cstruct.sub buf 0x02 magic_len in
    let magic_buf = Cstruct.to_string magic_sub in
    if magic_buf <> file_magic then
      raise (Invalid_puz_file ("Invalid magic: " ^ magic_buf))
    else
      ()

(* Read puzzle from file *)
let read_puzzle filename =
  let ic = open_in_bin filename in
  let file_size = in_channel_length ic in
  let bytes = Bytes.create file_size in
  really_input ic bytes 0 file_size;
  close_in ic;
  let buf = Cstruct.of_bytes bytes in

  (* Verify magic *)
  verify_magic buf;

  (* Read header fields *)
  if Cstruct.length buf < 0x34 then
    raise (Invalid_puz_file "File too short for header");

  let width = Cstruct.get_uint8 buf 0x2C in
  let height = Cstruct.get_uint8 buf 0x2D in
  let num_clues = LE.get_uint16 buf 0x2E in
  let scrambled_tag = LE.get_uint16 buf 0x32 in

  if scrambled_tag <> 0 then
    raise (Invalid_puz_file "Scrambled puzzles not supported");

  (* Calculate board size *)
  let board_size = width * height in
  let header_end = 0x34 in

  if Cstruct.length buf < header_end + (2 * board_size) then
    raise (Invalid_puz_file "File too short for board data");

  (* Read solution board *)
  let solution_sub = Cstruct.sub buf header_end board_size in
  let solution = Cstruct.to_string solution_sub in

  (* Read player state board *)
  let grid_sub = Cstruct.sub buf (header_end + board_size) board_size in
  let grid = Cstruct.to_string grid_sub in

  (* Read strings section *)
  let strings_start = header_end + (2 * board_size) in
  let strings = read_all_strings buf strings_start in

  (* Parse strings: title, author, copyright, clues, notes *)
  let (title, rest) = match strings with
    | h :: t -> (h, t)
    | [] -> ("", [])
  in
  let (author, rest) = match rest with
    | h :: t -> (h, t)
    | [] -> ("", [])
  in
  let (copyright, rest) = match rest with
    | h :: t -> (h, t)
    | [] -> ("", [])
  in
  (* Clues come next, then notes *)
  let clues_len = List.length strings - 3 in
  let (clues, notes_list) = if clues_len >= num_clues then
      let (clues_list, notes_rest) = 
        split_at (List.length strings - 3) rest in
      (clues_list, notes_rest)
    else
      (rest, [])
  in
  let notes = match notes_list with
    | h :: _ -> h
    | [] -> ""
  in

  {
    width;
    height;
    num_clues;
    solution;
    grid;
    title;
    author;
    copyright;
    clues;
    notes;
  }

