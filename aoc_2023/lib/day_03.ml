open! Core

type part = {
  num: int;
  row: int;
  col: int;
  len: int;
}

type symbol = {
  c: char;
  row: int;
  col: int;
}
(* recursively go through character by character
   if character is a digit, set start, increment len
   else 
   *)
let part_new s row col len = 
  let num = Int.of_string s in
  { num; row; col; len}

let generate_parts (row: int) (s: string) = 
  let rec aux (parts: part list) (start: int option) (len: int option) (idx: int) (chars: char list) = 
    match start, len, chars with
    | None, None, '0'..'9'::tail -> 
      aux parts (Some idx) (Some 1) (idx + 1) tail
    | Some(start), Some(len), '0'..'9'::tail -> 
      aux parts (Some start) (Some (len + 1)) (idx + 1) tail
    | Some(start), Some(len), _::tail -> 
      let new_part = part_new (String.slice s start (start + len)) row start len in
      aux (new_part::parts) None None (idx+1) tail
    | None, None, _::tail -> 
      aux parts None None (idx+1) tail
    | Some(start), Some(len), [] -> 
      let new_part = part_new (String.slice s start (start + len)) row start len in
      new_part::parts
    | None, None, [] -> 
      parts
    | _ -> failwith "invalid state reached in generate_parts" (* interesting way of throwing error *)
  in
  aux [] None None 0 (String.to_list s)

let generate_symbols (row: int) (s: string)  = 
  List.filter_mapi (String.to_list s) ~f:(fun idx c -> 
    match c with
    | '.' -> None
    | '0'..'9' -> None
    | c -> Some {c;row; col = idx})

let directions = [
  (-1,-1);(-1,0);(-1,1);
  (0,1);(1,1);
  (1,0);(1,-1);(0,-1)]

let valid_part (symbols: symbol list) (p: part) : (bool) = 
  List.find symbols ~f:(fun s -> 
    List.find directions ~f:(fun (r, c)-> 
      let row = s.row + r in
      let col = s.col + c in
      p.row = row && p.col <= col && (p.col + p.len) > col
    ) |> Option.is_some
  ) |> Option.is_some

let part1 (s: string) = 
  let lines = String.split_lines @@ String.strip s in
  let symbols = List.concat (List.mapi lines ~f:generate_symbols) in
  let parts = List.concat (List.mapi lines ~f:generate_parts) in
  let filtered = List.filter parts ~f:(valid_part symbols) in
  List.fold filtered ~f:(fun acc n -> acc + n.num) ~init:0 
  |> Int.to_string

let part2 s = 
  let lines = String.split_lines @@ String.strip s in
  let symbols = List.concat (List.mapi lines ~f:generate_symbols) in
  let gears = List.filter symbols ~f:(fun s -> Char.equal '*' s.c) in
  let parts = List.concat (List.mapi lines ~f:generate_parts) in
  let result = List.filter_map gears ~f:(fun g -> 
    let parts = List.filter parts ~f:(fun p -> 
      List.find directions ~f:(fun (r,c) -> 
        let row = g.row + r in
        let col = g.col + c in
        p.row = row && p.col <= col && (p.col + p.len) > col
      ) |> Option.is_some
    ) in
    if (List.length parts) = 2 then
      (Some (List.fold parts ~init:1 ~f:(fun acc p -> acc * p.num)))
    else 
      None
  ) in
  List.fold result ~f:(fun acc n -> acc + n) ~init:0 
  |> Int.to_string

