open! Core
let parse_game = function
  | None -> 0
  | Some(x) -> match List.nth (String.split ~on:' ' x) 1  with
    | None -> 0
    | Some(x) -> Int.of_string x

let%test "parse_game" = Int.equal (parse_game (Some "Game 100")) 100


let valid_cube (s: string) = 
  let split = String.split s ~on:' ' in
  let num = Int.of_string (Option.value (List.nth split 0) ~default:"0") in
  let color = List.nth split 1 in
  match color with
  | None -> false
  | Some(c) -> match c.[0] with
    | 'r' -> num <= 12
    | 'g' -> num <= 13
    | 'b' -> num <= 14
    | _ -> false

let%test "valid_cube" = valid_cube "3 blue"
let%test "valid_cube" = not @@ valid_cube "20 red"

let validate_match = function
  | None -> true
  | Some(matches) -> 
    let matches = String.split_on_chars matches ~on:[';'; ','] in
    let results = List.map matches ~f:(fun m-> valid_cube @@ String.strip m) in
    List.for_all results ~f:Fun.id

let%test "validate_match" = validate_match @@ Some "3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
let%test "validate_match" = not @@ validate_match @@ Some "1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"

let solve_part1 (s: string) = 
  let split = String.split s ~on:':' in 
  let gameNo = parse_game @@ List.hd split in
  let valid = validate_match (List.nth split 1) in
  match valid with
    | true -> gameNo
    | false -> 0

let%test "solve_part1" = Int.equal (solve_part1 "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green") 1
let%test "solve_part1" = Int.equal (solve_part1 "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue") 2
let%test "solve_part1" = Int.equal (solve_part1 "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red") 0
let part1 (input: string)= 
  input
  |> String.split_lines
  |> List.map ~f:solve_part1
  |> List.fold ~f:(fun acc n -> acc + n) ~init:0
  |> Int.to_string

type set = {
  red: int;
  green: int;
  blue: int;
}

let compare_set a b = 
  a.red = b.red && a.green = b.green && a.blue = b.blue

let calc_cube (s: string) = 
  let split = String.split s~on:' 'in
  let num = Int.of_string (Option.value (List.nth split 0) ~default:"0") in
  let color = List.nth split 1 in
  match color with
  | None -> { red = 0; green = 0; blue = 0}
  | Some(c) -> match c.[0] with
    | 'r' -> { red = num; green = 0; blue = 0}
    | 'g' -> { red = 0; green = num; blue = 0}
    | 'b' -> { red = 0; green = 0; blue = num}
    | _ -> { red = 0; green = 0; blue = 0}

let%test "calc_cube" = compare_set (calc_cube "3 red") {red=3;green=0;blue=0}
let%test "calc_cube" = compare_set (calc_cube "6 blue") {red=0;green=0;blue=6}
let%test "calc_cube" = compare_set (calc_cube "2 green") {red=0;green=2;blue=0}

let calc_match (s: string) = 
  let split = String.split s ~on:',' in
  let sets = List.map split ~f:(fun spl -> calc_cube @@ String.strip spl) in
  let result = List.reduce sets ~f:(fun a b -> {
    red = a.red + b.red;
    green = a.green + b.green;
    blue = a.blue + b.blue;
  }) in
  Option.value result ~default:{red=0;green=0;blue=0;}

let%test "calc_match" = compare_set (calc_match "3 blue, 4 red") {red=4;blue=3;green=0}

let solve_part2 (s: string) = 
  let sets = List.nth (String.split s ~on:':') 1 in
  match sets with
  | None -> 0
  | Some(sets) -> 
  let sets = String.split sets ~on:';' in
  let matches = List.map sets ~f:calc_match in
  let result = List.reduce matches ~f:(fun a b -> {
    red = max a.red b.red;
    green = max a.green b.green;
    blue = max a.blue b.blue;
  }) in
  let result = Option.value result ~default:{red=0;green=0;blue=0;} in
  result.red * result.green * result.blue

let%test "solve_part2" = (solve_part2 "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green") = 48


let part2 (input: string): (string) = 
  input
  |> String.split_lines
  |> List.map ~f:solve_part2
  |> List.fold ~f:(fun acc n -> acc + n) ~init:0
  |> Int.to_string
