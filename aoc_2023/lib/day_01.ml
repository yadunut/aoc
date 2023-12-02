open Base

let is_alpha = function 'a'..'z' | 'A'..'Z' -> true | _ -> false
let is_digit = function '0'..'9' -> true | _ -> false

let join_characters a b = String.make 1 a ^ String.make 1 b


let numbers = [
  ("one", 1);
  ("two", 2);
  ("three", 3);
  ("four", 4);
  ("five", 5);
  ("six", 6);
  ("seven", 7);
  ("eight", 8);
  ("nine", 9);
  ("1", 1);
  ("2", 2);
  ("3", 3);
  ("4", 4);
  ("5", 5);
  ("6", 6);
  ("7", 7);
  ("8", 8);
  ("9", 9);
]

let solve_part1 (str: string): (int) = 
str 
  |> String.filter ~f:is_digit
  |> function 
  | "" -> 0
  | x -> Int.of_string (join_characters x.[0] x.[(String.length x)-1])
  
  
let str_begins_with (str: string) = 
  numbers
  |> List.filter ~f:(fun (num, _) -> String.is_prefix str ~prefix:num)
  |> List.map ~f:(function (_, i) -> i)
  |> List.find ~f:(fun _ -> true) (* hacky way to get first element *)

let str_ends_with (str: string) = 
  let str = String.rev str in
  numbers
  |> List.filter ~f:(fun (num, _) -> String.is_prefix str ~prefix:(String.rev num))
  |> List.map ~f:(function (_, i) -> i)
  |> List.find ~f:(fun _ -> true) (* hacky way to get first element *)

let rec first_num (s: string) = 
  match str_begins_with s with
    | Some x -> x
    | None -> first_num (Stdlib.String.sub s 1 ((String.length s)-1))

let rec last_num (s: string) = 
  match str_ends_with s with
    | Some x -> x
    | None -> last_num (Stdlib.String.sub s 0 ((String.length s)-1))

let solve_part2 (s: string) = 
  let first_num = first_num s in
  let last_num = last_num s in
  first_num * 10 + last_num



let%test "str_begins_with" = Option.equal Int.equal (str_begins_with "1twothree") (Option.some 1)
let%test "str_ends_with" = Option.equal Int.equal (str_ends_with "threetwo") (Option.some 2)
let%test "first_num" = Int.equal (first_num "aaaaaaaeight6twojtzlvlhgjncvx") 8
let%test "first_num" = Int.equal (first_num "eight6twojtzlvlhgjncvx") 8
let%test "last_num" = Int.equal (last_num "eight6twojtzlvlhgjncvx") 2
let%test "solve_part2" = Int.equal (solve_part2 "two1nine") 29

let part1 (input_text: string): (string) = 
  input_text 
  |> String.split_lines
  |> List.map ~f:solve_part1 
  |> List.fold ~f:(fun acc n -> acc + n) ~init:0
  |> Int.to_string

let part2 (input_text: string): (string) = 
  input_text
  |> String.split_lines
  |> List.map ~f:solve_part2
  |> List.fold ~f:(fun acc n -> acc + n) ~init:0
  |> Int.to_string

