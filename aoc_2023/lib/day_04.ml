open! Core
let nums_to_list (s: string option): (int list) = 
  match s with
  | None -> failwith "invalid case i nums_to_list"
  | Some(s) -> let result = List.map (String.split s ~on:' ') ~f:Int.of_string_opt in
    List.filter_map result ~f:Fun.id

module Ints = Set.Make(Int)

let calc_matches (s: string): (int) = 
  let split = List.map ~f:String.strip @@ String.split_on_chars s ~on:[':';'|'] in
  let winning = Ints.of_list @@ nums_to_list @@ List.nth split 1 in
  let pile =  Ints.of_list @@ nums_to_list @@ List.nth split 2 in
  let intersection = Set.inter winning pile in
  Set.length intersection

let calc_points (s: string): (int) = 
  let points = calc_matches s in
  match points with 
  | 0 -> 0
  | points -> Int.pow 2 (points-1)

  
  (* the 1st and 2nd are the lists *)

let part1 (s: string) = 
  let input = String.split_lines @@ String.strip s in
  List.map input ~f:calc_points |>
  List.fold ~f:(fun acc n -> acc + n) ~init:0 |>
  Int.to_string
  
type card = {
  num: int;
  quantity: int;
}

module Mapping = Map.Make(Int)

let part2 s = 
  let input = String.split_lines @@ String.strip s in
  let alist = List.map (List.range 0 ((List.length input))) ~f:(fun i-> (i, 1)) in
  let mapping = Mapping.of_alist_exn alist in
  let mapping = List.foldi input ~init:mapping ~f:(fun idx mapping s ->
    let matches = calc_matches s in
    let ranges = List.range (idx+1) (min (idx+matches+1) (List.length input)) in
    let multiplier = Map.find_exn mapping idx in
    List.fold ranges ~init:mapping ~f:(fun mapping num ->
      Map.update mapping num ~f:(fun curr -> 
        match curr with 
        | None -> multiplier
        | Some(x) -> x + multiplier
      ))) in
  Map.fold mapping ~init:0 ~f:(fun ~key:_ ~data:d acc -> acc + d)
  |> Int.to_string

