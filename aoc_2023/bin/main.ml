let usage_msg = ""
let day = ref 0

let anon_fun inDay = day := int_of_string(inDay)



let () = 
  let () = Arg.parse [] anon_fun usage_msg in
  let (part1, part2) = match !day with 
    | 1 -> Aoc_2023.Day_01.(part1, part2)
    | 2 -> Aoc_2023.Day_02.(part1, part2)
    | 3 -> Aoc_2023.Day_03.(part1, part2)
    | 4 -> Aoc_2023.Day_04.(part1, part2)
    | _ -> raise (Invalid_argument "Aoc Day is not specified")
  in 
  let input_text = String.trim @@ Aoc_2023.Utils.read_whole_input_file ("input/day" ^ string_of_int !day) in
  let answer_text1 = input_text |> part1 in
  let answer_text2 = input_text |> part2 in
  print_endline answer_text1;
  print_endline answer_text2

