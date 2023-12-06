let read_whole_input_file filename = 
  let ic = open_in filename in
  let s = really_input_string ic (in_channel_length ic) in 
  close_in ic;
  s
