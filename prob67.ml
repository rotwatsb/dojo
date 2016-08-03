#load "str.cma";;
open String;;

let rec int_list strlist =
  match strlist with
  | x::xs -> if x <> ";;" then (int_of_string x)::(int_list xs) else []
  | [] -> []
		    
let rec get_lists chan lists =
  try
    get_lists chan ((int_list (Str.split (Str.regexp " ") (String.trim (input_line chan))))::lists)
  with End_of_file -> close_in chan; lists

let rec make_triangle lists triangle i =
  match lists with
  | x::xs -> Array.set triangle i (Array.of_list x);
	     make_triangle xs triangle (i - 1)
  | [] -> ()

let rec max_score triangle row col hshtbl =
  try
    Hashtbl.find hshtbl (row, col)
  with Not_found ->
    let v = Array.get (Array.get triangle row) col in
    let best_prev_score = 
      if col = 0 then if row = 0 then 0 else max_score triangle (row - 1) col hshtbl
      else if col = row then max_score triangle (row - 1) (col - 1) hshtbl
      else max (max_score triangle (row - 1) col hshtbl)
	       (max_score triangle (row - 1) (col - 1) hshtbl) in
    Hashtbl.add hshtbl (row, col) (v + best_prev_score);
    v + best_prev_score
	       

let prob67 file =
  let chan = open_in file in
  let lists = get_lists chan [] in
  let triangle = Array.make 100 (Array.make 100 0) in
  make_triangle lists triangle 99;
  let hshtbl = Hashtbl.create 5050 in
  let (m, _) = Array.fold_right (fun x a ->
				 let (cur_max, i) = a in
				 let score = max_score triangle 99 i hshtbl in
				 if score > cur_max
				 then (score, i + 1)
				 else (cur_max, i + 1))
				(Array.get triangle 99)
				(min_int, 0) in
  m

