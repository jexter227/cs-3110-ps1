(*requires: an int list
 *returns: true if int list is monotonically increasing and false otherwise*)
let rec is_mon_inc (lst: int list):bool = 
	match lst with
	[] -> true 
	| [h] -> true
	| h::t -> h<=List.hd(t) && is_mon_inc (List.tl(lst))

(* requires: an int list
 * returns: true if int list is monotonically decreasing and false otherwise*)
let rec is_uni_dec (lst:int list): bool=
	match lst with 
	[] -> true
	| [h] -> true
	| h::t -> h>=List.hd(t) && is_uni_dec (List.tl(lst))

(* requires: an int list
 * returns: true if int list is unimodal and false otherwise*)
let rec is_unimodal (lst: int list):bool =
	match lst with 
	[] -> true
	| [h] -> true
	| h::t -> if h<=List.hd(t) then is_unimodal(List.tl(lst)) 
              else is_uni_dec(lst)