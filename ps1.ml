(*requires: an int list
 *returns: true if int list is monotonically increasing and false otherwise*)
let rec is_mon_inc (lst: int list):bool = 
    match lst with
	[] -> true 
	| [h] -> true
	| h::t -> h<=List.hd(t) && is_mon_inc (List.tl(lst))

(* requires: an int list
 * returns: true if int list is monotonically decreasing and false otherwise *)
let rec is_uni_dec (lst:int list): bool=
    match lst with 
	[] -> true
	| [h] -> true
	| h::t -> h>=List.hd(t) && is_uni_dec (List.tl(lst))

(* requires: an int list
 * returns: true if int list is unimodal and false otherwise *)
let rec is_unimodal (lst: int list):bool =
    match lst with 
	[] -> true
	| [h] -> true
	| h::t -> if h<=List.hd(t) then is_unimodal(List.tl(lst)) 
              else is_uni_dec(lst)

(* requires: an int list list and an int
 * returns: the int list list with the int cons'ed to each int list
 * example: insert_el([[];[1]],2) -> [[2];[2;1]] *)
let rec insert_el ((lst:(int list) list),( el: int)): int list list =
    match lst with 
    [] -> []
    | [[]] -> [[el]]
    | h::t -> (el::h)::insert_el(t,el)

(* requires: an int list 
 * returns: an int list list, the powerset of the int list *)
let rec powerset (lst: int list): int list list =
	match lst with 
	[] -> []::[]
	| [a] -> []::[[a]]
	| h::t -> insert_el(powerset(t),h)@powerset(t)

(* requires: a string and an index
 * returns: a string, the reverse of the input keeping the sign *)
let rec rev_string ((x:string),(index:int)):string =
	if(String.length(x)=index) then "" 
	else rev_string((x),(index+1)) ^ Char.escaped(x.[index])
(* requires: an int
 * returns: an int, the reverse of the input keeping the sign *)
let rec rev_int (x:int):int =
	if x>0 then int_of_string(rev_string(string_of_int(x), 0))
	else -1*int_of_string(rev_string(string_of_int(-1*x), 0))