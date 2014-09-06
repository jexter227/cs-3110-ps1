(*Takes an input of an int list and returns true if each element in the list is greater
 *than or equal to the element before it and false otherwise
 *requires: an int list
 *returns: true if the int list monotonically increaing and false otherwise*)
let rec is_mon_inc (lst: int list):bool = 
	match lst with
	[] -> true 
	| [h] -> true
	| h::t -> h<=List.hd(t) && is_mon_inc (List.tl(lst))
