(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup = let (a,b,c) = tup in (c,b,a)

let is_odd x = if x mod 2 = 0 then 
		false
		else 
		true 

let area x y = let (a,b) = x in
		let (c,d) = y in 
		 abs(a-c) * abs(b-d) 

let volume x y = let (a,b,c) = x in 
		  let (d,e,f) = y in 
		   abs(a-d) * abs(b-e) * abs(c-f) 

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n =  
	if n <= 1 then 
	 n
	else 
	 fibonacci (n-1) + fibonacci (n-2)  

let rec pow x y = 
	if y = 0 then
	 1
	else if y = 1 then
	 x
	else 
	 x * pow x (y-1)

let rec log x y = 
	if y < x then
	 0
	else if y = x then
	 1
	else 
	 1 + log x (y/x)

let rec gcf x y = 
	if y = 0 then 
	 x
	else 
	 gcf y (x mod y)

let rec is_prime x = if x = 0 || x =1 then 
			false
		else if x mod 2 = 0 && x != 2 then 
			false
		else if x mod 3 = 0 && x != 3 then
			false
		else if x mod 5 = 5 && x != 5 then
			false
		else if x mod 7 = 0 && x != 7 then
			false
		else if x mod 11 = 0 && x != 11 then
			false
		else 
		 	true

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get idx lst = 
	if idx = 0 then 
	   match lst with 
       	   | (h::_) -> h
	   | [] -> failwith "Out of bounds"
	else 
	   match lst with
	   | (h::t) -> (get (idx-1) t)
   	   | [] -> failwith "Out of bounds"


let rec len_aux lst = match lst with
	| [] -> 0
	| _::t -> 1 + (len_aux t) 

let larger lst1 lst2 = let a = (len_aux lst1) in
			let b = (len_aux lst2) in
			if a > b then
			  lst1
			else if b > a then
			  lst2
			else 
			   []

let rec append a b = match a with
		| [] -> b
		| (x::xs) -> x::(append xs b)

let rec reverse lst = 
	match lst with
	| [] -> []
	| (x::xs) -> append (reverse xs) (x::[])


let rec combine lst1 lst2 = match (lst1,lst2) with
	| [],[] -> []
	| h::t, [] -> h::t
	| [],y::ys -> y::ys
	| h::t,y::ys -> if h>y then
			  y :: (combine lst1 ys)
			else if h<y then
			  h :: (combine t lst2)
			else 
			  h :: (combine t ys)

let rec merge lst1 lst2 = match (lst1,lst2) with
	 | [],[] -> []
	 | h::t, [] -> h :: (merge t lst2)
	 | [], x::xs -> x :: (merge lst1 xs)  
	 | h::t , x::xs -> if h<x then 
			h :: (merge t lst2)
			else 
			x :: (merge lst1 xs)
 

let rec rotate shift lst = 
		if shift = 0 then
	       	  lst
		else
		  match lst with
		  | (x::xs) -> rotate (shift-1) (append (xs) (x::[]))
		  | [] -> []

let rec is_palindrome lst = 
		let x = reverse lst in 
		if x = lst then
		 true
		else 
		 false
