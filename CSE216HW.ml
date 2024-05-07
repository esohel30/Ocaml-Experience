
(* Problem 1 *)

let rec compress l = 
  match l with
  | [] -> []
  | [h] -> [h]
  | h :: t -> 
      if h = List.hd t then compress t
      else h :: compress t;;
  
(* Problem 2 *)

let rec remove_if l f =
  match l with
  | [] -> []
  | [h] -> 
      if f h = true then []
      else [h] 
  | h :: t ->
      if f h = true then remove_if t f
      else h :: remove_if t f;; 

(* Problem 3 *)

let slice l i j =
  let rec aux l i j c = 
    match l with
    | [] -> []
    | h :: t -> 
        if c >= i && c < j then 
          h :: aux t i j (c+1)
        else 
          aux t i j (c+1) in
  aux l i j 0;;

(* Problem 4 *)

let equivs f l = 
  let rec insert comp acc = match acc with
  | [] -> [[comp]]
  | h :: t ->
      if f comp (List.hd h) = true then
        (h @ [comp]) :: t
      else
        h :: insert comp t in
  let rec aux l acc = match l with
  | [] -> acc
  | h :: t ->
      aux t (insert h acc) in
  aux l [];;
    

(* Problem 5 *)
(* Can we assume only valid input? *)
let rec goldbachpair n =
  let rec isprime n = match n with
  | 0 -> false
  | 1 -> false
  | _ -> 
      let rec checkzero a n =
        if (a > 1) then
          match n mod a with
          | 0 -> false
          | _ -> checkzero (a-1) n
        else true
      in checkzero (n-1) n in
  let rec formpair a n = 
    match isprime a && isprime n with
    | true -> (a, n)
    | false -> formpair (a+1) (n-1)
  in formpair 0 n;;

(*Problem 6*)

let rec identical_on f g l =
  match l with
  | [] -> true
  | h::t ->
      if (f h) = (g h) then
        identical_on f g t
      else false;;

(* Problem 7 *)

let rec pairwisefilter cmp lst =
  let rec aux lst acc =
    match lst with
    | [] -> acc 
    | [h] -> acc @ [h]
    | h :: t ->
        aux (List.tl t) (acc @ [cmp h (List.hd t)])
  in aux lst [];;

(* Problem 8 *)

(* Include a parameter but do not supply an argument for the parameter
   What's returned is a function without an application. 
   Eg. Pass in x but leave x unevaluated *)

let rec polynomial lst =
  let rec pow b e = match e with
  | 0 -> 1
  | x -> b * pow b (e-1) in
  let create_term cof exp x = cof * (pow x exp) in
  let add_terms f g x = (f x) + (g x) in
  let rec aux lst acc =
    match lst with
    | [] -> acc 
    | (c, e)::t -> aux t (add_terms acc (create_term c e)) in
  aux lst (fun x -> 0);;

(* Problem 9 *)

let rec suffixes lst = 
  let rec gen lst acc = match lst with
  | [] -> acc
  | h::t -> gen t (acc @ [h]) in
  let rec gen2 lst acc = match lst with
  | [] -> acc
  | h::t -> acc @ gen2 t [gen lst []]
  in gen2 lst [];;

(* Problem 10 *)
(* Prof. Kane said order shouldn't matter for result *)

let rec powerset lst = 
  let rec aux1 cmp acc prev = match prev with
  | [] -> acc
  | h::t -> aux1 cmp (acc @ [h @ [cmp]]) t in
  let rec aux2 lst acc = match lst with
  | [] -> acc
  | h::t -> aux2 t (aux1 h acc acc) 
  in aux2 lst [[]];;




