
type dlist = int list

(* Exponentiation by squaring implementation from
 * https://en.wikipedia.org/wiki/Exponentiation_by_squaring *)
let int_exp a b =
  let rec helper acc a b =
    if b < 0 then failwith "only positive exponents"
    else if b = 0 then acc
    else if b = 1 then a * acc
    else if b mod 2 = 0 then helper acc (a*a) (b/2)
    else helper (acc*a) (a*a) ((b-1)/2)
  in
  helper 1 a b

let int_exp_mod a b m =
  let rec helper acc a b =
    if b < 0 then failwith "only positive exponents"
    else if b = 0 then acc
    else if b = 1 then a*acc mod m
    else if b mod 2 = 0 then helper acc (a*a mod m) (b/2)
    else helper (acc*a mod m) (a*a mod m) ((b-1)/2)
  in
  helper 1 a b

let digit_list n =
  let rec helper acc d =
    if d < 10 then d:: acc
    else helper ((d mod 10)::acc) (d / 10) in
  helper [] n

let base_10_rep b dlst =
  let len = List.length dlst in
  let rec helper acc exp = function
    | [] -> acc
    | h::t -> helper (acc + h * (int_exp b exp)) (exp-1) t
  in
  helper 0 (len-1) dlst

let dlst_to_string dlst =
  let rec helper acc lst =
    match lst with
    | [] -> acc
    | h::t -> helper (acc^string_of_int h) t
  in
  helper "" dlst

let base_convert bold bnew dlst =
  let n = base_10_rep bold dlst in
  let rec helper acc = function
    | 0 -> acc
    | x -> helper ((x mod bnew)::acc) (x/bnew)
  in
  helper [] n

let pad_zeros totalbits bitlst =
  let rec helper acc morebits =
    if morebits = 0 then acc
    else if morebits < 0 then failwith "padding negative number of zeros"
    else helper (0::acc) (morebits-1)
  in
  helper bitlst (totalbits-(List.length bitlst))

let add_mod_2 lst1 lst2 =
  if List.length lst1 <> List.length lst2
    then failwith "list are different lengths"
  else
    List.fold_left2 (fun acc a b -> ((a + b) mod 2)::acc) [] lst1 lst2

let split inbits lst =
  let arr = Array.of_list lst in
  let len = Array.length arr in
  let outbits = len - inbits in
  (Array.sub arr 0 inbits, Array.sub arr inbits outbits)
  |> (fun (a1,a2) -> (Array.to_list a1, Array.to_list a2))