open Complex

type st = Tree of st*st | Node of Complex.t

let zero = Tree (Node Complex.one, Node Complex.zero)

let one = Tree (Node Complex.zero, Node Complex.one)

(* [int_pow a b] returns [a]^[b] i.e. [a] to the power [b].
 * requires:
 * - a : int
 * - b : int *)
let rec int_pow a b = if b = 0 then 1 else a * (int_pow a (b-1))

(* [empty n] returns an empty state with n number of bits. Empty state means
 * that all of the coefficients are 0. *)
let rec empty n =
  if n = 0 then Node Complex.zero else Tree (empty (n-1), empty (n-1))

(* [log2 n ] returns log of [n] to the base 2
 * requires: 
 * - n : int *)
let rec log2 n =
  if n = 1 then Some 0
  else if n mod 2 = 0 
  then
    match log2 (n/2) with
    | Some n -> Some (n+1)
    | None -> None
  else None

(* [n_bits s] returns the number of bits in state [s]
 * requires:
 * - s : st *)
let rec n_bits = function
  | Tree (s, _) -> 1 + n_bits(s)
  | Node _ -> 0

let coeff s =
  let rec helper s =
    match s with
    | Tree (s1, s2) -> helper s1 @ helper s2
    | Node c -> c :: [] in
  helper s

let normalize s =
  let rec mag s =
    match s with
    | Tree (s1, s2) -> mag s1 +. mag s2
    | Node c -> Complex.norm2 c in
  let m = Complex.polar (Pervasives.sqrt (mag s)) 0. in
  let rec helper s =
    match s with
    | Tree (s1, s2) -> Tree (helper s1, helper s2)
    | Node c -> Node (Complex.div c m) in
  helper s

let make lst =
  let size = 
    match log2 (List.length lst) with
    | Some n ->
        if n <= 0 then failwith "Improper length of list in State.make" else n
    | None -> failwith "Improper length of list in State.make" in
  let arr = Array.of_list lst in
  let rec helper lev n =
    if lev = 0 
    then Node arr.(n)
    else Tree (helper (lev-1) n, helper (lev-1) (n+(int_pow 2 (lev-1)))) in
  normalize (helper size 0)

let add s1 s2 =
  let rec helper s1 s2 =
    match (s1, s2) with
    | (Tree (s1a, s1b), Tree (s2a, s2b)) -> 
        Tree (helper s1a s2a, helper s1b s2b)
    | (Node c1, Node c2) -> Node (Complex.add c1 c2)
    | _ -> failwith "Incorrect sizes in adding states" in
  helper s1 s2

let rec phase ang s =
  match s with
  | Tree (s1, s2) -> Tree (phase ang s1, phase ang s2)
  | Node c -> Node (Complex.mul (Complex.polar 1. ang) c)

(* [tensor_two s1 s2] returns the tensor product of two states [s1] and [s2],
 * with the coefficients from [s1] contributing to the more significant bits,
 *  -- meaning that the function returns s1*s2
 * requires:
 * - s1 : st
 * - s2 : st *)
let tensor_two s1 s2 =
  let coef1 = Array.of_list (coeff s1) in
  let coef2 = Array.of_list (coeff s2) in
  let n = Array.length coef2 in
  let coef = Array.make (Array.length coef1 * n) Complex.zero in
  for i = 0 to (Array.length coef1-1) 
  do
    for j = 0 to (n-1) 
    do
      coef.(i*n+j) <- (Complex.mul coef1.(i) coef2.(j))
    done
  done;
  make (Array.to_list coef)

let tensor sl = 
  match sl with
  | h :: t -> List.fold_left tensor_two h t 
  | _ -> failwith "no states in tensor"
  
let state_of n m =
  let x = n mod (int_pow 2 m) in
  let rec helper x idx=
    if idx < 0 
    then []
    else if x >= int_pow 2 idx 
    then one :: helper (x-(int_pow 2 idx)) (idx-1)
    else zero :: helper x (idx-1) in
  tensor (helper x (m-1))

let val_string s =
  let almost_zero c = Complex.norm c < (10.)**(-6.) in
  let format_float f = 
    let s1 = string_of_float f in
    let dp_in = String.index s1 '.' in
    let nat = String.sub s1 0 (dp_in+1) in 
    let dp_str = String.sub s1 (dp_in+1) (String.length s1 - dp_in - 1) in  
    if String.contains s1 'e' 
    then "0."
    else if String.length dp_str > 3 
    then (nat ^ String.sub dp_str 0 3)
    else s1 in
  let format_comp c = 
    if format_float c.re = "0."
    then "(" ^ (format_float c.im) ^ "i)"
    else if format_float c.im = "0."
    then "(" ^ (format_float c.re) ^ ")"
    else "(" ^ (format_float c.re) ^ "+" ^ (format_float c.im) ^ "i)" in
  let rec helper sa str =
    match sa with
    | Tree (s1, s2) -> (helper s1 (str ^ "0")) ^ (helper s2 (str ^ "1"))
    | Node c ->
        if almost_zero c then "" else (format_comp c) ^ "|" ^ str ^ "> + " in
  let str = helper s "" in
  let len = String.length str in
  String.sub str 0 (len-3)
