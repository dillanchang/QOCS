open State
open Arithmetic

let pi = 4.0 *. atan 1.0

(* Cardinal directions in 3 dimensions *)
type card_dir = XHat | YHat | ZHat

(* Types of the fundamental gates *)
type t =
  I | X of int | Y of int | Z of int | M of int | H of int
  | Perm | Phase of float | U of card_dir*int*float
  | Product of t list | Cont of int list * t
  | Uf of (int -> int) * int

let gate_of s =
  let gate_of_exn = Failure "Invalid string in gate_of" in
  let split_s = Str.split (Str.regexp "[,]") (String.trim s) in
  let (head,tail) = match split_s with
  | h::t -> (h,t)
  | _ -> raise gate_of_exn in
  match head with
  | "I" -> if(List.length tail >= 0) then I else raise gate_of_exn
  | "X" ->
    (let n_str = match tail with
    | v::[] -> v
    | _ -> raise gate_of_exn in
    let n = try(int_of_string n_str) with
    | Failure _ -> raise gate_of_exn in
    if(n<0) then
      raise gate_of_exn
    else
      X(n))
  | "Y" ->
    (let n_str = match tail with
    | v::[] -> v
    | _ -> raise gate_of_exn in
    let n = try(int_of_string n_str) with
    | Failure _ -> raise gate_of_exn in
    if(n<0) then
      raise gate_of_exn
    else
      Y(n))
  | "Z" ->
    (let n_str = match tail with
    | v::[] -> v
    | _ -> raise gate_of_exn in
    let n = try(int_of_string n_str) with
    | Failure _ -> raise gate_of_exn in
    if(n<0) then
      raise gate_of_exn
    else
      Z(n))
  | "H" ->
    (let n_str = match tail with
    | v::[] -> v
    | _ -> raise gate_of_exn in
    let n = try(int_of_string n_str) with
    | Failure _ -> raise gate_of_exn in
    if(n<0) then
      raise gate_of_exn
    else
      H(n))
  | "M" ->
    (let n_str = match tail with
    | v::[] -> v
    | _ -> raise gate_of_exn in
    let n = try(int_of_string n_str) with
    | Failure _ -> raise gate_of_exn in
    if(n<0) then
      raise gate_of_exn
    else
      M(n))
  | "PHASE" ->
    (let f_str = match tail with
    | v::[] -> v
    | _ -> raise gate_of_exn in
    let f = try(float_of_string f_str) with
    | Failure _ -> raise gate_of_exn in
    Phase(f))
  | "U" ->
    (let (dir,n_str,f_str) = match tail with
    | v1::v2::v3::[] -> (v1,v2,v3)
    | _ -> raise gate_of_exn in
    let n = try(int_of_string n_str) with
    | Failure _ -> raise gate_of_exn in
    let f = try(float_of_string f_str) with
    | Failure _ -> raise gate_of_exn in
    if(n<0) then raise
      gate_of_exn
    else
      match dir with
      | "X" -> U(XHat,n,f)
      | "Y" -> U(YHat,n,f)
      | "Z" -> U(ZHat,n,f)
      | _ -> raise gate_of_exn )
  | "CNOT" ->
    (let (n1_str,n2_str) = match tail with
    | v1::v2::[] -> (v1,v2)
    | _ -> raise gate_of_exn in
    let n1 = try(int_of_string n1_str) with
    | Failure _ -> raise gate_of_exn in
    let n2 = try(int_of_string n2_str) with
    | Failure _ -> raise gate_of_exn in
    if(n1<0||n2<0) then raise
      gate_of_exn
    else
      Cont([n1],X(n2)))
  | "TOF" ->
    (let (n1_str,n2_str,n3_str) = match tail with
    | v1::v2::v3::[] -> (v1,v2,v3)
    | _ -> raise gate_of_exn in
    let n1 = try(int_of_string n1_str) with
    | Failure _ -> raise gate_of_exn in
    let n2 = try(int_of_string n2_str) with
    | Failure _ -> raise gate_of_exn in
    let n3 = try(int_of_string n3_str) with
    | Failure _ -> raise gate_of_exn in
    if(n1<0||n2<0||n3<0) then raise
      gate_of_exn
    else
      Cont([n1;n2],X(n3)))
  | "Ufexp" ->
    (let (base_str, inbits_str) = match tail with
     | v1::v2::[] -> (v1,v2)
     | _ -> raise gate_of_exn in
     let base = try (int_of_string base_str) with
     | Failure _ -> raise gate_of_exn in
     let inbits = try (int_of_string inbits_str) with
     | Failure _ -> raise gate_of_exn in
     if (base<1 || inbits<0) then raise gate_of_exn
     else
       Uf((fun x -> int_exp base x), inbits))
  | "P" -> if(List.length tail >= 0) then Perm else raise gate_of_exn
  | _ -> raise gate_of_exn

let product l = Product(l)

(* Returns a gate with gate [g] being controlled by bits in [l] *)
let control l g = Cont(l,g)

(* Attaches an overall phase of [ang] to the entire state [s]. *)
let rec appPhase ang s = State.phase ang s

(* Applies the X operator on the [i]th bit on state [s] *)
let appX i s =
  (* Helper method for applying the X operator on the [lev]'th level *)
  let rec helper s lev =
    match s with
    | Tree(s0,s1) ->
      if (lev=i) then
        Tree(s1,s0)
      else Tree(helper s0 (lev-1), helper s1 (lev-1))
    | Node(c) -> Node(c) in
  helper s ((n_bits s) - 1)

(* Applies the Y operator on the [i]th bit on state [s] *)
let appY i s =
  (* [helper s lev] traverses down the state s and applies the Y operator when
   * the desired level is reached. *)
  let rec helper s lev =
    match s with
    | Tree(s0,s1) ->
      if (lev=i) then
        Tree(appPhase (pi *. -0.5) s1, appPhase (pi *. 0.5) s0)
      else Tree(helper s0 (lev-1), helper s1 (lev-1))
    | Node(c) -> Node(c) in
  helper s ((n_bits s) - 1)

(* Applies the Z operator on the [i]th bit on state [s] *)
let appZ i s =
  (* [helper s lev] traverses down the state s and applies the Z operator when
   * the desired level is reached. *)
  let rec helper s lev =
    match s with
    | Tree(s0,s1) ->
      if (lev=i) then
        Tree(s0, appPhase pi s1)
      else Tree(helper s0 (lev-1), helper s1 (lev-1))
    | Node(c) -> Node(c) in
  helper s ((n_bits s) - 1)

(* Applies the H operator on the [i]th bit on state [s] *)
let appH s i =
  let s1 = appX s i in
  let s2 = appZ s i in
  State.normalize (State.add s1 s2)

(* Applies the M operator on the [i]th bit on state [s] *)
let appM i s =
  let p0 = ref 0. in
  let p1 = ref 0. in
  (* [accProb s p] sums all of the probabilities in [s] and stores it
   * imperatively to [p] *)
  let rec accProb s p =
    match s with
    | Tree(s0,s1) -> let () = accProb s0 p in accProb s1 p;
    | Node(c) -> p := (!p +. Complex.norm2 c); in
  (* [helper s lev] traverses down s until a desired level [lev] is reached.
   *  Then, it starts summing all of the probabilities in the two subtrees *)
  let rec helper s lev =
    match s with
    | Tree(s0,s1) ->
      if (lev=i) then
        let () = accProb s0 p0 in accProb s1 p1;
      else
        let () = helper s0 (lev-1) in helper s1 (lev-1);
    | Node(c) -> failwith "invalid index in appM" in
  helper s ((n_bits s)-1);
  let col_to_zero = Random.float 1.0 < !p0 in
  (* [collapse s lev b] collapses a quantum state after a measurement has been
   * made *)
  let rec collapse s lev b =
    match s with
    | Tree(s0,s1) ->
      if (lev=i) then
        if (b) then
          Tree(s0, State.empty lev)
        else
          Tree(State.empty lev, s1)
      else
        Tree(collapse s0 (lev-1) b, collapse s1 (lev-1) b)
    | Node(c) -> failwith "invalid index in appM" in
  State.normalize (collapse s ((n_bits s)-1) col_to_zero)

(* Applies an rotation in the direction of [dir] on bit [i] on state [s]
 * with angle [ang]. *)
let appU dir i ang s =
  (* [scal s c] scales a quantum state [s] by complex number [c] *)
  let rec scal s c =
    match s with
    | Tree(s0,s1) -> Tree(scal s0 c, scal s1 c)
    | Node(coef) -> Node(Complex.mul c coef) in
  let c1 = Complex.polar (cos (ang *. 0.5)) 0. in
  let c2 = Complex.mul Complex.i (Complex.polar (sin (ang *. 0.5)) 0.) in
  let s1 = scal s c1 in
  let s2pre = match dir with
  | XHat -> appX i s
  | YHat -> appY i s
  | ZHat -> appZ i s in
  let s2 = scal s2pre c2 in
  State.normalize ( State.add s1 s2 )

(* Applies f to the state corresponding to [index] coverted to base 2
   where the sate has [inbits] number of input bits
   and [numbits] number of total bits *)
let applyf f index c csref inbits numbits =
  let cs = !csref in
  let outbits = numbits - inbits in
  base_convert 10 2 (digit_list index)
  |> pad_zeros numbits
  |> split inbits
  |> (fun (ins, outs) -> (base_convert 2 10 ins, base_convert 2 10 outs))
  |> (fun (ins, outs) -> (base_10_rep 2 ins, base_10_rep 2 outs))
  |> (fun (ins, outs) -> (ins, outs, (f ins) mod outbits))
  |> (fun (ins, outs, fin) -> (digit_list ins,
                               digit_list outs,
                               digit_list fin))
  |> (fun (ins, outs, fin) -> (base_convert 10 2 ins,
                               base_convert 10 2 outs,
                               base_convert 10 2 fin))
  |> (fun (ins, outs, fin) -> (pad_zeros inbits ins,
                               pad_zeros outbits outs,
                               pad_zeros outbits fin))
  |> (fun (ins, outs, fin) -> ins@add_mod_2 outs fin)
  |> base_10_rep 2
  |> (fun n -> cs.(n) <- (Complex.add cs.(n) c))

(* [appUf f inbits st] applies the function [f] to the input bits [inbits] of
 * the input state [st]. *)
let appUf f inbits st =
  let numbits = State.n_bits st in
  let coeffs' = Array.make (int_exp 2 numbits) Complex.zero in
  let process =
    let csref = ref coeffs' in
    List.iteri
    (fun i c ->
      if c = Complex.zero then ()
      else applyf f i c csref inbits numbits
    )
    (State.coeff st)
  in
  process; State.make (Array.to_list coeffs')

(* Maps the state |([index] converted to base 2)>
 * to the state with all its bits flipped.
 * Example: c1 |001> becomes c1 |100> *)
let reverse index c csref numbits =
  let cs = !csref in
  base_convert 10 2 (digit_list index)
  |> pad_zeros numbits
  |> List.rev
  |> base_10_rep 10
  |> (fun n -> cs.(n) <- c)

(* [appP st] applies the Permute operator on [st] *)
let rec appP st =
  let numbits = State.n_bits st in
  let coeffs' = Array.make (int_exp 2 numbits) Complex.zero in
  let process =
    let csref = ref coeffs' in
    List.iteri
    (fun i c -> reverse i c csref numbits)
    (State.coeff st)
  in
  process; State.make (Array.to_list coeffs')

let rec apply g s =
  match g with
  | I -> s
  | X(i) -> appX i s
  | Y(i) -> appY i s
  | Z(i) -> appZ i s
  | H(i) -> appH i s
  | M(i) -> appM i s
  | Phase(ang) -> appPhase ang s
  | U(dir,i,ang) -> appU dir i ang s
  | Product(gs) -> let f s g = apply g s in List.fold_left f s gs
  | Cont(conts,g) -> appCont conts g s
  | Uf(f,inbits) -> appUf f inbits s
  | Perm -> appP s

(* Applies the controled gate [g] with control bits [conts] *)
and appCont conts g s =
  let s_new = apply g s in
  (* [helper s_new s_old lev tally] applies the control gate by merging a old
   * state, s_old, and a new state, s_new. *)
  let rec helper s_new s_old lev tally =
    match (s_new,s_old) with
    | (Tree(sa0,sa1),Tree(sb0,sb1)) ->
      (if( List.exists (fun x -> x=lev) conts ) then
        (if(tally=(List.length conts-1)) then
          Tree(sb0, sa1)
        else
          Tree(
            sb0,
            (helper sa1 sb1 (lev-1) (tally+1))
          ))
      else
        Tree(
        helper sa0 sb0 (lev-1) tally,
        helper sa1 sb1 (lev-1) tally
        )
      )
    | (Node(ca),Node(cb)) -> Node(ca)
    | _ -> failwith "Error in appCont" in
  State.normalize (helper s_new s ((n_bits s) - 1) 0)

let rec gate_to_string g =
  match g with
  | I -> "I"
  | X(n) -> "X,"^(string_of_int n)
  | Y(n) -> "Y,"^(string_of_int n)
  | Z(n) -> "Z,"^(string_of_int n)
  | M(n) -> "M,"^(string_of_int n)
  | H(n) -> "H,"^(string_of_int n)
  | Perm -> "P"
  | Phase(f) -> "PHASE,"^(string_of_float f)
  | U(d,n,f) ->
    (let s = match d with
    | XHat -> "X"
    | YHat -> "Y"
    | ZHat -> "Z" in
    "U,"^s^","^(string_of_int n)^","^(string_of_float f))
  | Product(l) ->
    (let rec helper l =
      match l with
      | h::t -> (gate_to_string h)^";"^(helper t)
      | [] -> "" in
    "PRODUCT("^(helper l)^")")
  | Cont(l,sub_g) ->
    (let rec helper l =
      match l with
      | n::t -> (string_of_int n)^";"^(helper t)
      | [] -> "" in
    "CONT(["^(helper l)^"],("^(gate_to_string sub_g)^"))")
  | Uf(_) -> failwith "Can't print Uf"

let rec string_to_gate s =
  let s_to_gate_exn = Failure "Invalid string in string_to_gate" in
  let len = String.length s in
  let headers = ["I";"X";"Y";"Z";"H";"M";"P";"U";"PHASE"] in
  if(len >= 7 && (String.sub s 0 7) = "PRODUCT") then
    let lg = String.sub s 8 (String.length s-9) in
    (* finds the location of the next semi colon in s *)
    let rec next_semi idx count s =
      if(s="") then
        raise s_to_gate_exn
      else
        let head = String.sub s 0 1 in
        let tail = String.sub s 1 (String.length s-1) in
        if(head="(") then
          next_semi (idx+1) (count+1) tail
        else if(head=")") then
          next_semi (idx+1) (count-1) tail
        else if(head=";" && count=0) then
          idx
        else
          next_semi (idx+1) count tail in
    (* [helper s] truncates a string s to the next valid semicolon, and converts
     * that string to a gate format. *)
    let rec helper s =
      if(s="") then
        []
      else
        let p = next_semi 0 0 s in
        let g = String.sub s 0 p in
        let t = String.sub s (p+1) (String.length s-p-1) in
        let gate = string_to_gate g in
        gate::(helper t) in
    product(helper lg)
  else if(len >= 4 && (String.sub s 0 4) = "CONT") then
    let index = String.index s ']' in
    (* [make_list s] converts a string s to a list of integers *)
    let rec make_list s =
      if(s = "") then
        []
      else
        let next_semi_pos = String.index s ';' in
        let h = int_of_string ( String.sub s 0 next_semi_pos ) in
        h :: make_list (String.sub s (next_semi_pos+1)
          (String.length s-next_semi_pos-1)) in
    let l = make_list (String.sub s 6 (index-6)) in
    let gate_s = String.sub s (index+3) (String.length s - (index+3)) in
    (* [find_end s idx count] finds the end of a gate_type string *)
    let rec find_end s idx count =
      if(String.length s = 0) then raise s_to_gate_exn
      else
        let head = String.sub s 0 1 in
        let tail = String.sub s 1 ((String.length s)-1) in
        if(head="(") then find_end tail (idx+1) (count+1)
        else if(head=")") then
          if(count = 0) then
            idx
          else
            find_end tail (idx+1) (count-1)
        else
          find_end tail (idx+1) count in
    let index_paren = find_end gate_s 0 0 in
    let g = string_to_gate (String.sub gate_s 0 index_paren ) in
    control l g
  else if(len >= 5 && List.mem (String.sub s 0 5) headers ) then
    gate_of s
  else if(len >= 4 && List.mem (String.sub s 0 4) headers ) then
    gate_of s
  else if(len >= 1 && List.mem (String.sub s 0 1) headers ) then
    gate_of s
  else
    raise s_to_gate_exn
