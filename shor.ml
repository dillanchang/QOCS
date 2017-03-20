open Gate
open State
open Arithmetic

let pi = 4.0 *. atan 1.0

(* Pre: [i] >= 0
 * Post: largest [n] such that 2^n <= i *)
let rec pow_of_two i =
  if i/2 = 0 then 0
  else  1 + pow_of_two (i/2)

(* Returns the absolute value of [fl] *)
let abs_fl fl = if fl < 0. then -. fl else fl

(* Creates a list [i;i+1;...;j-1;j]
*  Implementation from CS 3110 Fall 2016 Recitation 3
 * http://www.cs.cornell.edu/courses/cs3110/2016fa/l/03-lists/rec.html *)
let (--) i j =
  let rec from i j l =
    if i>j then l
    else from i (j-1) (j::l)
  in from i j []

(* Makes a list of length [n] containing [x] in each entry *)
let rec make_list n x =
  let rec helper acc n = if n = 0 then acc else helper (x::acc) (n-1) in
  helper [] n

module Fraction = struct
  type t = int * int

  let rep_ok (a,b) = if b <> 0 then (a,b) else failwith "Div by 0"

  let rec gcd a b = if b = 0 then a else gcd b (a mod b)

  let inverse f = let (a,b) = rep_ok f in rep_ok (b,a)

  let reduce f =
    let (a,b) = rep_ok f in
    let c = gcd a b in
    rep_ok (a/c,b/c)

  let subtract f1 f2 =
    let (a,b) = rep_ok f1 in
    let (c,d) = rep_ok f2 in
    (a*d - b*c, b*d) |> reduce |> rep_ok

  let add f1 f2 =
    let (a,b) = rep_ok f1 in
    let (c,d) = rep_ok f2 in
    (a*d + b*c, b*d) |> reduce |> rep_ok

  (* Produces the continued fraction representation of the fraction [(a,b)]
     Ex: [cont_frac (3,2)] evaluates to (1,[2]) *)
  let cont_frac (a,b) =
    let rec helper acc (a,b) =
      let i = a / b in
      let (f1,f2) =  subtract (a,b) (i,1) in
      if f1 = 0 then i::acc else helper (i::acc) (inverse (f1,f2))
    in
    match helper [] (a,b) |> List.rev with
    | [] -> (0,[])
    | h::t -> (h,t)

  (* Given a continued fraction representation, returns the [n]th partial sum
   * If [n] is greater than the numer of digits in the continued fraction
   * representation, then [partial_sum] will return the full rational
   * representation of [(i,clst)] *)
  let partial_sum n (i,clst) =
    let rec helper acc k =
      function
      | [] -> reduce acc
      | h::t -> if k = 0 then reduce acc
                else add acc (inverse (helper (h,1) (k-1) t))
    in
    if n = 0 then (i,1) else helper (i,1) n clst

  (* Returns a list of all the partial sums of the fraction f
   * such that the [i]th entry represents the [i]th partial sum *)
  let list_of_partials f =
    let (i,clst) = cont_frac f in
    let len = List.length clst in
    let rec helper acc k =
      if k = 0 then (partial_sum k (i,clst)) :: acc
      else helper ((partial_sum k (i,clst)) :: acc) (k-1)
    in
    helper [] len

end

(* Returns the max element of the list.
 * NOTE: This function compares using [min_int]. *)
let max_of_list lst =
  let rec helper acc = function
    | [] -> acc
    | h::t -> helper (max h acc) t
  in
  if lst = [] then failwith "Max of empty list" else helper min_int lst

(* Pre: [State.coeff [state]] has only one non-zero entry
 * Post; returns the base 10 index of that entry *)
let index_of_nonzero_state state =
  let rec helper index coefs =
    match coefs with
    | [] -> failwith "No y returned!"
    | h::t ->
    if abs_fl ((Complex.norm2 h) -. 1.) < (10.** -. 6.) then index
    else helper (index+1) t
  in
  helper 0 (coeff state)

(* [is_prime n] evaluates to [true] if [n] is prime and [false] otherwise. *)
let is_prime n =
  let n = abs n in
  let rec not_divisible acc d =
    if d > n/2 then acc
    else not_divisible (acc && n mod d <> 0) (d+1) in
  not_divisible true 2

(* Returns a controlled phase gate V_k controlled on [ctr] with
 * target bit [target] *)
let ctr_phase ctr target k =
  if ctr = target then failwith "Controlling phase using target!"
  else
    gate_of ("U,Z,"
             ^string_of_int target^","
             ^string_of_float (pi /. (2. ** (float_of_int k))))
    |> control [ctr]

(* Returns the row of gates corresponding to [target_bit] in the
 * quantum Fourier transform algorithm *)
let row_of_gates (num_of_qft_bits:int) (num_of_total_bits:int)
                 (target_bit:int) : Gate.t list=
  let ctr_phase_gates =
    List.map (fun k -> ctr_phase (target_bit - k) target_bit k)
             (1 -- (target_bit - (num_of_total_bits - num_of_qft_bits)))
  in
  [gate_of ("H,"^string_of_int target_bit)]@(ctr_phase_gates)

(* Creates a quantum Fourier transform gate *)
let qft_gate (num_of_qft_bits:int) (num_of_total_bits:int): Gate.t =
  let rec append_rows n =
    if n = 1 then [gate_of ("H,"
                           ^string_of_int (num_of_total_bits - num_of_qft_bits))]
    else (append_rows (n-1))@row_of_gates
                               num_of_qft_bits
                               num_of_total_bits
                              (num_of_total_bits - 1 - num_of_qft_bits + n)
  in
  product (append_rows num_of_qft_bits)

(* Pre: |y-(2^n)/r| < 1/2 and 2^n > pq^2 > r^2
 * Follows the procedure in Appendix K of Quantum Computer Science by
 * David Mermin*)
let post_process y n a pq =
  let x = (y,int_exp 2 n) in
  let x_partial_sums = Fraction.list_of_partials x in
  let r0 =
    (List.filter (fun (_,d) -> 0 < d && d < pq) x_partial_sums)
    |> List.map (fun (a,b) -> b) |> max_of_list in
  if y = 0 then (false, -1)
  else if int_exp_mod a r0 pq = 1 then (true, r0)
  else
    let tries = pq/r0 in
    let rlst = (List.map (fun x -> r0*x) (1 -- tries))
               |> List.filter (fun x -> int_exp_mod a x pq = 1) in
    match rlst with
    | [] -> (false,-1)
    | h::t -> (true,h) (* Pick smallest period r *)

(* Finds the period of the function f(x) = a^x mod pq *)
let period_of a pq =
  let n =
    let n0 = (pow_of_two pq) + 1 in
    3 * n0 in
  let outbits = 0 -- (n/3 - 1) in
  let inbits = (n/3) -- (n - 1) in
  let num_in = 2*n/3 in
  let inputhads =
    List.map (fun x -> gate_of ("H,"^string_of_int x)) inbits
    |> product in
  let uf = gate_of ("Ufexp,"^(string_of_int a)^","^string_of_int (num_in)) in
  let measr_out =
    List.map (fun x -> gate_of ("M,"^string_of_int x)) outbits
    |> product in
  let qft = qft_gate (num_in) n in
  let measr_in =
    List.map (fun x -> gate_of ("M,"^string_of_int x)) inbits
    |> product in
  let init_st =
    (Complex.one :: (make_list ((int_exp 2 n)-1) Complex.zero)) |> make in
  let rec get_r (found,r) =
    let fin_st =
      init_st
      |> apply inputhads
      |> apply uf
      |> apply measr_out
      |> apply qft
      |> apply measr_in in
    let y_dlst =
      fin_st
      |> index_of_nonzero_state
      |> digit_list
      |> base_convert 10 2
      |> pad_zeros n
      |> split num_in
      |> fst in
    let y_in_bits_str = dlst_to_string y_dlst in
    let y =
      y_dlst |> base_10_rep 2 in
    (if r <> -1 then
      (print_endline ("Output of quantum circuit: input bit is in state |"
                    ^(y_in_bits_str)
                    ^">"))
     else ();
    if found = true then r else get_r (post_process y num_in a pq))
  in
  get_r (false, -1)


let factor i =
  let rec generate_a a =
    if Fraction.gcd i a = 1 && a <> 1 then
      (print_endline ("\"Good\" a found : a = "^string_of_int a);
       a)
    else
      (print_endline "a is not coprime with pq. Generating new a.";
       generate_a ((Random.int (i-1)) + 1))
  in
  let rec get_a_r (a,r) =
    let new_a = print_endline "Generating a in f(x)=a^x for period finding...";
                generate_a a in
    let new_r = print_endline ("Finding period of f(x)="
                              ^(string_of_int a)
                              ^"^x");
                period_of new_a i in
    if new_r mod 2 = 0 then
      (print_endline ("Found r that is even : r = "^string_of_int new_r);
       (new_a,new_r))
    else
      (print_endline ("Found r that is not even. Finding new r...");
       get_a_r (new_a,new_r))
  in
  let rec get_p_q (a,r) x pq =
    let (new_a,new_r) = get_a_r (a,r) in
    let new_x =
      (print_endline ("Found x = a^(r/2): x = "^string_of_int x);
       int_exp_mod new_a (new_r/2) pq) in
    if (new_x +1) mod pq <> 0 then
      let prelim_p = Fraction.gcd pq (new_x+1) in
      let p = if prelim_p <> 1 then prelim_p else Fraction.gcd pq (new_x-1) in
      let q = pq /p in
      if p = pq || q = pq then get_p_q (new_a,new_r) new_x pq
      else (print_endline "Success!";(p,q))
    else
      (print_endline "x +1 mod pq <> 0. Restarting...";
       get_p_q (new_a,new_r) new_x pq)
  in
  if i <= 0 then failwith "You can factor positive integers only."
  else if is_prime i = true then
    print_endline "This number is prime! Cannot be factored."
  else if i > 22 then
    let (p,q) = get_p_q (2,2) (i-1) i in
    ((print_endline ("Factoring this number requires many qubits."
                   ^"This might take awhile...\n"));
    (Printf.printf "\nThe prime factors of %n are %n and %n.\n" i p q))
  else
    let (p,q) = get_p_q (2,2) (i-1) i in
    (Printf.printf "\nThe prime factors of %n are %n and %n.\n" i p q)