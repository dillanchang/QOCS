
type dlist = int list

(* [int_exp a b] produces integer exponentiation a^b *)
val int_exp : int -> int -> int

(* [int_exp_mod a b m] produces integer exponentiation a^b mod m. *)
val int_exp_mod : int -> int -> int -> int

(* [digit_list n] produces a list of digits of [n].
 * Example: [digit_list 123] evaluates to [1;2;3] *)
val digit_list : int -> dlist

(* [base_10_rep b dlst] converts a number in base [b] represented by
 * the digit list [dlst] to its base 10 representation *)
val base_10_rep : int -> dlist -> int

(* [base_convert bold bnew dlst] converts a number in base [bold]
 * represented by the digit list [dlst] to its base [bnew] repsentation
 * as a dlst *)
val base_convert : int -> int -> dlist -> dlist

(* [dlst_to_string dlst] converts a digit list to a string.
 * Example: [dlst_to_string [1;2;3]] evaluates to "123" *)
val dlst_to_string : dlist -> string

(* [pad_zeros n dlst] adds zeros to the head of [dlst] until
 * it is of length [n] *)
val pad_zeros : int -> dlist -> dlist

(* [add_mod_2 dlst1 dlst2] performs bitwise xor operation on [dlst1]
 * and [dlst2].
 * Example: [add_mod_2 [1;0;0] [1;1;1]] evaluates to [0;1;1] *)
val add_mod_2 : dlist -> dlist -> dlist

(* [split n dlst] splits [dlst] into two parts which are the elements
 * of the output pair. [dlst] is split such that the first element of the
 * pair has length [n] *)
val split : int -> dlist -> dlist * dlist
