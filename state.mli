  open Complex

  (* the overall type of the state of a certain number of qubits *)
	type st = Tree of st*st | Node of Complex.t

  (* [make lst n] takes as input a list of complex numbers and an 
   * integer n to produce a state of n qubits where elements of the list
   * serve as coefficients of states for their respective indices in the
   * list. For example, the list [1+i; 0; 5; 3i] will become 
   * the state (1/6)((1+i)|00> + 0|01> + 5|10> + 3i|11>). 1/6 is the 
   * normalization factor 
   * raises: Failure if the number of elements in [lst] is not 2^[n] *)
  val make : Complex.t list -> st

  (* [state_of n m] returns the [n]'th qubit, with [m] number of bits.
   * For example, [state_of 5 3] will return [0,0,0,0,0,1,0,0].
   * If n > 2^m, the top bits will be truncated. For example, 
   * [state_of 22 3] will return [0,0,0,0,0,0,1,0]. *)
  val state_of : int -> int -> st

  (* [empty n] returns an n bit qbit with all coefficients as 0 *)
  val empty : int -> st

  (* [n_bits s] returns the number of bits in a state. *)
  val n_bits : st -> int

  (* [zero] returns a 1 bit zero qbit *)
  val zero : st

  (* [one] returns a 1 bit one qbit *)
  val one : st

  (* [tensor sl] Returns the tensor product of all the states in sl. The first
   * elements of the list will become the significant bits in the new state. *)
  val tensor : st list -> st

  (* [normalize s] takes as input a state and outputs the same state
   * normalized based on coefficients of respective components of the state *)
  val normalize : st -> st

  (* [add s1 s2] adds [s1] and [s2] and returns the normalized state
   * precondition: s1 and s2 have the same number of bits.
   * WARNING: The output state is not normalized. *)
  val add : st -> st -> st

  (* [phase s ang] adds an overall phase ang to all elements in s *)
  val phase : float -> st -> st

 (* [coeff s] returns the coefficients of the state [s] as a list of Complex
   * numbers. *)
  val coeff : st -> Complex.t list

  (* [val_string s] takes as input a state and returns a formatted string of its
   * coefficients. *)
  val val_string : st -> string
