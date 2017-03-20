	open Gate

	(* [factor i] takes in an int [i] and factors and prints its prime factors.
   * This function simulates factoring using shor's algorithm as well as
   * efficient classical algorithms and prints statistics comparing efficiency
   * of the 2 techniques
   *
   * Pre: i is a product of primes p q and i > 0 *)
	val factor : int -> unit


  (* [qft_gate in total] returns a quantum fourier transform circuit as a
   * single gate with [in] number of qubits involved in the QFT out of
   * [total] number of qubits. The indices of the qubits involved in the QFT
   * is counted in decreasing order.
   * Example: [qft_gate 2 4] returns a QFT gate that performs the QFT on
   * qubit 2 and 3 (zero-indexed) and leaves qubit 0 and 1 alone. *)
  val qft_gate : int -> int -> Gate.t
