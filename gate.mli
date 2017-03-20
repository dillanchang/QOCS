  open State

  (* the overall type of an n-qubit gate *)
  type t

  (* [gate_of s] takes in an input string which illustrates the type of a gate
   * and the bits that they act on, with each gate separated by spaces and outputs
   * the resultant gate. For example, the input can be "X1", and the resultant
   * output will be a gate described by the string.
   *
   * [gate_of] will only accept strings that denote "fundamental gates." The list
   * of fundamental gates are as follows: ("#" refers to an int)
   * - I
   * - X,#
   * - Y,#
   * - Z,#
   * - H,#
   * - M,# (Measurement).
   * - PHASE,[angle in radians]
   * - CNOT,#,#. The first # is the control, second # is the target.
   * - U,[X or Y or Z],#,[angle in radians].
   *   For example, "U,X,3,-1." is a rotation of bit 3 about x by -1 radians
   *   counterclockwise.
   * - TOF,#,#,#. The first and second # are the controls, the third # is the
   *   target.
   * - P (Reverses order of the bits)
   *)
  val gate_of : string -> t

  (* [control l s] returns a gate that is controlled by the numbers in l. Every
   * value of the bits in l must be on in order for the gate [s] to be active.
   * precondition: the numbers in [l] must not be any of the bits that are being
   * operated on in the [s] gate *)
  val control : int list -> t -> t

  (* [apply s g] takes in inputs state [s] and gate [g] and outputs the
   * resultant state of applying [g] to [s].
   * Post-condition: resulting state is normalized.
   *)
  val apply : t -> st -> st

  (* [product l] takes in a list of gates and outputs their tensor product.
   * In the producted gate, the first element gate is applied first,
   * and then the second, and so forth. *)
  val product : t list -> t

  (* [gate_to_string t] returns a string that allows the user to store a gate in 
   * string form. *)
  val gate_to_string : t -> string

  (* [string_to_gate s] returns the gate of string [s] that is returned after
   * calling gate_to_string on a gate *)
  val string_to_gate : string -> t
