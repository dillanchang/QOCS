open State 
open Gate
open Shor

exception IncorrectGate
exception IncorrectState

module AT = ANSITerminal

let bg = AT.on_black

let pi = 4.0 *. atan 1.0

let files : (string * Gate.t) list ref = ref []

(**************************** General Helpers *****************************)
  
(* precondition : None
 * postcondition : returns the current size of the terminal window *)  
let get_terminal_size () : (int * int) = AT.size ()

(* precondition : None
 * postcondition : prints a line of = signs (length of line = current
 * width of terminal window) *)
let linegap () : unit = 
  let (w, l) = get_terminal_size () in 
  for i = 1 to w 
  do 
    AT.print_string [bg; AT.cyan] "="
  done;
  print_endline ""

(* precondition : None
 * postcondition : prints "QOCS # " before user input *)
let input_line () : unit = 
  AT.print_string [bg; AT.magenta] "QOCS ";
  AT.print_string [bg; AT.green] "# "

(* precondition : None
 * postcondition : prints colorful image of QOCS *)
let print_finish () : unit = 
  linegap ();
  AT.print_string [bg;AT.red] "  __   ";
  AT.print_string [bg;AT.white] "  __   ";
  AT.print_string [bg;AT.blue] " ____ ";
  AT.print_string [bg;AT.yellow] " _____\n";
  AT.print_string [bg;AT.red] " /  \\  ";
  AT.print_string [bg;AT.white] " /  \\ ";
  AT.print_string [bg;AT.blue] " |    ";
  AT.print_string [bg;AT.yellow] " |\n";
  AT.print_string [bg;AT.red] "|    | ";
  AT.print_string [bg;AT.white] "|    | ";
  AT.print_string [bg;AT.blue] "|    ";
  AT.print_string [bg;AT.yellow] " |_____\n";
  AT.print_string [bg;AT.red] " \\_\\/  ";
  AT.print_string [bg;AT.white] "|    | ";
  AT.print_string [bg;AT.blue] "|    ";
  AT.print_string [bg;AT.yellow] "       |\n";
  AT.print_string [bg;AT.red] "    \\  ";
  AT.print_string [bg;AT.white] " \\__/ ";
  AT.print_string [bg;AT.blue] " |____ ";
  AT.print_string [bg;AT.yellow] " _____| \n";
  linegap ()

(* precondition : None
 * postcondition : prints all names (first component of tuples) 
 * in [files] *)
let rec print_files lst : unit = 
  match lst with
  | [] -> ()
  | (n, g) :: t -> print_endline n; print_files t 

(* precondition : [ch] must be a valid input_channel 
 * postcondition : reads gates from ch and adds them to [files] if they
 * do not already exist in [files] *)
let rec read_gates ch : unit = 
  try
    let name = Pervasives.input_line ch in 
    let g = string_to_gate (Pervasives.input_line ch) in 
    (if List.mem (name,g) !files 
    then ()
    else files := ((name,g) :: !files));
    read_gates ch 
  with
  | End_of_file -> close_in ch 

(* precondition : [ch] must be a valid output_channel
 * postcondition : saves gates in [files] to [ch] *)
let rec save_gates_to_file ch : unit = 
  for i = 1 to (List.length !files)
  do 
    let (name, g) = List.nth !files (i-1) in 
    output_string ch name;
    output_string ch "\n";
    output_string ch (gate_to_string g);
    output_string ch "\n"
  done;
  print_endline "Gates have been saved to an external file for future use!";
  close_out ch 

(**************************** State Helpers ******************************)

(* precondition : None
 * postcondition : prints format for entering states *)
let get_state_format () : unit = 
  print_endline ("Steps to apply gates to a state: \n" ^
  "1. To apply gates to a state, you must have a saved circuit.\n" ^ 
  "2. If you do not have a saved circuit, exit this mode and build a new " ^ 
  "circuit.\n" ^
  "3. The state is represnted by each of its 2^n coefficients.\n" ^ 
  "The coefficients need not be normalised.\n" ^ 
  "4. You can either list out all 2^n components or represent a state \n" ^ 
  "by a tensor product of states.\n" ^ 
  "5. To perform the action of a circuit on a state, type the name of \n" ^
  "the circuit followed by the state in one of the above forms.\n" ^
  "Here are 2 ways in which to apply state \"G\" to the 3-qubit state: \n" ^
  "|000> + i|010> + 3|001> + (1+i)|011> -\n" ^
  "a) Normal: G [1,i,3,1+i,0,0,0,0]\n" ^
  "b) Tensor product: G [1,0] [1,i,3,1+i]");
  linegap ()

(* precondition : [str] must be of the form a + bi where a,b are floats
 * postcondition : returns a tuple of the real and imaginary components
 * of [str] *)
let extract_components str : (float * float) = 
  let comp_lst = Str.split (Str.regexp "[ +]+") (String.trim str) in 
  let rec extract_helper (ax, ay) lst = 
    match lst with
    | [] -> (ax, ay)  
    | h :: t -> 
      if h = "-" 
      then 
        let tail = 
          (match t with
          | [] -> []
          | h :: t -> ("-" ^ h) :: t) in 
        extract_helper (ax, ay) tail
      else 
        let (x, y) = 
          if String.contains h 'I' 
          then 
            let im = h 
              |> String.map (fun x -> if x = 'I' then ' ' else x) 
              |> String.trim in 
            if im = "" then (0.0, 1.0) else (0.0, float_of_string im)
          else 
            (float_of_string h, 0.0) in 
        extract_helper (x +. ax, y +. ay) t in 
  extract_helper (0.0, 0.0) comp_lst

(* precondition : [lst] is a string list with each element being a string
 * of the form a + bi.
 * postcondition : returns a list of complex numbers which represent the 
 * coefficients of the state *)
let get_state lst : Complex.t list = 
  let rec get_state_helper acc l = 
    match l with
    | [] -> List.rev acc
    | h :: t -> 
        let (x, y) = extract_components h in 
        let comp_h = 
          if (x, y) = (0.0, 0.0) then Complex.zero 
          else Complex.polar ((x ** 2.0 +. y ** 2.0) ** 0.5) (atan (y /. x)) in 
        get_state_helper (comp_h :: acc) t in 
  get_state_helper [] lst 

(* precondition : [str] must one or more lists separated by spaces with 
 * each component of the list being of the form a + bi where a,b are floats
 * postcondition : prints a list of states which are represented by the 
 * coefficients in [str] *)
let get_states_list str : State.st list = 
  let rec get_states_list_helper acc s = 
    try
      let lbrac = String.index s '[' in 
      let rbrac = String.index s ']' in 
      let s_coeff = String.sub s lbrac (rbrac-lbrac) in 
      let coeff_lst = Str.split (Str.regexp "[],[]") (String.trim s_coeff) in
      let curr_state = coeff_lst |> get_state |> make in 
      let remain = String.sub s (rbrac + 1) (String.length s - rbrac - 1) in 
      get_states_list_helper (curr_state :: acc) remain
    with
    | _ -> List.rev acc in 
  get_states_list_helper [] str 
    
(**************************** Gate Helpers *******************************)

(* precondition : None
 * postcondition : prints format for entering gates *)
let get_gate_format () : unit = 
  print_endline 
    ("Format for entering gates: \n" ^
    "\"X n\": for an X gate applied to the n-th qubit \n" ^
    "\"Y n\": for a Y gate applied to the n-th qubit \n" ^
    "\"Z n\": for a Z gate applied to the n-th qubit \n" ^
    "\"I \": for the identity gate \n" ^
    "\"H n\": for a hadamrd applied to the n-th qubit \n" ^
    "\"M n\": for a measurement gate applied to the n-th qubit \n" ^
    "\"PHASE a\": for a phase gate to apply a phase of a radians \n" ^
    "\"CNOT n m\": for a controlled-NOT gate with n: control bit " ^ 
    "and m: target bit \n" ^
    "\"U g n a\": for a rotation gate applied to the n-th qubit with a \n" ^ 
    "rotation of a: radians about the axis of g: gate. g must be " ^
    "X or Y or Z \n" ^ 
    "\"TOF l m n\": for an Toffoli gate with l,m: control bit and " ^
    "n: target bit \n" ^
    "\"gs\": for a previously saved gate gs \n" ^
    "\"C n1 n2 ... nm g\": where n1 n2 ... nm are the control bits and g \n" ^
    "is the gate to control.");
  AT.print_string [bg; AT.red] "Warning: ";
  print_endline "\'g\' should NOT act on any of the given control bits.";
  linegap ();
  print_endline 
    ("Some additional notes: \n" ^ 
    "1. If you enter multiple gates, then separate each gate by a space. \n" ^ 
    "2. The sequence of gates will be applied first to last i.e. \n" ^
    "the first gate in sequence will be applied first and the last will \n" ^
    "be applied last. \n" ^
    "3. Once you have completed this you can choose to save this sequence\n" ^ 
    "by entering \"SAVE\" followed by hitting the enter key.\n" ^
    "4. You can then provide a name to this circuit which is a tensor \n" ^
    "product of the gates listed above.\n ");
  AT.print_string [bg; AT.red] "Warning: ";
  print_endline 
    ("While saving, make sure to not repeat the name of a fundamental \n" ^ 
    "gate or a previously saved gate.");
  linegap ()

(* precondition : [h1] must be "PHASE". [h2] must either be a float or
 * a fraction of "pi".
 * postcondition : returns a PHASE gate performing a rotation of [h2]  *)
let check_phase h1 h2 : Gate.t = 
  try
    let lst = Str.split (Str.regexp "['/']+") h2 in 
    (match lst with
    | h :: [] -> gate_of (h1 ^ "," ^ (string_of_float (float_of_string h)))
    | h :: t :: [] -> 
      let num = 
        if h = "-PI"
        then string_of_float ((-. pi) /. (float_of_string t))
        else if h = "PI" 
        then string_of_float (pi /. (float_of_string t))
        else 
          let nr = float_of_string h in 
          let dr = float_of_string t in 
          string_of_float (nr /. dr) in 
      gate_of (h1 ^ "," ^ num) 
    | _ -> raise IncorrectGate)
  with
  | _ -> raise IncorrectGate

(* precondition : [h2] must be "X", "Y" or "Z". [h3] must be an int. [h4] must 
 * be either a float or a fraction of "pi".
 * postcondition : returns a "U" gate acting on bit [h3] performing a rotation
 * of [h4] about axis [h2] *)
let check_U h2 h3 h4 : Gate.t = 
  try
    let axis = 
      match h2 with
      | "X" | "Y" | "Z" -> h2
      | _ -> raise IncorrectGate in 
    let bit = string_of_int (int_of_string h3) in 
    let angle = 
      let lst = Str.split (Str.regexp "['/']+") h4 in 
      (match lst with
      | h :: [] -> (string_of_float (float_of_string h))
      | h :: t :: [] ->
          if h = "-PI"
          then string_of_float ((-. pi) /. (float_of_string t))
          else if h = "PI" 
          then string_of_float (pi /. (float_of_string t))
          else 
            let nr = float_of_string h in 
            let dr = float_of_string t in 
            string_of_float (nr /. dr)
      | _ -> raise IncorrectGate) in 
    gate_of ("U," ^ axis ^ "," ^ bit ^ "," ^ angle) 
  with
  | _ -> print_endline "In check_U"; raise IncorrectGate

(* precondition : [t] is a non empty string list
 * postcondition : returns a tuple of a "CONTROL" gate with its control bits 
 * and target bits and gates in [t], and the rest of the list [t]. *)
let rec check_control t acc : (Gate.t * string list) = 
  match t with
  | [] -> raise IncorrectGate
  | h1 :: t1 -> 
      (try
        let new_acc = (int_of_string h1) :: acc in 
        check_control t1 new_acc
      with _ -> let (g, tn) = (next_gate t) in ((control acc g), tn))

(* precondition : [t] is a non empty string list
 * postcondition : returns the next gate whose components are in order in 
 * list [t] along with the rest of the list [t] *)
and next_gate t : (Gate.t * string list) = 
  match t with
  | [] -> raise IncorrectGate
  | h1 :: t1 -> 
    (match h1 with
    | "I" | "" -> (gate_of "I", t1)
    | "X" | "Y" | "Z" | "H" | "M" -> 
      (match t1 with
      | h2 :: t2 -> 
        let g = gate_of (h1 ^ "," ^ (string_of_int (int_of_string h2))) in 
        (g, t2) 
      | _ -> raise IncorrectGate)
    | "PHASE" ->
      (match t1 with
      | h2 :: t2 -> 
        let g = check_phase h1 h2 in 
        (g, t2) 
      | _ -> raise IncorrectGate)
    | "CNOT" -> 
      (match t1 with
      | h2 :: h3 :: t3 -> 
        let g = gate_of (h1 ^ "," ^ 
          (string_of_int (int_of_string h2)) ^ "," ^ 
          (string_of_int (int_of_string h3))) in 
        (g, t3) 
      | _ -> raise IncorrectGate)
    | "TOF" -> 
      (match t1 with
      | h2 :: h3 :: h4 :: t4 -> 
        let g = gate_of (h1 ^ "," ^ 
          (string_of_int (int_of_string h2)) ^ "," ^ 
          (string_of_int (int_of_string h3)) ^ "," ^ 
          (string_of_int (int_of_string h4))) in 
        (g, t4)  
      | _ -> raise IncorrectGate)
    | "U" -> 
      (match t1 with
      | h2 :: h3 :: h4 :: t4 -> (check_U h2 h3 h4, t4) 
      | _ -> raise IncorrectGate)
    | "C" -> 
      (match t1 with
      | h2 :: t2 -> check_control t2 [(int_of_string h2)] 
      | _ -> raise IncorrectGate)
    | x when List.mem_assoc x !files ->
        (List.assoc x !files, t1)
    | _ -> raise IncorrectGate)

(* precondition : [lst] is a string list
 * postcondition : returns a list of gates by converting them from string
 * form in [lst] to type Gate.t. Returns None if [lst] represents incorrectly 
 * typed gates. *)
let rec get_gate_lst acc lst : Gate.t list option = 
  try
    (match lst with
    | []     -> Some (List.rev acc)
    | x -> let (g, t) = next_gate x in get_gate_lst (g :: acc) t)
  with
  | _ -> None

(********************************* REPL's ********************************)

(* precondition : [cm] is a string
 * postcondition : runs the outermost REPL - to either factor numbers,
 * start building quantum circuits or to quit *)
let rec repl cm : unit = 
  let command = ref cm in 
  (if cm = ""
  then 
    (print_endline 
      ("You can choose one of 3 options: \n" ^
      "a) Type S to start the quantum circuit simulator \n" ^ 
      "b) Type F to factor a natural number \n" ^
      "c) Type Q to quit \n");
    linegap ();  
    input_line ();
    command := read_line () |> String.uppercase |> String.trim;
    linegap ())
  else ()); 
  match !command with
  | "Q" -> 
      print_endline 
        ("Do you wish to save the circuits built and saved above?\n" ^ 
        "Y: yes\n" ^ "N: no");
      linegap ();
      input_line ();
      let answer = read_line () |> String.uppercase |> String.trim in 
      linegap ();
      (if answer = "Y" || answer = "YES" 
      then 
        (let ch = open_out "saved_gates.txt" in save_gates_to_file ch;
        linegap ();
        print_endline 
          ("Hope you enjoyed our QOCS. It will be bigger and " ^
          "better in our next update!");
        print_finish ())
      else if answer = "N" || answer = "NO" 
      then 
        (print_endline 
          ("Hope you enjoyed our QOCS. It will be bigger and " ^
          "better in our next update!");
        print_finish ())
      else 
        (print_endline "Incorrect input. Try again"; 
        linegap (); repl "Q"));
  | "S" -> repl_qcs ()
  | "F" -> 
      print_endline "Enter your natural number to factor: "; 
      AT.print_string [bg;AT.magenta] "NOTE: ";
      print_endline 
        ("The integer must be of the form pq where p and q are distinct\n" ^
        "primes, to see the beauty of the quantum factoring algorithm");
      linegap ();
      input_line ();
      (try
        let i = int_of_string (read_line ()) in 
        linegap (); Shor.factor i;
        linegap (); repl ""
      with
      | _ -> 
        linegap ();
        print_endline "Not a natural number. Try again.";
        linegap (); repl "F");
  | _   -> 
      print_endline "Incorrect input. Try again."; 
      linegap (); repl ""

(* precondition : None
 * postcondition : runs the REPL to build circuits, apply circuits to states
 * or to quit *)
and repl_qcs () : unit = 
  print_endline 
    ("Select one of the following: \n" ^
    "a) Type C to build a circuit by listing out gates \n" ^
    "b) Type A to apply a gate to a state \n" ^
    "c) Type Q to exit this mode");
  linegap ();
  input_line ();
  let com = read_line () |> String.uppercase |> String.trim in
  linegap ();
  match com with
  | "C" -> get_gate_format (); repl_circuit (gate_of "I")
  | "A" -> get_state_format (); repl_apply ()
  | "Q" -> repl ""
  | _   -> print_endline "Incorrect input. Try again."; linegap (); repl_qcs ()

(* precondition : [curr] is a Gate.t
 * postcondition : runs the REPL to build or save circuits and to view format
 * of entering gates or quit *)
and repl_circuit curr : unit = 
  print_endline 
    ("Do one of the following: \n" ^ 
    "a) Enter a gate or a sequence of gates separated by spaces.\n" ^ 
    "b) Save the circuit built by entering \"SAVE\". \n" ^
    "c) Enter Q to quit this mode. \n" ^
    "d) Enter F to view format for inputting gates.");
  linegap ();
  input_line ();
  let com = read_line () |> String.uppercase |> String.trim in 
  linegap ();
  match com with
  | "SAVE" -> 
      if curr = gate_of "I"
      then 
        (print_endline 
          ("You need to build a circuit to save it. Enter a circuit \n" ^
          "before trying to save."); 
        repl_circuit curr)
      else 
        print_string "Enter name of circuit you wish to save: ";
        let name = read_line () |> String.uppercase |> String.trim in 
        linegap ();
        (if List.mem_assoc name !files 
        then 
          print_endline "A circuit with that name already exists! Try again"
        else 
          (files := ((name, curr) :: !files);
          print_endline "Your circuit above has been successfully saved!"));
        linegap ();
        repl_circuit curr
  | "Q" | "QUIT" -> 
      print_endline "Hope you built your circuit to satisfaction!"; 
      linegap (); repl_qcs ()
  | "F" | "FORMAT" -> get_gate_format (); repl_circuit curr
  | _ -> combine_gates com curr

(* precondition : [str] represents a list of gates separated by spaces 
 * (according to format printed by [get_gate_format]. [curr] is a Gate.t)
 * postcondition : tensor products the list of gates represented by [str] and
 * runs [repl_circuit new_gate] where [new_gate] is the tensor product of the 
 * gates *)
and combine_gates str curr : unit = 
  let g_lst = Str.split (Str.regexp "[' ']+") str in 
  let new_gate = 
    (match get_gate_lst [] g_lst with
    | None -> 
        print_endline "A gate has been incorrectly typed. Try again."; curr
    | Some x -> 
        print_endline "Your sequence of gates has been registered.";
        product x) in 
  linegap ();
  repl_circuit new_gate

(* precondition : None
 * postcondition : runs the REPL to apply circuits to a gate, to view currently
 * available circuits or to quit *)
and repl_apply () : unit = 
  try
    print_endline 
      ("Enter a circuit name followed by a state, V to view the currently \n" ^ 
      "available circuits or Q to quit this mode:");
    linegap ();
    input_line ();
    let com = read_line () |> String.uppercase |> String.trim in 
    linegap ();
    if com = "Q" || com = "QUIT" 
    then repl_qcs ()
    else if com = "V" || com = "VIEW"
    then 
      (print_endline "Available circuits:"; 
      print_files !files; 
      linegap (); repl_apply ())
    else 
      (let brac1 = String.index com '[' in 
      let circuit = String.sub com 0 (brac1-1) in 
      let g = List.assoc circuit !files in 
      let remain = String.sub com brac1 (String.length com - brac1) in 
      let states_lst = get_states_list remain in 
      let init_state = tensor states_lst in 
      let result = val_string (apply g init_state) in 
      print_endline 
        ("The new state having passed: \n" ^ (val_string (init_state))
        ^ "\nthrough the circuit: \"" ^ circuit ^ "\" is \n" ^ result);
      linegap ();
      repl_apply ())
  with
  | Not_found -> 
    print_endline "A circuit containing that name does not exist. Try again";
    linegap (); repl_apply ()
  | _ -> 
    print_endline "Incorrectly application. Try again."; 
    linegap (); repl_apply ()

(**************************** Main Function *****************************)

let main () = 
  let (w,l) = get_terminal_size () in 
  for i = 1 to l 
  do 
    print_endline ""
  done;
  linegap ();
	print_endline ("Hello! Welcome the one and only Quantum Circuit OCaml "
  ^ "Simulator! (QOCS) \nNOTE: Maximise your terminal window size for optimal " 
  ^ "user experience.");
  let channel = open_in "saved_gates.txt" in 
  read_gates channel;
  linegap ();
  repl ""

let () = main ()
  
