===============================================================================
=================[ QOCS - Quantum OCaml Circuit Simulator ]====================
===============================================================================

---[ RUNNING ]---
To run QOC, simply unzip the source files, and type "make" in the terminal.
To clean the compiled files, simply run "make clean" in the terminal.

---[ EXAMPLES of simulator ]---
Here are some examples of gates that one could make using QOCS. Run the
following commands after running make to see the results!

1. Hadamards
s                           - Enter simulator
c                           - Enter circuit building mode 
H 0 H 1 H 2                 - Make the hadamard gates by listing each H
save                        - Save the gate...
hadamards                   - as "hadamards"
q                           - Quit circuit building mode
a                           - Enter apply mode
hadamards [1,0,0,0,0,0,0,0] - Apply the hadamard state to |000>, and see
                              results.
After that, you should see a normalized state with all of the qubits 
with equal amplitudes!
q                           - Quit apply mode                           
q                           - Quit simulator
q                           - Quit QOCS

2. Toffoli using U and CNOT
s                           - Enter simulator
c                           - Enter circuit building mode 
u y 0 pi/4 cnot 1 0 
u y 0 pi/4 cnot 2 0 
u y 0 -pi/4 cnot 1 0 
u y 0 -pi/4 (one line)      -  Make the tof gate          
save                        - Save the gate...
tof                         - as "tof"
q                           - Quit circuit building mode
a                           - Enter apply mode
tof [0,0,0,0,0,0,1,0]       - Apply the tof gate to |000>, and see results.
q                           - Quit apply mode                           
q                           - Quit simulator
q                           - Quit QOCS

3. GHZ state
s                           - Enter simulator
c                           - Enter circuit building mode 
h 1 cnot 1 0                - To make the gate for GHZ state
save                        - Save the gate...
ghz                         - as "ghz"
q                           - Quit circuit building mode
a                           - Enter apply mode
ghz [1,0,0,0]               - Apply the tof gate to |00>, and see results.
q                           - Quit apply mode                           
q                           - Quit simulator
q                           - Quit QOCS

---[ Factoring ]---
You can also enter factoring mode. Factor allows you factor a number of type
x = p*q where p and q are prime numbers, such as x = 15.  Factoring mode uses
quantum computers and Quantum Fourier Transform the factor the number, instead
of calculating it classically.
