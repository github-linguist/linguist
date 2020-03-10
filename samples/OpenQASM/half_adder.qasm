// A half adder circuit.

OPENQASM 2.0;
include "qelib1.inc";

qreg q[4];
creg c[2];

x q[0]; // Remove to keep the first input as 0.
x q[1]; // Remove to keep the first input as 0.
cx q[0], q[2];
cx q[1], q[2];
ccx q[0], q[1], q[3];
measure q[2] -> c[0];
measure q[3] -> c[1];