// test_qft_complete.qc
// Comprehensive test suite for QFT and dagger operations

from "quantumlib/qstd.qc" import QFT, Bell

let PI = 3.14159265

print("========================================")
print("   QUANTUM FOURIER TRANSFORM TESTS")
print("========================================")

// Test 1: QFT on |00> state
print("\n--- Test 1: QFT on |00> (ground state) ---")
quantum q1[2]
print("Initial state |00>:")
debug_state(q1)

QFT(q1)
print("After QFT (should be equal superposition):")
debug_state(q1)
// Expected: |00>, |01>, |10>, |11> all with amplitude 0.5

// Test 2: QFT on |01> state
print("\n--- Test 2: QFT on |01> ---")
quantum q2[2]
apply X(q2[0])
print("Initial state |01>:")
debug_state(q2)

QFT(q2)
print("After QFT:")
debug_state(q2)
// Expected: (1/2)(|00> - |01> + |10> - |11>)

// Test 3: QFT on |10> state
print("\n--- Test 3: QFT on |10> ---")
quantum q3[2]
apply X(q3[1])
print("Initial state |10>:")
debug_state(q3)

QFT(q3)
print("After QFT:")
debug_state(q3)
// Expected: (1/2)(|00> + i|01> + |10> + i|11>)

// Test 4: QFT on |11> state
print("\n--- Test 4: QFT on |11> ---")
quantum q4[2]
apply X(q4[0])
apply X(q4[1])
print("Initial state |11>:")
debug_state(q4)

QFT(q4)
print("After QFT:")
debug_state(q4)
// Expected: (1/2)(|00> - i|01> + |10> - i|11>)

print("\n========================================")
print("   INVERSE QFT (DAGGER) TESTS")
print("========================================")

// Test 5: QFT then inverse QFT should return to original state
print("\n--- Test 5: QFT Inversion on |01> ---")
quantum q5[2]
apply X(q5[0])
print("Initial state |01>:")
debug_state(q5)

QFT(q5)
print("After QFT:")
debug_state(q5)

dagger(QFT)(q5)
print("After inverse QFT (should be back to |01>):")
debug_state(q5)

// Test 6: QFT inversion on different state
print("\n--- Test 6: QFT Inversion on |11> ---")
quantum q6[2]
apply X(q6[0])
apply X(q6[1])
print("Initial state |11>:")
debug_state(q6)

QFT(q6)
print("After QFT:")
debug_state(q6)

dagger(QFT)(q6)
print("After inverse QFT (should be back to |11>):")
debug_state(q6)

print("\n========================================")
print("   BELL STATE TESTS")
print("========================================")

// Test 7: Bell state creation
print("\n--- Test 7: Creating Bell State ---")
quantum q7[2]
Bell(q7[0], q7[1])
print("Bell state (|00> + |11>)/sqrt(2):")
debug_state(q7)
// Expected: |00> and |11> with amplitude ~0.707

// Test 8: Dagger of Bell state
print("\n--- Test 8: Inverse Bell State ---")
quantum q8[2]
Bell(q8[0], q8[1])
print("After Bell:")
debug_state(q8)

dagger(Bell)(q8[0], q8[1])
print("After dagger(Bell) (should be back to |00>):")
debug_state(q8)

print("\n========================================")
print("   PARAMETERIZED GATE DAGGER TESTS")
print("========================================")

// Test 9: RX gate and its dagger
print("\n--- Test 9: RX(PI/2) and dagger ---")
quantum q9[1]
let HALF_PI = 1.57079633
apply RX(HALF_PI)(q9[0])
print("After RX(PI/2):")
debug_state(q9)

apply dagger(RX(HALF_PI))(q9[0])
print("After dagger(RX(PI/2)) (should be back to |0>):")
debug_state(q9)

// Test 10: Controlled parameterized gate dagger
print("\n--- Test 10: Controlled RY dagger ---")
quantum q10[2]
apply X(q10[0])
apply controlled(RY(HALF_PI))(q10[0], q10[1])
print("After controlled(RY(PI/2)):")
debug_state(q10)

apply dagger(controlled(RY(HALF_PI)))(q10[0], q10[1])
print("After dagger (should be back to |10>):")
debug_state(q10)

print("\n========================================")
print("   ALL TESTS COMPLETE!")
print("========================================")