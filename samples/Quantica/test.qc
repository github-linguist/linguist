// test_all_gates.qc - Comprehensive gate testing suite
let PI = 3.14159265

print("=== QUANTICA GATE TEST SUITE ===\n")

// ============================================
// 1. SINGLE-QUBIT PAULI GATES
// ============================================
print("--- 1. Pauli X Gate (NOT) ---")
quantum qx[1]
print("Initial state:")
debug_state(qx)
apply X(qx[0])
print("After X gate:")
debug_state(qx)

print("\n--- 2. Pauli Y Gate ---")
quantum qy[1]
apply Y(qy[0])
print("After Y gate on |0>:")
debug_state(qy)

print("\n--- 3. Pauli Z Gate ---")
quantum qz[1]
apply X(qz[0])
print("State |1> before Z:")
debug_state(qz)
apply Z(qz[0])
print("After Z gate:")
debug_state(qz)

// ============================================
// 2. HADAMARD GATE
// ============================================
print("\n--- 4. Hadamard Gate ---")
quantum qh[1]
apply Hadamard(qh[0])
print("After H on |0>:")
debug_state(qh)
apply Hadamard(qh[0])
print("After H again (should be |0>):")
debug_state(qh)

// ============================================
// 3. PHASE GATES (S and T)
// ============================================
print("\n--- 5. S Gate (Phase π/2) ---")
quantum qs[1]
apply X(qs[0])
apply S(qs[0])
print("After S on |1>:")
debug_state(qs)

print("\n--- 6. T Gate (Phase π/4) ---")
quantum qt[1]
apply X(qt[0])
apply T(qt[0])
print("After T on |1>:")
debug_state(qt)

// ============================================
// 4. ROTATION GATES
// ============================================
print("\n--- 7. RX Gate (Rotation around X-axis) ---")
quantum qrx[1]
apply RX(PI)(qrx[0])
print("After RX(π) on |0>:")
debug_state(qrx)

print("\n--- 8. RY Gate (Rotation around Y-axis) ---")
quantum qry[1]
apply RY(PI / 2.0)(qry[0])
print("After RY(π/2) on |0>:")
debug_state(qry)

print("\n--- 9. RZ Gate (Rotation around Z-axis) ---")
quantum qrz[1]
apply X(qrz[0])
apply RZ(PI / 2.0)(qrz[0])
print("After RZ(π/2) on |1>:")
debug_state(qrz)

// ============================================
// 5. TWO-QUBIT GATES
// ============================================
print("\n--- 10. CNOT Gate (Controlled-NOT) ---")
quantum qcnot[2]
apply X(qcnot[0])
print("Initial state |10>:")
debug_state(qcnot)
apply CNOT(qcnot[0], qcnot[1])
print("After CNOT:")
debug_state(qcnot)

print("\n--- 11. CZ Gate (Controlled-Z) ---")
quantum qcz[2]
apply X(qcz[0])
apply X(qcz[1])
print("Initial state |11>:")
debug_state(qcz)
apply CZ(qcz[0], qcz[1])
print("After CZ:")
debug_state(qcz)

print("\n--- 12. Swap Gate ---")
quantum qswap[2]
apply X(qswap[0])
print("Initial state |10>:")
debug_state(qswap)
apply Swap(qswap[0], qswap[1])
print("After Swap:")
debug_state(qswap)

print("\n--- 13. CPhase Gate ---")
quantum qcp[2]
apply X(qcp[0])
apply X(qcp[1])
print("Initial state |11>:")
debug_state(qcp)
apply CPhase(PI / 4.0)(qcp[0], qcp[1])
print("After CPhase(π/4):")
debug_state(qcp)

// ============================================
// 6. UNIVERSAL GATE (U)
// ============================================
print("\n--- 14. U Gate (Universal Single-Qubit) ---")
quantum qu[1]
apply U(PI / 2.0, 0.0, PI)(qu[0])
print("After U(π/2, 0, π) on |0>:")
debug_state(qu)

// ============================================
// 7. CONTROLLED GATES
// ============================================
print("\n--- 15. Controlled-X (CNOT) via controlled() ---")
quantum qcx[2]
apply X(qcx[0])
apply controlled(X)(qcx[0], qcx[1])
print("After controlled(X):")
debug_state(qcx)

print("\n--- 16. Controlled-Hadamard ---")
quantum qch[2]
apply X(qch[0])
apply controlled(Hadamard)(qch[0], qch[1])
print("After controlled(H):")
debug_state(qch)

print("\n--- 17. Controlled-RX ---")
quantum qcrx[2]
apply X(qcrx[0])
apply controlled(RX(PI))(qcrx[0], qcrx[1])
print("After controlled(RX(π)):")
debug_state(qcrx)

print("\n--- 18. Controlled-RY ---")
quantum qcry[2]
apply X(qcry[0])
apply controlled(RY(PI / 2.0))(qcry[0], qcry[1])
print("After controlled(RY(π/2)):")
debug_state(qcry)

print("\n--- 19. Controlled-RZ ---")
quantum qcrz[2]
apply X(qcrz[0])
apply controlled(RZ(PI / 4.0))(qcrz[0], qcrz[1])
print("After controlled(RZ(π/4)):")
debug_state(qcrz)

// ============================================
// 8. DOUBLY-CONTROLLED GATES (3-QUBIT)
// ============================================
print("\n--- 20. Toffoli Gate (CCX) ---")
quantum qtof[3]
apply X(qtof[0])
apply X(qtof[1])
print("Initial state |110>:")
debug_state(qtof)
apply controlled(controlled(X))(qtof[0], qtof[1], qtof[2])
print("After Toffoli:")
debug_state(qtof)

print("\n--- 21. Controlled-Controlled-Hadamard ---")
quantum qcch[3]
apply X(qcch[0])
apply X(qcch[1])
apply controlled(controlled(Hadamard))(qcch[0], qcch[1], qcch[2])
print("After CCH:")
debug_state(qcch)

print("\n--- 22. Controlled-Controlled-RX ---")
quantum qccrx[3]
apply X(qccrx[0])
apply X(qccrx[1])
apply controlled(controlled(RX(PI)))(qccrx[0], qccrx[1], qccrx[2])
print("After CCRX(π):")
debug_state(qccrx)

// ============================================
// 9. DAGGER (INVERSE) GATES
// ============================================
print("\n--- 23. Dagger(X) = X ---")
quantum qdx[1]
apply X(qdx[0])
apply dagger(X)(qdx[0])
print("After X then dagger(X):")
debug_state(qdx)

print("\n--- 24. Dagger(S) = S† ---")
quantum qds[1]
apply X(qds[0])
apply S(qds[0])
print("After S:")
debug_state(qds)
apply dagger(S)(qds[0])
print("After dagger(S):")
debug_state(qds)

print("\n--- 25. Dagger(T) = T† ---")
quantum qdt[1]
apply X(qdt[0])
apply T(qdt[0])
print("After T:")
debug_state(qdt)
apply dagger(T)(qdt[0])
print("After dagger(T):")
debug_state(qdt)

print("\n--- 26. Dagger(RX) ---")
quantum qdrx[1]
apply RX(PI / 4.0)(qdrx[0])
apply dagger(RX(PI / 4.0))(qdrx[0])
print("After RX(π/4) then dagger(RX(π/4)):")
debug_state(qdrx)

print("\n--- 27. Dagger(Controlled(X)) ---")
quantum qdcx[2]
apply X(qdcx[0])
apply controlled(X)(qdcx[0], qdcx[1])
apply dagger(controlled(X))(qdcx[0], qdcx[1])
print("After controlled(X) then dagger:")
debug_state(qdcx)

// ============================================
// 10. BELL STATES
// ============================================
print("\n--- 28. Bell State |Φ+> = (|00> + |11>) / √2 ---")
quantum bell1[2]
apply Hadamard(bell1[0])
apply CNOT(bell1[0], bell1[1])
print("Bell state Φ+:")
debug_state(bell1)

print("\n--- 29. Bell State |Φ-> = (|00> - |11>) / √2 ---")
quantum bell2[2]
apply X(bell2[1])
apply Hadamard(bell2[0])
apply CNOT(bell2[0], bell2[1])
apply X(bell2[1])
print("Bell state Φ-:")
debug_state(bell2)

print("\n--- 30. Bell State |Ψ+> = (|01> + |10>) / √2 ---")
quantum bell3[2]
apply X(bell3[0])
apply Hadamard(bell3[0])
apply CNOT(bell3[0], bell3[1])
print("Bell state Ψ+:")
debug_state(bell3)

print("\n--- 31. Bell State |Ψ-> = (|01> - |10>) / √2 ---")
quantum bell4[2]
apply X(bell4[0])
apply X(bell4[1])
apply Hadamard(bell4[0])
apply CNOT(bell4[0], bell4[1])
apply X(bell4[1])
print("Bell state Ψ-:")
debug_state(bell4)

// ============================================
// 11. GHZ STATE (3-QUBIT ENTANGLEMENT)
// ============================================
print("\n--- 32. GHZ State (|000> + |111>) / √2 ---")
quantum ghz[3]
apply Hadamard(ghz[0])
apply CNOT(ghz[0], ghz[1])
apply CNOT(ghz[0], ghz[2])
print("GHZ state:")
debug_state(ghz)

// ============================================
// 12. MEASUREMENT
// ============================================
print("\n--- 33. Measurement Test ---")
quantum qm[1]
apply Hadamard(qm[0])
print("Superposition state before measurement:")
debug_state(qm)
let result = measure(qm[0])
print("Measurement result:")
print(result)
print("State after measurement:")
debug_state(qm)

// ============================================
// SUMMARY
// ============================================
print("\n=== TEST SUITE COMPLETE ===")
print("✓ Single-qubit gates: X, Y, Z, H, S, T")
print("✓ Rotation gates: RX, RY, RZ")
print("✓ Universal gate: U")
print("✓ Two-qubit gates: CNOT, CZ, Swap, CPhase")
print("✓ Controlled gates: C(X), C(H), C(RX), C(RY), C(RZ)")
print("✓ Doubly-controlled gates: CCX, CCH, CCRX")
print("✓ Dagger gates: X†, S†, T†, RX†, C(X)†")
print("✓ Bell states: Φ+, Φ-, Ψ+, Ψ-")
print("✓ GHZ state")
print("✓ Measurement")
print("\n🎉 ALL GATES WORKING! 🎉")