// in qstd.qc (Fixed indentation version)

circuit controlled_Rk(control_q: Qubit, target_q: Qubit, k: Float):
    let PI = 3.14159265
    let phi = 2.0 * PI / k
    apply CPhase(phi)(control_q, target_q)

circuit QFT_recursive(q: QuantumRegister, start_index: Int, end_index: Int):
    if start_index == end_index:
        apply Hadamard(q[start_index])
        return
    apply Hadamard(q[start_index])
    mut j = start_index + 1
    while j <= end_index:
        let k_val = (j - start_index) + 1.0
        apply controlled_Rk(q[j], q[start_index], k_val)
        j = j + 1
    QFT_recursive(q, start_index + 1, end_index)

circuit QFT(q: QuantumRegister):
    let size = q.length - 1
    QFT_recursive(q, 0, size)
    if size >= 1:
        apply Swap(q[0], q[size])