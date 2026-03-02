package cangjie

import std.collection.*

enum Rotation <: ToString & Hashable & Equatable<Rotation> {
    F
    | B
    | L
    | R
    | U
    | D
    | X(Rotation, Rotation) 
    | I(Rotation) 

    
    public operator func *(that: Rotation): Rotation {
        match (this) {
            case X(x1, x2) => X(x1, X(x2, that)) 
            case _ => X(this, that)
        }
    }

    
    public operator func **(exp: UInt32): Rotation {
        var result = this
        for (_ in 0..(exp - 1)) {
            result = result * this
        }
        return result
    }

    
    private func text(inv: Bool): String {
        let exp = if (inv) {
            "⁻¹"
        } else {
            ""
        }
        match (this) {
            case F => "F${exp}"
            case B => "B${exp}"
            case L => "L${exp}"
            case R => "R${exp}"
            case U => "U${exp}"
            case D => "D${exp}"
            case I(r) => r.text(!inv)
            case X(x1, x2) =>
                if (inv) { 
                    x2.text(inv) + x1.text(inv)
                } else {
                    x1.text(inv) + x2.text(inv)
                }
        }
    }

    public func toString(): String {
        text(false)
    }

    
    public func hashCode(): Int64 {
        this.toString().hashCode()
    }

    public operator func ==(that: Rotation): Bool {
        this.toString() == that.toString()
    }

    public operator func !=(that: Rotation): Bool {
        this.toString() != that.toString()
    }
}

type Face = Rotation

type Vector = Array<(Face, Int64)>

type Matrix = Array<Vector>

enum Index {
    Row(Int64) | Col(Int64) | Inv(Index)
    
    public operator func -(): Index {
        match (this) {
            case Inv(v) => v
            case _ => Inv(this)
        }
    }
    
    public func value(): Index {
        match (this) {
            case Inv(v) => v
            case _ => this
        }
    }
}

extend Matrix {
    
    public static func create(face: Face) {
        Matrix(
            3,
            {
                i => Vector(3, {j => (face, 3 * i + j + 1)})
            }
        )
    }

    
    public operator func [](index: Index): Vector {
        match (index) {
            case Row(r) => this[r].clone()
            case Col(c) => Vector(this.size, {i => this[i][c]})
            case Inv(i) => this[i] 
        }
    }

    
    public func set(index: Index, value: Vector): Unit {
        match (index) {
            case Row(r) => this[r] = value
            case Col(c) => for (i in 0..value.size) {
                this[i][c] = value[i]
            }
            case Inv(i) => 
                value.reverse()
                this.set(i, value)
        }
    }

    
    public func print(row: Int64) {
        for ((face, id) in this[row]) {
            print("${face}${id} ")
        }
    }

    
    public func print() {
        for (row in 0..this.size) {
            print("         ")
            print(row)
            println()
        }
    }
}


extend Int64 {
    public prop r: Index {
        get() {
            Index.Row(this)
        }
    }
    public prop c: Index {
        get() {
            Index.Col(this)
        }
    }
}



type Permutation = Array<(Face, Index)>

extend Permutation {
    
    public func inverse() {
        let perm = this.clone()
        for (i in 0..perm.size) {
            let j = (i + 1) % perm.size
            let (face, index) = this[i] 
            if (let Inv(_) <- this[j][1]) { 
                perm[i] = (face, -(index.value()))
            } else {
                perm[i] = (face, index.value())
            }
        }
        perm.reverse()
        return perm
    }
}

class Cube {
    public let data = HashMap<Face, Matrix>()
    private var history = ArrayList<Rotation>()
    private static let permutation = HashMap<Face, Permutation>()

    public init() {
        reset()
    }

    static init() {
        
        permutation[F] = [(L, 2.c), (U, -2.r), (R, 0.c), (D, -0.r)]
        permutation[B] = [(L, -0.c), (D, 2.r), (R, -2.c), (U, 0.r)]
        permutation[L] = [(U, 0.c), (F, 0.c), (D, 0.c), (B, 0.c)]
        permutation[R] = [(B, 2.c), (D, 2.c), (F, 2.c), (U, 2.c)]
        permutation[U] = [(L, 0.r), (B, -2.r), (R, -0.r), (F, 0.r)]
        permutation[D] = [(R, 2.r), (B, -0.r), (L, -2.r), (F, 2.r)]
    }

    public func reset() {
        history = ArrayList<Rotation>()
        for (face in [F, B, L, R, U, D]) {
            data[face] = Matrix.create(face)
        }
    }

    
    
    private func rotate(face: Face, inverse: Bool) {
        const N = 3
        let matrix = Matrix.create(face)
        let map = if (inverse) {
            {i: Int64, j: Int64 => (N - 1 - j, i)}
        } else {
            {i: Int64, j: Int64 => (j, N - 1 - i)}
        }
        for (i in 0..N) {
            for (j in 0..N) {
                let (u, v) = map(i, j)
                matrix[u][v] = data[face][i][j]
            }
        }
        data[face] = matrix
    }

    
    private func permute(perm: Permutation, inverse: Bool) {
        let p = if (inverse) {
            perm.inverse()
        } else {
            perm.clone()
        }
        p.reverse()
        var (lastFace, lastIndex) = p[0]
        let vector = data[lastFace][lastIndex]
        for (i in 1..p.size) {
            let (face, index) = p[i]
            data[lastFace].set(lastIndex, data[face][index])
            (lastFace, lastIndex) = (face, index)
        }
        data[lastFace].set(lastIndex, vector)
    }

    
    private func transform(rotation: Rotation, inverse: Bool): Unit {
        match (rotation) {
            case I(r) => transform(r, !inverse)
            case X(r1, r2) => 
                if (inverse) { 
                    transform(r2, inverse)
                    transform(r1, inverse)
                } else {
                    transform(r1, inverse)
                    transform(r2, inverse)
                }
            case _ => 
                rotate(rotation, inverse)
                permute(permutation[rotation], inverse)
        }
    }

    public func transform(rotation: Rotation) {
        transform(rotation, false)
        history.add(rotation)
        print()
    }

    public func print() {
        var prompt = ">> "
        for (rotation in history) {
            prompt += rotation.toString()
        }
        println(prompt)
        data[U].print()
        for (i in 0..3) {
            for (face in [L, F, R]) {
                data[face].print(i)
            }
            println()
        }
        data[D].print()
        data[B].print()
        println()
    }
}

main() {
    let cube = Cube()
    cube.print()
    
    for (r in [F, B, L, R, U, D]) {
        cube.transform(r) 
    }
    Cube().transform(F * R * I(F * R)) 

    
    
    Cube().transform((F * F * R * R) ** 6)
    
    Cube().transform((F * U * L * L * R) ** 36)
    Cube().transform((F * F * L * L * B * R) ** 90)

    
    
    let G = I(R) * D * R * F * D * I(F)
    Cube().transform(G)
    
    let H = U * G * I(U)
    Cube().transform(H)

    
    
    let M = R * I(L) * F * F * L * I(R) * I(D) * R * I(L) * F * L * I(R)
    Cube().transform(M)
    
    
    let N = M * I(U) * I(M) * U
    Cube().transform(N)
    
    let P = N * U * I(N) * I(U)
    Cube().transform(P)
}
