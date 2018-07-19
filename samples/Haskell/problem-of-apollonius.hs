data Circle = Circle { x, y, r :: Double } deriving (Show, Eq)
data Tangent = Externally | Internally deriving Eq

{--
Solves the Problem of Apollonius (finding a circle tangent to three
other circles in the plane).

Params:
  c1 = First circle of the problem.
  c2 = Second circle of the problem.
  c3 = Third circle of the problem.
  t1 = How is the solution tangent (externally or internally) to c1.
  t2 = How is the solution tangent (externally or internally) to c2.
  t3 = How is the solution tangent (externally or internally) to c3.

Returns: The Circle that is tangent to c1, c2 and c3.
--}
solveApollonius :: Circle -> Circle -> Circle ->
                   Tangent -> Tangent -> Tangent ->
                   Circle
solveApollonius c1 c2 c3 t1 t2 t3 =
    Circle (m + n * rs) (p + q * rs) rs
    where
        s1 = if t1 == Externally then 1.0 else -1.0
        s2 = if t2 == Externally then 1.0 else -1.0
        s3 = if t3 == Externally then 1.0 else -1.0

        v11 = 2 * x c2 - 2 * x c1
        v12 = 2 * y c2 - 2 * y c1
        v13 = x c1 ^ 2 - x c2 ^ 2 +
              y c1 ^ 2 - y c2 ^ 2 -
              r c1 ^ 2 + r c2 ^ 2
        v14 = 2 * s2 * r c2 - 2 * s1 * r c1

        v21 = 2 * x c3 - 2 * x c2
        v22 = 2 * y c3 - 2 * y c2
        v23 = x c2 ^ 2 - x c3 ^ 2 +
              y c2 ^ 2 - y c3 ^ 2 -
              r c2 ^ 2 + r c3 ^ 2;
        v24 = 2 * s3 * r c3 - 2 * s2 * r c2

        w12 = v12 / v11
        w13 = v13 / v11
        w14 = v14 / v11

        w22 = v22 / v21 - w12
        w23 = v23 / v21 - w13
        w24 = v24 / v21 - w14

        p = -w23 / w22
        q =  w24 / w22
        m = -w12 * p - w13
        n =  w14 - w12 * q

        a = n * n + q ^ 2 - 1
        b = 2 * m * n - 2 * n * x c1 +
            2 * p * q - 2 * q * y c1 +
            2 * s1 * r c1
        c = x c1 ^ 2 + m ^ 2 - 2 * m * x c1 +
            p ^ 2 + y c1 ^ 2 - 2 * p * y c1 - r c1 ^ 2

        -- Find a root of a quadratic equation.
        -- This requires the circle centers not to be e.g. colinear.
        d = b ^ 2 - 4 * a * c
        rs = (-b - sqrt d) / (2 * a)

main = do
    let c1 = Circle 0.0 0.0 1.0
    let c2 = Circle 4.0 0.0 1.0
    let c3 = Circle 2.0 4.0 2.0
    let te = Externally
    print $ solveApollonius c1 c2 c3 te te te

    let ti = Internally
    print $ solveApollonius c1 c2 c3 ti ti ti
