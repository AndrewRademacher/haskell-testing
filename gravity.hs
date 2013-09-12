--types for variables
type Mass = Double
type Pos = (Double, Double, Double)
type Obj = (Mass, Pos)

--list of functions needed
{-
 - Takes a list of objects.
 - Returns a list of (sum of mass times other object mass over distance for all objects)
 - Order is preserved.
 -}

--overall function type
calcMassesOverDists :: [Obj] -> [Double]
