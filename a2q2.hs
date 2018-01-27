data Point = Point Float Float
type Region = Point -> Bool

circle_maker :: Float -> Region
circle_maker r = f1 r

f1 :: Float -> Point -> Bool
f1 r (Point x y) = (x*x + y*y - r*r <= 0)

rectangle_maker :: Float -> Float -> Region
rectangle_maker l b = f2 l b

f2 :: Float -> Float -> Point -> Bool
f2 l b (Point x y) = (abs x <= (l / 2.0) && abs y <= (b / 2.0))

not_in :: Region -> Region
not_in reg1 = f3 reg1

f3 :: Region -> Point -> Bool
f3 reg1 (Point x y) = not (reg1 (Point x y))

intersection :: Region -> Region -> Region
intersection reg1 reg2 = \(Point x y) -> (reg1 (Point x y)) && (reg2 (Point x y))

union :: Region -> Region -> Region
union reg1 reg2 = \(Point x y) -> (reg1 (Point x y)) || (reg2 (Point x y))

annulus :: Region -> Region -> Region
annulus reg1 reg2 = \(Point x y) -> ((union reg1 reg2) (Point x y)) && (not ((intersection reg1 reg2) (Point x y)))

translate :: Region -> Point -> Region
translate reg1 (Point x0 y0) = \(Point x y) -> (reg1 (Point (x-x0) (y-y0)))