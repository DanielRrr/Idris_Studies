-- Use `:doc Shape` to view information, provided via |||

||| Represents shapes
data Shape = ||| A triangle, with its base length and height
             Triangle Double Double
             | ||| A rectangle, with its length and height
             Rectangle Double Double -- width and height
             | ||| A circle, with its radius
             Circle Double -- radius

-- Implement interactively function `area` which calculates area of the given shape

area : Shape -> Double
area (Triangle x y) = 0.5 * x * y
area (Rectangle a b) = a * b
area (Circle r) = pi * r * r

-- Add documenting comments to this datatype representing pictures

data Picture = Primitive Shape
               | Combine Picture Picture
               | Rotate Double Picture
               | Translate Double Double Picture

-- Make sure you understand the meaning of word "translate" in this context

-- Examples of pictures
rectangle : Picture
rectangle = Primitive (Rectangle 20 10)

circle : Picture
circle = Primitive (Circle 5)

triangle : Picture
triangle = Primitive (Triangle 10 10)

test_picture : Picture
test_picture = Combine (Translate 5 5 rectangle)
                    (Combine (Translate 35 5 circle)
                    (Translate 15 25 triangle))

-- Implement interactively
picture_area : Picture -> Double
picture_area (Primitive shape) = area shape
picture_area (Combine shape1 shape2) = (picture_area shape1) + (picture_area shape2)
picture_area (Rotate x shape) = picture_area shape
picture_area (Translate x y shape) = picture_area shape

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing my = my
maxMaybe mx Nothing = mx
maxMaybe (Just x) (Just y) = ifThenElse (x > y) (Just x) (Just y)

||| Returns the area of the biggest triangle in a picture
biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive shape) =
    case shape of
         (Triangle x y) => Just (x * y / 2)
         (Rectangle x y) => Nothing
         (Circle r) => Nothing
biggestTriangle (Combine shape1 shape2) = maxMaybe (biggestTriangle shape1) (biggestTriangle shape2)
biggestTriangle (Rotate x shape) = biggestTriangle shape
biggestTriangle (Translate x y shape) = biggestTriangle 
