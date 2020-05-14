module Shapes
( Point (..)
, Shape (..)
, surface
, nudge
, baseCircle
, baseRect)
where

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

-- Circle is a value constructor, which is a function that can be partially applied. Below we construct the circle with three fields and the pattern match 

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2 
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs y1 - y2) * (abs x2 - x1)

nudge :: Shape -> Float -> Float -> Shape 
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1 + a) (y2 + b)) (Point (x2 + a) (y2 + b))

baseCircle :: Float -> Shape
baseCircle = Circle (Point 0.0 0.0) 

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0.0 0.0) (Point width height) 

-- Maybe is a type constructor because it takes a type parameter
data Maybe a = Nothing | Just a 

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq, Show, Read)
-- person is an instance of the Eq typeclass and can be compared against oter persons 

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving(Eq, Ord, Show, Read, Enum, Bounded)

-- type constructor on left of assingment operator used to parameterize types included in the algebracie datatype; right side of assignment operator contains value constructor used to create fields in new datatype 


-- Recursive Datatypes

data List a = Empty | Cons a (List a) deriving (Eq, Show, Read, Ord) 

