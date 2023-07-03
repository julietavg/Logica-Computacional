module Shape where

data Shape = Circle Float
        | Square Float
        | Rectangle Float Float
        | Triangle Float Float
        | Trapeze Float Float Float
        deriving (Show, Eq)
instance Ord Shape where
        a1 `compare` a2 = (area a1) `compare` (area a2)

area :: Shape -> Float
area (Circle radium) = 3.1416*(radium**2)
area (Square side) = side**2
area (Rectangle base height) = base*height
area (Triangle base height) = (base*height)/2
area (Trapeze minorBase majorBase height) = ((minorBase+majorBase)/2)*height

perimeter :: Shape -> Float
perimeter (Circle radium) = 2*3.1416*radium
perimeter (Square side) = side*4
perimeter (Rectangle base height) = 2*base+2*height
perimeter (Triangle base height) = base+height+(sqrt((base**2)+(height**2)))
perimeter (Trapeze minorBase majorBase height) = 2*(sqrt((height**2)+(((majorBase-minorBase)/2)**2)))+majorBase+minorBase
