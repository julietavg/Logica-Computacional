module Point where

type Point = (Float,Float)	

distance :: Point -> Point -> Float
distance (x1,y1) (x2,y2) = sqrt((x2-x1)**2+(y2-y1)**2)

fromO :: Point -> Float
fromO (x,y) = sqrt(x*2+y*2)

