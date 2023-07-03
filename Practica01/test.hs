import Shape
import Point
import Haskellium
import Trees
import Test.QuickCheck

shapes :: [Shape]
shapes = [
            Circle 22.6,
            Square 22.6,
            Rectangle 2.26 5.0,
            Triangle 2.26 5.0
         ]

haskellium1 :: Haskellium
haskellium1 = Haskellium {
	            name       = "Juanito",
	            lastName1  = "Perez",
	            lastName2  = "Lambda",
	            location   = (3,5),
	            houseShape = shapes!!2
}
haskellium2 :: Haskellium
haskellium2 = Haskellium {
	            name       = "Algo",
	            lastName1  = "Ritmo",
	            lastName2  = "C",
	            location   = (12,6),
	            houseShape = shapes!!0
}


----------------- SHAPES -----------------
-- PERIMETER
t1_perimeter :: Bool
t1_perimeter = perimeter (shapes!!0) == 142
t2_perimeter :: Bool
t2_perimeter=  perimeter (shapes!!2) == 14.52
--AREA
t1_area :: Bool
t1_area = area (shapes!!1) == 510.76000000000005
t2_area :: Bool
t2_area = area (shapes!!3) == 5.6499999999999995
-- ORD
t1_ord :: Bool
t1_ord = (sort shapes) == reverse shapes
----------------- POINT ---------------------
-- DISTANCE
t1_distance :: Bool
t1_distance = distance (1,1) (4,5) == 5
t2_distance :: Bool
t2_distance = distance (11,31) (42,5) == 40.459856648288
-- FROMO
t1_fromO :: Bool
t1_fromO = fromO (1,1) == 1.4142135623731
t2_fromO :: Bool
t2_fromO = fromO (11,31) == 32.893768406797
-------------- HASKELLIUMS -----------------
-- SON
t1_son :: Bool
t1_son = son haskellium1 haskellium2 "Mickey" == Haskellium {
	            name       = "Mickey",
	            lastName1  = "Perez",
	            lastName2  = "Ritmo",
	            location   = (3,5),
	            houseShape = shapes!!2
}
t2_son :: Bool
t2_son = son haskellium2 haskellium1 "Mickey" == Haskellium {
	            name       = "Mickey",
	            lastName1  = "Ritmo",
	            lastName2  = "Perez",
	            location   = (12,6),
	            houseShape = shapes!!0
}
-- HOUSE COST
t1_houseCost :: Bool
t1_houseCost = houseCost haskellium1 == area x + perimeter x
 where x = shapes!!2
t2_houseCost :: Bool
t2_houseCost = houseCost haskellium2 == area x + perimeter x
 where x = shapes!!0
-- TIME TO WORK
t1_timeToWork :: Bool
t1_timeToWork = timeToWork haskellium1 == fromO x
 where x = (3,5)
t2_timeToWork :: Bool
t2_timeToWork = timeToWork haskellium2 == fromO x
 where x = (12,6)














