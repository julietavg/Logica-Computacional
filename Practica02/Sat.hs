module Sat where
import Prop

type Literal = Prop
type Clausula = [Literal]

clausulas :: Prop -> [Clausula]
clausulas p = case p of
    And a b -> clausulas a ++ clausulas b
    Or a b -> [x ++ y | x <- clausulas a, y <- clausulas b]
    _ -> [[p]]

resolucion :: Clausula -> Clausula -> Clausula
resolucion c1 c2 = [l1 | l1 <- c1, notElem (neg l1) c2] ++ [l2 | l2 <- c2, notElem (neg l2) c1]
    where neg (Not l) = l
          neg l = Not l

hayResolvente :: Clausula -> Clausula -> Bool
hayResolvente c1 c2 = any (\l -> elem (neg l) c2) c1 || any (\l -> elem (neg l) c1) c2
    where neg (Not l) = l
          neg l = Not l

saturacion :: Prop -> Bool
saturacion p = go ([cl | cl <- clausulas (fnc (Not p))], []) 
    where go ([], _) = True
          go (cl:cls, ant) | any (\c -> not $ hayResolvente cl c) ant = go (cls, cl:ant)
                           | otherwise = False

