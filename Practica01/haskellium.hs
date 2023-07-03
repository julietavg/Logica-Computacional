module Haskellium where 
import Point
import Shape
data Haskellium = Haskellium { 
                               name :: String,
                               lastName1 :: String,
                               lastName2 :: String, 
                               location :: Point,
                               houseShape:: Shape
					         } deriving (Show)

--Dados dos Haskelliums y un String de nombre, regresa un Haskellium que sería hijo 
--de los dos Haskelliums con el nombre dado--

son :: Haskellium -> Haskellium -> String -> Haskellium
son father mother nameSon = Haskellium{
                                        name = nameSon,
                                        lastName1 = lastName1 father,
                                        lastName2 = lastName1 mother,
                                        location = location father,
                                        houseShape = houseShape father
									}


--Dado un Haskellium, calcula las unidades necesarias para construir su casa--

houseCost :: Haskellium -> Float
houseCost haskellium = let x = houseShape haskellium in area x + perimeter x


 
--Dado un Haskellium, se calcula el tiempo en unidades t, que le cuesta llegar a su trabajo.
timeToWork :: Haskellium -> Float
timeToWork haskellium = let x = location haskellium in if (fromO x <= 300.0) == True
																	then fromO (location haskellium)/30
																	else fromO (location haskellium)/70
