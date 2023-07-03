module Prop where

data Prop = Var String | Cons Bool| Not Prop
          | And Prop Prop | Or Prop Prop
          | Impl Prop Prop | Syss Prop Prop
          deriving (Show, Eq)

{- PARTE 3  -}
-- Funcion que elimina equivalencias
eliminaEquiv :: Prop -> Prop
eliminaEquiv (Cons True) = (Cons True) 
eliminaEquiv (Cons False) = (Cons False) 
eliminaEquiv (Var p) = (Var p)
eliminaEquiv (Not a) = (Not(eliminaEquiv a))
eliminaEquiv (And a b) = (And (eliminaEquiv a) (eliminaEquiv b))
eliminaEquiv (Or a b) = (Or (eliminaEquiv a) (eliminaEquiv b))
eliminaEquiv (Impl a b) = (Impl (eliminaEquiv a) (eliminaEquiv b))
eliminaEquiv (Syss a b) = (And (Impl (eliminaEquiv a) (eliminaEquiv b)) (Impl (eliminaEquiv b) (eliminaEquiv a)))

-- Elimina Impllicaciones en una formula
eliminaImpl :: Prop -> Prop
eliminaImpl (Cons True) = (Cons True) 
eliminaImpl (Cons False) = (Cons False) 
eliminaImpl (Var p) = (Var p)
eliminaImpl (Not a) = (Not (eliminaImpl a))
eliminaImpl (And a b) = (And (eliminaImpl a) (eliminaImpl b))
eliminaImpl (Or a b) = (Or (eliminaImpl a) (eliminaImpl b))
eliminaImpl (Impl a b) = (Or (Not (eliminaImpl a)) (eliminaImpl b))

-- Empuja las Notaciones y elimina dobles Notaciones
empujaNot :: Prop -> Prop 
empujaNot (Cons True) = (Cons True) 
empujaNot (Cons False) = (Cons False) 
empujaNot (Var p) = (Var p)
empujaNot (And a b) = (And (empujaNot a)(empujaNot b))
empujaNot (Or a b) = (Or (empujaNot a)(empujaNot b))
empujaNot (Not a) = empujaNotAux(Not a)

-- Funcion auxiliar que considera el casos espaciales de la funcion empujaNot
empujaNotAux :: Prop -> Prop 
empujaNotAux (Not(Cons True)) = (Cons True) 
empujaNotAux (Not(Cons False)) = (Cons False) 
empujaNotAux (Not (Var p)) = (Not (Var p))
empujaNotAux (Not (And a b)) =(Or (empujaNot(Not a))(empujaNot(Not b)))
empujaNotAux (Not (Or a b)) = (And (empujaNot(Not a))(empujaNot(Not b)))
empujaNotAux (Not (Not a)) = empujaNot(a)

-- Forma normal Notativa
fnn :: Prop -> Prop 
fnn a = empujaNot((eliminaImpl(eliminaEquiv(a))))

{-Parte 4-}

{-Funcion que determina si una formula es una literal o no-}
literal :: Prop -> Bool
literal (Cons True) = True
literal (Cons False) = True 
literal (Var p) = True
literal (Not (Cons True)) = True 
literal (Not (Cons False)) = True
literal (Not (Var p)) = True
literal (Not a) = False -- Suponiendo que las Notaciones solo figuran frente a atomos
literal (And a b) = False
literal (Or a b) = False
literal (Impl a b) = False
literal (Syss a b) = False

{-Forma normal conjuntiva-}
fnc :: Prop -> Prop
fnc a = fncAux(fnn a)

-- Funcion auxiliar de la fnc
fncAux :: Prop -> Prop
fncAux (Cons True) = (Cons True) 
fncAux (Cons False) = (Cons False) 
fncAux (Var p) = (Var p)
fncAux (Not p) = (Not p) -- Suponiendo que las Notaciones figuran frente a atomos
fncAux (And a b) = (And (fnc(a))(fnc(b)))
fncAux (Or a b) = (distr (fnc a) (fnc b))

-- Funcion auxiliar de la fnc que distribuye una disyuncion
distr :: Prop -> Prop -> Prop 
distr a b = if (literal(a)&&literal(b)) then (Or a b) else distrAux a b

-- Funcion auxiliar de la funcion de distribucion
distrAux :: Prop -> Prop -> Prop
distrAux (Var p) (Or (Var q) (Var s)) = (Or (Var p) (Or (Var q) (Var s)))
distrAux (Or (Var q) (Var s)) (Var p)  = (Or  (Or (Var q) (Var s)) (Var p))
distrAux (Or a1 a2) (Or b1 b2) = (Or (distr a1 a2) (distr b1 b2))
distrAux (And a1 a2) (And b1 b2) = (And (distr a1 (And b1 b2)) (distr a2 (And b1 b2)))
distrAux (And a1 a2) b = (And (distr b a1) (distr b a2))
distrAux a (And b1 b2) = (And (distr a b1) (distr a b2))
distrAux a b = (Or a b) --Ya cubrimos todos los casos por lo tanto este ultimo seria el que es una clausula de puras disyunciones
