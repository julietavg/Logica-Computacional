{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module DPLL where

import Prop
import Data.List
import Data.Maybe
import Data.Ord (comparing)


type Interpretacion = [( String , Bool ) ]
type Estado = ( Interpretacion , [ Clausula ])

conflict :: Estado -> Bool
conflict (_, clausulas) = [] `elem` clausulas

success :: Estado -> Bool
success (_, clausulas) = Data.List.null clausulas

unit :: Estado -> Estado
unit (intp, clausulas) =
  case find isUnit clausulas of
    Just unitClausula -> (addLiteral intp (head unitClausula), simplify (removeLiteral unitClausula clausulas))
    Nothing -> (intp, clausulas)
  where
    isUnit :: Clausula -> Bool
    isUnit c = length c == 1

    addLiteral :: Interpretacion -> Literal -> Interpretacion
    addLiteral intp (V p) = (p, True) : intp
    addLiteral intp (NotV p) = (p, False) : intp

    removeLiteral :: Clausula -> [Clausula] -> [Clausula]
    removeLiteral c clausulas = Data.List.filter (/= c) (Data.List.map (remove c) clausulas)

    remove :: Clausula -> Clausula -> Clausula

    simplify :: [Clausula] -> [Clausula]
    remove c = Data.List.filter (`notElem` c)
    simplify = Data.List.filter (/= [])

elim :: Estado -> Estado
elim (interp, clausulas) =
  let clausulasEliminadas = foldr eliminarClausula clausulas interp
  in (interp, clausulasEliminadas)
  where
    eliminarClausula :: (String, Bool) -> [Clausula] -> [Clausula]
    eliminarClausula (var, True) clausulas = filter (notElem (V var)) clausulas
    eliminarClausula (var, False) clausulas = filter (notElem (NotV var)) clausulas

red :: Estado -> Estado
red (intp, clausulas) =
  let newClauses = simplify $ map (removeTrueLiterals intp) clausulas
  in (intp, newClauses)
  where
    removeTrueLiterals :: Interpretacion -> Clausula -> Clausula
    removeTrueLiterals intp clause = filter (not . isTrueLiteral intp) clause
    isTrueLiteral :: Interpretacion -> Literal -> Bool
    isTrueLiteral intp (V p) = (p, True) `elem` intp
    isTrueLiteral intp (NotV p) = (p, False) `elem` intp

    simplify :: [Clausula] -> [Clausula]
    simplify = filter (/= [])


sep :: Literal -> Estado -> (Estado, Estado)
sep lit (interp, clausulas) =
  let (var, val) = case lit of
                     V v -> (v, True)
                     NotV v -> (v, False)
      m1 = (addLiteral interp lit, clausulas)
      m2 = (addLiteral interp (negate lit), removeLiteral [negate lit] clausulas)
  in (m1, m2)
  where
    addLiteral :: Interpretacion -> Literal -> Interpretacion
    addLiteral intp l = (getVar l, getVal l) : intp
    
    removeLiteral :: [Literal] -> [Clausula] -> [Clausula]
    removeLiteral ls clausulas = filter (not . null) (map (remove ls) clausulas)
    
    remove :: [Literal] -> Clausula -> Clausula
    remove ls c = filter (not . (`elem` ls)) c
    
    getVar :: Literal -> String
    getVar (V v) = v
    getVar (NotV v) = v
    
    getVal :: Literal -> Bool
    getVal (V _) = True
    getVal (NotV _) = False
    
    negate :: Literal -> Literal
    negate (V v) = NotV v
    negate (NotV v) = V v

data ArbolDPLL = Node Estado ArbolDPLL | Branch Estado ArbolDPLL ArbolDPLL deriving (Eq, Show)

heuristicsLiteral :: [Clausula] -> Literal
heuristicsLiteral clauses =
  let literals = nub $ concat clauses
      occurrences = map (\l -> (l, countOccurrences l clauses)) literals
  in fst $ maximumBy (\(_, count1) (_, count2) -> compare count1 count2) occurrences
  where
    countOccurrences :: Literal -> [Clausula] -> Int
    countOccurrences lit clauses = length $ filter (elem lit) clauses

dpll :: [Clausula] -> Interpretacion
dpll clauses =
  let initialState = ([], clauses)
      searchTree = Node initialState (buildSearchTree initialState)
      solution = findSolution searchTree
  in maybe [] fst solution
  where
    buildSearchTree :: Estado -> ArbolDPLL
    buildSearchTree state@(intp, clauses)
      | conflict state = Branch state (Node ([], [[]]) (buildSearchTree ([], [[]]))) (Node ([], [[]]) (buildSearchTree ([], [[]])))
      | success state = Node state (Node ([], [[]]) (buildSearchTree ([], [[]])))
      | otherwise =
          let separator = heuristicsLiteral clauses
              (trueState, falseState) = sep separator state
          in Branch state (Node trueState (buildSearchTree trueState)) (Node falseState (buildSearchTree falseState))

    findSolution :: ArbolDPLL -> Maybe Estado
    findSolution (Node state _) = if success state then Just state else Nothing
    findSolution (Branch state leftTree rightTree) =
      let leftSolution = findSolution leftTree
          rightSolution = findSolution rightTree
      in case (leftSolution, rightSolution) of
           (Just solution, _) -> Just solution
           (_, Just solution) -> Just solution
           _ -> Nothing
