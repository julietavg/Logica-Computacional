module Prop where
import Data.List

data Prop = Var String | Cons Bool| Not Prop
          | And Prop Prop | Or Prop Prop
          | Impl Prop Prop | Syss Prop Prop
          deriving (Eq)

instance Show Prop where
    show p = case p of
        (Var s) -> s
        (Cons b) -> show b
        (Not p) -> "¬" ++ (show p)
        (And p q) -> "(" ++ (show p) ++ " ∧ " ++ (show q) ++ ")"
        (Or p q) -> "(" ++ (show p) ++ " ∨ " ++ (show q) ++ ")"
        (Impl p q) -> "(" ++ (show p) ++ " → " ++ (show q) ++ ")"
        (Syss p q) -> "(" ++ (show p) ++ " ↔ " ++ (show q) ++ ")"

data Literal = V String | NotV String deriving Eq
type Clausula = [Literal]

instance Show Literal where
    show p = case p of
                (V s) -> s
                (NotV s) -> "¬" ++ s

fnn :: Prop -> Prop
fnn p = negs_remove $ morgan $ imp_remove p

imp_remove :: Prop -> Prop
imp_remove (Impl p q) = Or (imp_remove (Not p)) (imp_remove q)
imp_remove (Syss p q) = (And (imp_remove (Impl p q)) (imp_remove (Impl q p)))
imp_remove (And p q) = And (imp_remove p) (imp_remove q)
imp_remove (Or p q) = Or (imp_remove p) (imp_remove q)
imp_remove (Not p) = Not (imp_remove p)
imp_remove p = p

morgan :: Prop -> Prop
morgan (Not (And p q)) = Or (morgan (Not p)) (morgan (Not q))
morgan (Not (Or p q)) = And (morgan (Not p)) (morgan (Not q))
morgan (Not p) = Not (morgan p)
morgan (And p q) = And (morgan p) (morgan q)
morgan (Or p q) = Or (morgan p) (morgan q)
morgan p = p

negs_remove :: Prop -> Prop
negs_remove (Not (Not p)) = negs_remove p
negs_remove (Not (Cons t)) = Cons (not t)
negs_remove (Not p) = Not (negs_remove p)
negs_remove (And p q) = And (negs_remove p) (negs_remove q)
negs_remove (Or p q) = Or (negs_remove p) (negs_remove q)
negs_remove p = p


fnc :: Prop -> Prop
fnc p = fnc' $ fnn p

fnc' :: Prop -> Prop
fnc' prop@(Or p q) = case (fnc' p, fnc' q) of
                     (And r s, t) -> And (fnc' (Or r t)) (fnc' (Or s t))
                     (r, And s t) -> And (fnc' (Or r s)) (fnc' (Or r t))
                     (r,s) -> prop
fnc' (And p q) = And (fnc' p) (fnc' q)
fnc' p = p

clausulas :: Prop -> [Clausula]
clausulas (And p q) = union (clausulas p)  (clausulas q)
clausulas p = [getLiterals p]

getLiterals :: Prop -> Clausula
getLiterals (Or p q) = union (getLiterals p) (getLiterals q)
getLiterals (Var q) = [V q]
getLiterals (Not (Var q)) = [NotV q]
getLiterals (Cons _) = []
getLiterals p = error "Formula no soportada"
