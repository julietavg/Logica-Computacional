module Test where

import Parser
import Prop
import DPLL
import qualified Control.Applicative as Map

mugiCheck :: Bool -> IO ()
mugiCheck a
  | a = putStrLn "Correcto! Kaizoku ou ni ore wa naru"
  | otherwise = putStrLn "Mal! Donificaste a Ace :C"

ej1 = parser $ lexer "(duerme ∧ corre) ∨ (¬duerme ∧ salta)"
ej2 = parser $ lexer "duerme ∧ ¬duerme"
ej3 = parser $ lexer "(ronca ↔ duerme) ∧ (¬ronca ↔ ¬duerme)"
ej4 = parser $ lexer "(p → q) ∧ p ∧ ¬q"
ej5 = parser $ lexer "((corre → suda) ∨ (corre → cansarse)) ↔ (corre ∧ ¬suda ∧ ¬cansarse)"
ej6 = parser $ lexer "T → ((p ↔ q) ∧ ((p ∧ ¬q) ∨ (¬p ∧ q) ∨ ⊥))"

t1 =  (dpll $ clausulas $ fnc ej1) /= Map.empty
t2 =  (dpll $ clausulas $ fnc ej2) == Map.empty
t3 =  (dpll $ clausulas $ fnc ej3) /= Map.empty
t4 =  (dpll $ clausulas $ fnc ej4) == Map.empty
t5 =  (dpll $ clausulas $ fnc ej5) == Map.empty
t6 =  (dpll $ clausulas $ fnc ej6) == Map.empty

main = do
    putStrLn "Tests"
    mugiCheck t1
    mugiCheck t2
    mugiCheck t3
    mugiCheck t4
    mugiCheck t5
    mugiCheck t6
    