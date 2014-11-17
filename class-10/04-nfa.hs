{-
  Следующие типы задают множество состояний, алфавит и функцию переходов
  недетерминированного конечного автомата (НКА).
-}
type Alphabet = [Char]
type State = Int
type States = [State]
type AcceptingStates = [State]
type InitialState = State
type TransitionFunction = State -> Char -> States
type NFA = (Alphabet, States, InitialState, TransitionFunction, AcceptingStates)

-- пример НКА
nfa_ex :: NFA
nfa_ex = (['0','1'], [1, 2], 1, tf, [2])
  where
    tf 1 '0' = [1]
    tf 1 '1' = [1, 2]

-- Напишите функцию, определяющую, корректно ли задан НКА
isCorrect :: NFA -> Bool
isCorrect (abc, sts, inSt, trFunc, acSt) = corAbc && corSts && corBeg && corEnd
  where
    corAbc = not . null $ abc
	corSts = not . null $ sts
	corBeg = inSt `elem` sts
	corEnd = acSt `intersect` sts == acSt

-- в дальнейшем может пригодиться функция whileM,
-- которая собирает результаты в список, пока истинно
-- заданное условие
whileM :: m Bool -> m a -> m [a]
whileM = undefined

-- Напишите функцию, определяющую, допускает ли НКА заданное слово 
accept :: NFA -> String -> Bool
accept (abc, sts, inSt, trFunc, acSt) str = corStr && accStr
  where
    corStr = all (`elem` abc) str
    accStr = any (\x -> x `elem` acSt) $ foldl step [inSt] str
    step s c = concat $ map (\x -> if x `elem` acSt then [] else trFunc x c) s

-- Постройте ещё как минимум три примера НКА
nfa1 :: NFA
nfa1 = (['0','1'], [1, 2, 3], 1, tf, [3])
  where
    tf 1 '1' = [2]
    tf 2 '0' = [2, 3]

nfa2 :: NFA
nfa2 = (['0','1'], [0, 1, 2], 0, tf, [2])
  where 
    tf 0 '0' = [0, 1]
    tf 0 '1' = [0]
    tf 1 '0' = [2]
    tf 1 '1' = []
    tf 2 '0' = []
    tf 2 '1' = []

nfa3 :: NFA
nfa3 = (['0','1'], [1, 2, 3, 4], 1, tf, [4])
  where
    tf 1 '0' = [2]
    tf 1 '1' = []
    tf 2 '0' = []
    tf 2 '1' = [2, 3]
    tf 3 '0' = [4]
    tf 3 '1' = []

{-
  Распределите заданные строки в соответствии с распознающими
  их НКА (одна строка может попасть в несколько групп).
-}

classify :: [NFA] -> [String] -> [(NFA, [String])]
classify nfa s = nfa >>= \x -> return (x, filter (accept x) s) 
