{- Пользуясь списком как монадой, вычислите пересечение  заданных списков -}
import Control.Monad

intersect :: Eq a => [[a]] -> [a]
intersect [] = []
intersect = foldr1 (\x acc -> return x >>= filter (`elem` acc))
