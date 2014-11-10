import Control.Applicative

{-
  Пользуясь возможностями аппликативных функторов, определите функцию, 
  вычисляющую наибольший из результатов двух вычислений (значений в некотором
  контексте), принимаемых в качестве параметров (для результатов имеется
  экземпляр класса типов Ord).
-}

maxApp2 :: (Ord a, Applicative f) => f a -> f a -> f a
maxApp2 x y = max <$> x <*> y

{- Реализуйте аналогичную функцию в случае трёх заданных значений в контексте. -}

maxApp3 :: (Ord a, Applicative f) => f a -> f a -> f a -> f a
maxApp3 x y z = max <$> x <*> maxApp2 y z

{- Реализуйте аналогичную функцию в случае списка значений в контексте. -}

maxApp :: (Ord a, Applicative f) => [f a] -> f a
maxApp xs = foldl1 (\a x -> max <$> a <*> x) xs 

{-
  Продемонстрируйте использование написанных функций для аппликативных функторов Maybe,
  список (для каждого из двух экземпляров), Either String и IO.
-}

main = do
  -- Maybe
  print $ maxApp3 (Just 1) (Just 3) (Just 2)
  -- список
  print $ maxApp [Just 1, Just 5, Just 2, Just 4, Just 3]
  print $ maxApp [(Left "a"), (Right "c"), (Right "b")]
  -- Either String
  print $ maxApp3 (Right "a") (Left "b") (Right "b")
  -- IO
  test <- maxApp2 getLine getLine
  print $ test

{- (необязательно)
  Ясно ли вам, что вы реализовали нечто, похожее на моноид на аппликативных функторах?
  Можете ли вы выразить это в коде? Необходимо ли добавлять какие-нибудь ограничения?
-}
