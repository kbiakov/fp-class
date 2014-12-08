import Data.Maybe
import Control.Monad
import Control.Applicative

{-
   Тип Parser может быть определён следуюшим образом:
-}

newtype Parser a = Parser { apply :: String -> Maybe (a, String) }

{-
   Определите экземпляры классов Monad и MonadPlus для типа Parser в этом случае:
-}

instance Monad Parser where
  return x = Parser (\s -> Just (x, s))
  p >>= q = Parser $ \s ->
    let r = apply p s in
      case r of
        Just (x, s') -> apply (q x) s'
        Nothing -> Nothing
  fail _ = Parser (\_ -> Nothing)

instance MonadPlus Parser where
  mzero = Parser (\_ -> Nothing)
  p `mplus` q = Parser $ \s ->
    let r = apply p s in
	  if isJust r then r
	  else apply q s
