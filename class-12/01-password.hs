{-
   Модифицируйте представленное на лекции решение задачи о запросе пароля,
   удовлетворяющего требованиям по стойкости, следующим образом:
   - в командной строке задаются ограничения (минимальная длина, наличие букв,
     наличие цифр, наличие знаков пунктуации);
   - к стеку монад добавляется монада Reader, и с её помощью организуется
     доступ к ограничениям в функции isValid;
   - все попытки ввода пароля протоколируются средствами монады Writer.
-}

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.Writer

import Data.Char
import System.Environment
import Control.Monad

isValid :: String -> [String] -> Bool
isValid s args = let [minlen, alpha, num, punct] = args in (
  length s >= read minlen &&
  (not (read alpha) || (any isAlpha s)) &&
  (not (read num)   || (any isNumber s)) &&
  (not (read punct) || (any isPunctuation s)))
    
getValidPassword :: [String] -> MaybeT (ReaderT [String] (WriterT [String] IO)) String 
getValidPassword x = do 
  liftIO $ putStrLn "Введите новый пароль:" 
  s <- liftIO getLine 
  tell [s]
  guard (isValid s x) 
  return s 

askPassword :: MaybeT (ReaderT [String] (WriterT [String] IO)) ()
askPassword  = do 
  config <- lift ask
  value <-  msum $ repeat (getValidPassword config)
  liftIO $ putStrLn "Сохранение в базе данных..." 

main = do
  args <- getArgs
  (_, passwords) <- runWriterT (runReaderT (runMaybeT askPassword) args)
  print passwords
