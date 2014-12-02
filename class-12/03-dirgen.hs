{-
  Для тестирования программ, работающих с файловой системой, часто необходимо
  достаточно большое дерево каталогов с файлами. Реализуйте случайный генератор
  такого дерева, управляемый набором параметров (минимальная и максимальная ширина
  и глубина дерева, количество и размеры файлов, что-нибудь ещё). В качестве идеи
  для архитектуры приложения используйте пример с подсчётом количества файлов в
  дереве (count.hs). Этот же пример можно использовать для тестирования
  разработанного приложения.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import System.Environment
import System.Directory
import System.FilePath
import System.Random

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative

import Data.List

data AppConfig = AppConfig {
      cfgRootDir :: FilePath,
      cfgDepth :: (Int,Int),
      cfgN :: (Int,Int),
      cfgFileSize :: (Int,Int)
    } deriving (Show)

data AppState = AppState {
      stCurDepth :: Int,
      stCurPath :: FilePath,
      stFname :: FilePath
    } deriving (Show)

type AppLog = [(FilePath, Int)]

newtype DirGen a = DirGen {
      run :: ReaderT AppConfig (StateT AppState (WriterT AppLog IO)) a
    } deriving (Functor, Applicative, Alternative,
				Monad, MonadPlus, MonadIO, MonadReader AppConfig,
				MonadWriter [(FilePath, Int)], MonadState AppState)

runDirGen :: DirGen a -> AppConfig -> IO (a, AppLog)
runDirGen app cfg@(AppConfig root depth width fsize) =
  let state = AppState 0 root "1"
  in runWriterT (evalStateT (runReaderT (run app) cfg) state)

parseConfig :: String -> AppConfig 
parseConfig s = 
  let [rootDir, a1, a2, b1, b2, c1, c2] = words s
  in AppConfig rootDir (read a1, read a2) (read b1, read b2) (read c1, read c2) 

listDirectory :: FilePath -> IO [String]
listDirectory = liftM (filter notDots) . getDirectoryContents
  where notDots p = p /= "." && p /= ".."

genFile :: DirGen ()
genFile = do
  st  <- get
  cfg <- ask
  let fname = stFname st
	  curPath = stCurPath st
	  (min, max) = cfgFileSize cfg    
  l <- liftIO $ randomRIO (min, max)
  let contents = take l . concat . repeat $ "ha"
  liftIO $ writeFile (curPath </> fname ++ ".txt") contents
  put $ st {stFname = show (read fname + 1)}

mkdir :: DirGen ()
mkdir = do 
  st <- get
  let curPath  = stCurPath st
	  fname    = stFname st
	  newPath  = curPath </> fname
	  curDepth = stCurDepth st
  liftIO $ createDirectory newPath
  put $ st {stFname = show (read fname + 1)}

step :: DirGen ()
step = do
  (min, max) <- cfgN <$> ask
  nFiles <- liftIO $ randomRIO (min, max)
  let nDirs = max - nFiles
  replicateM_ nFiles genFile
  replicateM_ nFiles mkdir

generate :: DirGen ()
generate = do
  cfg <- ask
  st <- get
  let maxDepth = snd . cfgDepth $ cfg
      curDepth = stCurDepth st
      curDir   = stCurPath st
  when (curDepth < maxDepth) $ do
    step
    ls <- liftIO $ listDirectory curDir
    let dirList = filter (not . (".txt" `isSuffixOf`)) ls
    forM_ dirList $ \name -> do
      let newDepth = curDepth + 1
      let newDir = curDir </> name
      liftIO $ setCurrentDirectory newDir
      put $ st {stCurDepth = newDepth, stCurPath = newDir}
      tell [(newDir,curDepth)]
      generate

main = do
  inp <- head <$> getArgs >>= readFile
  let cfg = parseConfig inp
  createDirectory (cfgRootDir cfg) 
  runDirGen (generate) cfg
