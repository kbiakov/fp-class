import Control.Monad
import Data.List
import Data.Maybe
import System.Environment

{-
   Дан текстовый файл (inventory.txt)  с перечислением всей имеющейся на складе
   лёгкой брони. Сформируйте список имеющихся полных комплектов брони одного
   вида (kind). Указание: в решении рекомендуется пользоваться монадическими
   операциями всюду, где только возможно.
-}

data ArmorType = Shield | Helmet | Gauntlets | Boots | Cuirass
   deriving (Show, Read, Eq, Ord, Enum, Bounded)
data ArmorKind = Chitin | Hide | Leather | Elven | Scaled | Glass | ImperialLight
   deriving (Show, Read, Eq, Ord, Enum, Bounded)
data ArmorItem = ArmorItem ArmorKind ArmorType 
   deriving (Show, Eq)
data ArmorKit = ArmorKit ArmorKind [ArmorType]
   deriving (Show, Eq)



loadInventory :: FilePath -> IO [ArmorItem]
loadInventory fname = readFile fname >>= return . map (item . words) . lines
  where item [kind, types] = ArmorItem (read kind) (read types)

buildArmorKit :: ArmorKind -> [ArmorItem] -> Maybe ArmorKit
buildArmorKit kind items
  | (length $ nub types) == 5 = Just (ArmorKit kind types)
  | otherwise = Nothing
  where types = map (\(ArmorItem _ t) -> t) $ filter (\(ArmorItem k _) -> k == kind) items

buildKits :: [ArmorItem] -> Maybe [ArmorKit]
buildKits items = sequence $ filter (isJust) ls
  where ls = zipWith buildArmorKit [Chitin, Hide, Leather, Elven, Scaled, Glass, ImperialLight] (replicate 7 items)

main = (head `liftM` getArgs) >>= loadInventory >>= print . buildKits
