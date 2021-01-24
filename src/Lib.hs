
module Lib
 where

import System.Random
import qualified Data.Map.Strict as Map
import Control.Monad.State.Lazy
import Data.Tuple.Select
import Data.Maybe


someFunc :: IO ()
someFunc = putStrLn "someFunc"


data CellType = Empty | Fill | Unknown deriving (Show, Eq)
type RowInfo = [Int]
type Row = [CellType]
type Cache = Map.Map (Int, Int) Bool
type St = State Cache Bool
data Field = Field {rows :: [Row], cols :: [Row]} deriving Show
data FieldState = FieldState {byRow :: [Row]} deriving Show


negateCellType :: CellType -> CellType
negateCellType Fill  = Empty
negateCellType Empty = Fill

cleanRow :: Row -> Bool
cleanRow row = all (\x -> x /= Fill) row

canFit :: Row -> Int -> Bool
canFit row size | length row == size = all (\x -> x /= Empty) row
canFit row size = last row /= Fill && all (\x -> x /= Empty) (init row) 

inner :: RowInfo -> Row -> Int -> Int -> St
-- recursion by rowInfo
inner [] row pos islandPos = return $ cleanRow $ drop pos row
inner rowInfo row pos islandPos | sum rowInfo + length rowInfo - 1 > length row = return False
inner (r: rs) [] pos islandPos = return False
inner rowInfo@(ri: ris) (r: rs) pos islandPos | r == Empty = inner rowInfo rs (pos + 1) islandPos
inner rowInfo@(ri: ris) row@(r: rs) pos islandPos | r == Fill && canFit (take (ri + 1) row) ri = inner ris (drop (ri + 1) row) (pos + ri + 1) (islandPos + 1)
inner rowInfo@(ri: ris) row@(r: rs) pos islandPos | r == Fill = return False
inner rowInfo@(ri: ris) row@(r: rs) pos islandPos | r == Unknown && not (canFit (take (ri + 1) row) ri) = return False
inner rowInfo@(ri: ris) row@(r: rs) pos islandPos | r == Unknown = do
    n1 <- getOrUpdate ((pos + ri + 1, islandPos + 1))  (inner ris (drop (ri + 1) row) (pos + ri + 1) (islandPos + 1))
    n2 <- getOrUpdate ((pos + 1, islandPos))  (inner rowInfo (drop 1 row) (pos + 1) islandPos)
    return (n1 || n2)

getOrUpdate :: (Int, Int) -> State Cache Bool -> State Cache Bool
getOrUpdate k ifEmptyState = do
  maybeVal <- gets (Map.lookup k)
  case maybeVal of
    Just v -> return v
    Nothing -> do
      ifEmpty <- ifEmptyState
      modify (Map.insert k ifEmpty)
      return ifEmpty

isPossible :: RowInfo -> Row -> Bool
isPossible rowInfo row = evalState (inner rowInfo row 0 0) Map.empty

better :: RowInfo -> Row -> Maybe Row
better rowInfo row | any (\x -> x == Unknown) row == False = Nothing
better rowInfo row = listToMaybe res
    where
    withIndex   = zip [0..] row
    filterIndex = filter (\x -> snd x == Unknown) withIndex
    indecies = map fst filterIndex
    indecies :: [Int]
    opt = concat [[(row, update row x Empty, x, Empty), (row, update row x Fill, x, Fill)] | x <- indecies]
    opt :: [(Row, Row, Int, CellType)]
    optFilter = filter (\x -> not $ isPossible rowInfo $ sel2 x) opt
    optFilterHead = take 1 optFilter
    res = map (\x -> update (sel1 x) (sel3 x) (negateCellType $ sel4 x)) optFilterHead
    res :: [Row]
    
update :: [a] -> Int -> a -> [a]
update list pos value = h ++ value : t
    where
        (h, _ : t) = splitAt pos list

best :: RowInfo -> Row -> Row
best rowInfo row = 
    case b of Nothing -> row
              Just a -> best rowInfo a
    where 
        b = better rowInfo row















