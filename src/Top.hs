
module Top (main) where

import Data.List (intercalate)
import Par4 (Par,parse,terminated,nl,many,lit,digit,sat,alts,int)
import System.Environment (getArgs)
import Text.Printf (printf)
import qualified Data.Map as Map

main :: IO ()
main = do
  base <- head <$> getArgs
  let filename = "data/"++base++".txt"
  s <- readFile filename
  let entries = parse gram s
  let xs = [ (name,analyze temps) | (name,temps) <- collate entries ]
  putStrLn (prettyPrint xs)

gram :: Par [(Name,Temp)]
gram = terminated nl line
  where
    nameChar = sat (\c -> c /= ';')
    line = do
      name <- many nameChar; lit ';'
      isNeg <- alts [ do lit '-'; pure True, pure False ]
      whole <- int; lit '.'
      frac <- digit;
      pure (name, mkTemp isNeg whole frac)

collate :: Ord k => [(k,v)] -> [(k,[v])]
collate xs = Map.toList (Map.fromListWith (++) [ (k,[v]) | (k,v) <- xs ])

analyze :: [Temp] -> Trip
analyze xs =
  Trip { min = minimum xs
       , mean = computeMean xs
       , max = maximum xs
       }

prettyPrint :: [(Name,Trip)] -> String
prettyPrint xs =
  "{" ++ intercalate ", " [ name ++ "=" ++ show trip | (name,trip) <- xs ] ++ "}"

type Name = String
data Trip = Trip { min :: Temp, mean :: Temp, max :: Temp }

instance Show Trip where
  show Trip{min,mean,max} =
    printf "%s/%s/%s" (show min) (show mean) (show max)

data Temp = Tenths Int deriving (Eq,Ord)

instance Show Temp where
  show (Tenths x) =
    if x < 0
    then printf "-%d.%d" ((-x) `div` 10) ((-x) `mod` 10)
    else printf "%d.%d" (x `div` 10) (x `mod` 10)

mkTemp :: Bool -> Int -> Int -> Temp
mkTemp isNeg w f = Tenths ((if isNeg then negate else id) (10*w+f))

computeMean :: [Temp] -> Temp
computeMean xs = do
  let num = sum [ fromIntegral n :: Double | Tenths n <- xs ]
  let dem = fromIntegral (length xs)
  Tenths (round (num/dem))
