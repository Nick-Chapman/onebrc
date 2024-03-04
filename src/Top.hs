
module Top (main) where

import Data.Char (ord)
import Data.List (intercalate)
import Data.Text (Text,uncons)
import System.Environment (getArgs)
import Text.Printf (printf)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text.IO as Text

main :: IO ()
main = do
  base <- head <$> getArgs
  let filename = "data/"++base++".txt"
  entries <- parseEntries filename
  let xs = collate_and_analyze entries
  putStrLn (prettyPrint xs)

prettyPrint :: [(Name,Quad)] -> String
prettyPrint xs =
  "{" ++ intercalate ", " [ name ++ "=" ++ show trip | (name,trip) <- xs ] ++ "}"

type Name = String
data Quad = Quad { min :: Temp, tot :: Temp, len :: Int, max :: Temp }

instance Show Quad where
  show Quad{min,tot,len,max} = do
    let mean :: Temp = round (fromIntegral tot / fromIntegral len :: Float)
    printf "%s/%s/%s" (show min) (show mean) (show max)

newtype Temp = Tenths Int deriving (Eq,Ord,Num,Integral,Real,Enum)

instance Show Temp where
  show (Tenths x) =
    if x < 0
    then printf "-%d.%d" ((-x) `div` 10) ((-x) `mod` 10)
    else printf "%d.%d" (x `div` 10) (x `mod` 10)

mkTemp :: Bool -> Int -> Int -> Temp
mkTemp isNeg w f = Tenths ((if isNeg then negate else id) (10*w+f))

type PR = [(Name,Temp)]

parseEntries :: FilePath -> IO PR
parseEntries fp = do
  t0 <- Text.readFile fp
  let

    startLine :: Text -> PR
    startLine t = do
      case uncons t of
        Nothing -> []
        Just (c,t) ->
          collectName [c] t

    ord0 = ord '0'

    convDigit :: Char -> Int
    convDigit c = ord c - ord0

    collectName :: [Char] -> Text -> PR
    collectName acc t =
      case uncons t of
        Nothing -> error "expected first char of name"
        Just (c,t) -> do
          case c of
            ';' -> collectTemp0 (reverse acc) t
            _ -> collectName (c:acc) t

    collectTemp0 :: Name -> Text -> PR
    collectTemp0 name t = do
      case uncons t of
        Nothing -> error "expected first char of temperature"
        Just (c,t) ->
          case c of
            '-' ->
              case uncons t of
                Nothing -> error "expected digit after minus sign"
                Just (c,t) ->
                  collectTemp name True (convDigit c) t
            _ ->
              collectTemp name False (convDigit c) t

    collectTemp :: Name -> Bool -> Int -> Text -> PR
    collectTemp name sign n t = do
      case uncons t of
        Nothing -> error "expected a decimal point"
        Just (c,t) -> do
          case c of
            '.' ->
              case uncons t of
                Nothing -> error "expected digit after decimal point"
                Just (c,t) -> do
                  case uncons t of
                    Nothing -> error "expected newline after final digit"
                    Just (_newline_,t) -> do
                      (name, mkTemp sign n (convDigit c)) : startLine t
            _ ->
              collectTemp name sign (10 * n + convDigit c) t

  pure (startLine t0)

collate_and_analyze :: [(Name,Temp)] -> [(Name,Quad)]
collate_and_analyze xs = openState (foldl step state0 xs)

type State = Map Name Quad

step :: State -> (Name,Temp) -> State
step m (name,temp1) = do
  case Map.lookup name m of
    Nothing -> Map.insert name (singleQ temp1) m
    Just temp2 -> Map.insert name (combineQ temp1 temp2) m

openState :: State -> [(Name,Quad)]
openState = Map.toList

state0 :: State
state0 = Map.empty

singleQ :: Temp -> Quad
singleQ v = Quad { min = v, tot = v, len = 1, max = v }

combineQ :: Temp -> Quad -> Quad
combineQ v1 Quad{min,tot,len,max} =
  Quad { min = Prelude.min v1 min
       , tot = tot + v1
       , len = len + 1
       , max = Prelude.max v1 max
       }
