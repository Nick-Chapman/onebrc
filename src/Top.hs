
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
  xs <- parseEntries filename
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

type PR = State

parseEntries :: FilePath -> IO [(Name,Quad)]
parseEntries fp = do
  t0 <- Text.readFile fp
  let

    startLine :: State -> Text -> PR
    startLine s t = do
      case uncons t of
        Nothing -> s
        Just (c,t) ->
          collectName s [c] t

    ord0 = ord '0'

    convDigit :: Char -> Int
    convDigit c = ord c - ord0

    collectName :: State -> [Char] -> Text -> PR
    collectName s acc t =
      case uncons t of
        Nothing -> error "expected first char of name"
        Just (c,t) -> do
          case c of
            ';' -> collectTemp0 s (reverse acc) t
            _ -> collectName s (c:acc) t

    collectTemp0 :: State -> Name -> Text -> PR
    collectTemp0 s name t = do
      case uncons t of
        Nothing -> error "expected first char of temperature"
        Just (c,t) ->
          case c of
            '-' ->
              case uncons t of
                Nothing -> error "expected digit after minus sign"
                Just (c,t) ->
                  collectTemp s name True (convDigit c) t
            _ ->
              collectTemp s name False (convDigit c) t

    collectTemp :: State -> Name -> Bool -> Int -> Text -> PR
    collectTemp s name sign n t = do
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
                      --(name, mkTemp sign n (convDigit c)) : startLine t
                      let s' = step s name (mkTemp sign n (convDigit c))
                      startLine s' t
            _ ->
              collectTemp s name sign (10 * n + convDigit c) t

  pure (openState (startLine state0 t0))

type State = Map Name Quad

step :: State -> Name -> Temp -> State
step m name temp1 = do
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
