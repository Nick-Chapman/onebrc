
-- | 4-value Parser Combinators
module Par4 (Par,parseFile,word,key,int,ws0,ws1,sp,nl,lit,sat,char,alts,opt,separated,terminated,many,some,digit,dot,noError) where

import Control.Applicative (Alternative,empty,(<|>),many,some)
import Control.Monad (ap,liftM)
import qualified Data.Char as Char

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
type Input = Text
inputFile :: FilePath -> IO Text
inputFile = Text.readFile
uncons :: Input -> Maybe (Char,Input)
uncons = Text.uncons
lengthInput :: Input -> Int
lengthInput = Text.length
getAt :: Input -> Int -> Char
getAt = undefined

instance Functor Par where fmap = liftM
instance Applicative Par where pure = Ret; (<*>) = ap
instance Alternative Par where empty = Fail; (<|>) = Alt
instance Monad Par where (>>=) = Bind

separated :: Par () -> Par a -> Par [a]
terminated :: Par () -> Par a -> Par [a]
opt :: Par a -> Par (Maybe a)
alts :: [Par a] -> Par a
word :: Par String
key :: String -> Par ()
int :: Par Int
ws1 :: Par ()
ws0 :: Par ()
digit :: Par Int
sp :: Par ()
nl :: Par ()
lit :: Char -> Par ()
dot :: Par Char
sat :: (Char -> Bool) -> Par Char
char :: Par Char
noError :: Par a -> Par a

separated sep p = do x <- p; alts [ pure [x], do sep; xs <- separated sep p; pure (x:xs) ]
terminated term p = alts [ pure [], do x <- p; term; xs <- terminated term p; pure (x:xs) ]
opt p = alts [ pure Nothing, fmap Just p ]
alts = foldl Alt Fail
word = some $ sat Char.isAlpha
key cs = NoError (mapM_ lit cs)
int = foldl (\acc d -> 10*acc + d) 0 <$> some digit
ws1 = do sp; ws0
ws0 = do _ <- many sp; return ()
digit = digitOfChar <$> sat Char.isDigit
sp = lit ' '
nl = lit '\n'
lit x = do _ <- sat (== x); pure ()
dot = sat (/= '\n')

sat = Satisfy
char = sat (const True)

digitOfChar :: Char -> Int
digitOfChar c = Char.ord c - ord0 where ord0 = Char.ord '0'

noError = NoError

data Par a where
  Ret :: a -> Par a
  Bind :: Par a -> (a -> Par b) -> Par b
  Fail :: Par a
  Satisfy :: (Char -> Bool) -> Par Char
  NoError :: Par a -> Par a
  Alt :: Par a -> Par a -> Par a

type Res a = Either Input (a,Input)

-- Four continuations:
data K4 a b = K4
  { eps :: a -> Res b            -- success; *no* input consumed
  , succ :: Input -> a -> Res b -- success; input consumed
  , fail :: () -> Res b          -- failure; *no* input consumed
  , err :: Input -> Res b       -- failure; input consumed (so an error!)
  }

parseFile :: Par a -> FilePath -> IO a
parseFile par filename = do
  input <- inputFile filename
  pure (parseInput par input)

parseInput :: Par a -> Input -> a
parseInput parStart input0  = do

  let
    len0 = lengthInput input0

    report :: Input -> String
    report remains = item ++ " at " ++ lc pos
      where
        item = if pos == len0 then "<EOF>" else show (input0 `getAt` pos)
        pos = len0 - lengthInput remains

    lc :: Int -> String
    lc p = "line " ++ show line ++ ", column " ++ show col
      where
        (line,col) =  (0::Int,p) -- TODO: fix hack!
        --line :: Int = 1 + length [ () | c <- take p chars, c == '\n' ]
        --col :: Int = length (takeWhile (/= '\n') (reverse (take p chars)))

    kFinal = K4 { eps = \a -> Right (a,input0)
                , succ = \chars a -> Right (a,chars)
                , fail = \() -> Left input0
                , err = \chars -> Left chars
                }

    run :: Input -> Par a -> K4 a b -> Res b
    run chars par k@K4{eps,succ,fail,err} = case par of

      Ret x -> eps x

      Fail -> fail ()

      Satisfy pred -> do
        case uncons chars of
          Nothing -> fail ()
          Just (c,chars) -> if pred c then succ chars c else fail ()

      NoError par -> do
        run chars par K4 { eps = eps
                         , succ = succ
                         , fail = fail
                         , err = \_ -> fail ()
                         }

      Alt p1 p2 -> do
        run chars p1 K4{ eps = \a1 ->
                           run chars p2 K4{ eps = \_ -> eps a1 -- left biased
                                          , succ
                                          , fail = \() -> eps a1
                                          , err
                                          }
                       , succ
                       , fail = \() -> run chars p2 k
                       , err
                       }

      Bind par f -> do
        run chars par K4{ eps = \a -> run chars (f a) k
                        , succ = \chars a ->
                            run chars (f a) K4{ eps = \a -> succ chars a -- consume
                                              , succ
                                              , fail = \() -> err chars -- fail->error
                                              , err
                                              }
                        , fail
                        , err
                        }

  case (run input0 parStart kFinal) of
    Left (remains) ->
      error $ "failed to parse: " ++ report remains
    Right (a,remains) ->
      if lengthInput remains == 0 then a else
        error $ "unparsed input from: " ++ report remains
