{
module Main(main) where
import Data.Array.MArray
import Data.Array.IO
import Data.Char
import System.Environment
}
%wrapper "monadUserState"

tokens :-

                                \>                              { emit MOV_RIGHT }
                                \<                              { emit MOV_LEFT }
                                \^                              { emit MOV_UP }
                                v                               { emit MOV_DOWN }
                                s                               { emit SPAWN }
                                g                               { emit GATHER }
                                r                               { emit RETURN }
                                bf                              { emit BUILD_FARM }
                                bb                              { emit BUILD_RAX }
                                t                               { emit TRAIN_FOOTMAN }
                                x                               { emit DIE }
                                $white                          { skip }

{

emit :: Token -> (AlexPosn, Char, String) -> Int -> Alex Token
emit token (_,_,input) len = do
  return token

data Token = MOV_RIGHT
           | MOV_LEFT
           | MOV_UP
           | MOV_DOWN
           | SPAWN
           | BUILD_FARM
           | BUILD_RAX
           | TRAIN_FOOTMAN
           | GATHER
           | RETURN
           | DIE
           | SEMAPHORE_UP
           | SEMAPHORE_DOWN
           | EOF
           deriving (Eq, Show)

data AlexUserState = AlexUserState {
}

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState {
                    }

alexEOF = return EOF

scanner :: String -> Either String [Token]
scanner str = runAlex str $ do
  let loop toks = do tok <- alexMonadScan
                     case tok of
                          EOF -> return $ (reverse (EOF:toks))
                          _ -> let foo = loop (tok : toks) in foo
  loop []

main :: IO ()
main = do
  args <- getArgs
  map <- readFile $ head args
  instrs <- readFile $ (head . tail) args
  case (scanner instrs) of
    Left message -> print message
    Right tokens -> mapM_ print tokens

}