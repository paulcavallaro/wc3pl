{
module Main(main) where
import Data.Char (chr)
import System.Environment (getArgs)
import WC3PL.Maps (buildMap)
import WC3PL.Core (Token(..), interp)
}
%wrapper "monadUserState"

tokens :-

        <0>                     \>                              { emit MOV_RIGHT }
        <0>                     \<                              { emit MOV_LEFT }
        <0>                     \^                              { emit MOV_UP }
        <0>                     v                               { emit MOV_DOWN }
        <0>                     s                               { emit SPAWN }
        <0>                     g                               { emit GATHER }
        <0>                     r                               { emit RETURN }
        <0>                     bf                              { emit BUILD_FARM }
        <0>                     bb                              { emit BUILD_RAX }
        <0>                     f                               { emit TRAIN_FOOTMAN }
        <0>                     x                               { emit DIE }
        <0>                     \[                              { emit JMPZ }
        <0>                     \]                              { emit JMPNZ }
        <0>                     \+                              { emit INCR }
        <0>                     \-                              { emit DECR }
        <0>                     \o                              { emit PUTCHAR }
        <0>                     \i                              { emit GETCHAR }
        <0>                     $white                          { skip }
        <0>                     \#                              { begin comment }
        <0>                     .                               { unrecognized }
        <comment>               \n                              { begin 0 }
        <comment>               .                               { skip }

{

emit :: Token -> (AlexPosn, Char, String) -> Int -> Alex Token
emit token (_,_,input) len = do
  return token

unrecognized :: (AlexPosn, Char, String) -> Int -> Alex Token
unrecognized (_,_,input) len = alexError $ "Could not recognize token:" ++ (take len input)

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
  map <- readFile (head args) >>= buildMap
  instrs <- readFile $ (head . tail) args
  case (scanner instrs) of
    Left message -> print message
    Right tokens -> interp map tokens



}
