module WC3PL.Core(interp,
                  Token(..)) where
import WC3PL.Maps (WC3Map, getFeatures, printMap)
import Data.Array.MArray (newArray, readArray, writeArray)

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
           | INCR
           | DECR
           | JMPZ
           | JMPNZ
           | PUTCHAR
           | GETCHAR
           | EOF
           deriving (Eq, Show)

type Commands = ([Token], Int)

getCommand :: Commands -> Token
getCommand (toks, n) = toks !! n

interp :: WC3Map -> [Token] -> IO ()
interp wc3map instrs = do
  mapM_ print (splitUp instrs)
  runState wc3map $ map (\x -> (x, 0)) (splitUp instrs)

splitUp :: [Token] -> [[Token]]
splitUp instrs =
  inner [] [] instrs
  where
    inner all [] [] = (reverse all)
    inner all lst [] = (reverse (lst:all))
    inner all [] (SPAWN:rest) = inner all [SPAWN] rest
    inner all lst (SPAWN:rest) = inner ((reverse lst):all) [SPAWN] rest
    inner all lst (TRAIN_FOOTMAN:rest) = inner ((reverse lst):all) [TRAIN_FOOTMAN] rest
    inner all lst (EOF:rest) = inner ((reverse lst):all) [] rest
    inner all lst (a:rest) = inner all (a:lst) rest

runState :: WC3Map -> [Commands] -> IO ()
runState wc3map commands =
  printMap wc3map

  -- split into logical threads of execution
  -- for each thread, in order specified, execute command if possible
  -- do any "end of turn" map updating
