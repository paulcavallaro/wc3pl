module WC3PL.Core(interp,
                  Token(..)) where
import WC3PL.Maps (WC3Map, getFeatures)
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

type Commands = [Token]

interp :: WC3Map -> [Token] -> IO ()
interp map instrs =
  mapM_ print (splitUp instrs)

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

runState :: WC3Map -> [Commands]
runState = undefined