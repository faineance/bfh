module Main where
import           Brainfuck
import           System.Environment (getArgs)





main :: IO ()
main = do
        filename <- fmap (!!0) getArgs
        contents <- readFile filename
        case parseBF contents of
             Left err -> print err
             Right program -> do
                            evalBF (repeat 0, 0, repeat 0) program
                            return ()
