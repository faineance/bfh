module Brainfuck where
import           Text.ParserCombinators.Parsec
import Control.Monad
data Instruction = Next | Prev | Inc | Dec | Read | Write | Loop [Instruction]
                deriving (Show, Eq)

type Program = [Instruction]

instr :: Parser Instruction
instr = (char '>' >> return Next)
           <|> (char '<' >> return Prev)
           <|> (char '+' >> return Inc)
           <|> (char '-' >> return Dec)
           <|> (char ',' >> return Read)
           <|> (char '.' >> return Write)
           <|> Loop <$> (char '[' *> many instr <* char ']')


parseBF :: String -> Either ParseError Program
parseBF = parse (many instr) "bf"


type Cell = Int
type Tape = ([Cell], Cell, [Cell])

evalBF :: Program -> IO Tape
evalBF = foldM evalBF' (repeat 0, 0, repeat 0)
    where
        evalBF' :: Tape ->  Instruction -> IO Tape
        evalBF' (ls, c, r:rs) Next = return (c:ls, r, rs)
        evalBF' (l:ls, c, rs) Prev = return (ls, l, c:rs)
        evalBF' (ls, c, rs)   Inc  = return (ls, c+1, rs)
        evalBF' (ls, c, rs)   Dec  = return (ls, c-1, rs)
        evalBF' (ls, c, rs)   Write = do
                                        print c
                                        return (ls, c, rs)
