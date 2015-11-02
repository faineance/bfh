module Brainfuck where
import           Text.ParserCombinators.Parsec

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
           <|> do _ <- char '['
                  instrs <- many instr
                  _ <- char ']'
                  return . Loop $ instrs


parseBF :: String -> Either ParseError Program
parseBF = parse (many instr) "bf"

type Cell = Int
type Tape = ([Cell], Cell, [Cell])

eval' :: Tape ->  Instruction -> Tape
eval' (ls, c, r:rs) Next = (c:ls, r, rs)
eval' (l:ls, c, rs) Prev = (ls, l, c:rs)
eval' (ls, c, rs)   Inc  = (ls, c+1, rs)
eval' (ls, c, rs)   Dec  = (ls, c-1, rs)


eval :: Program -> Tape
eval = foldl eval' (repeat 0, 0, repeat 0)
