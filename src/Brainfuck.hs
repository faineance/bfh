module Brainfuck where
import           System.Environment (getArgs)
import           Text.ParserCombinators.Parsec
import Data.Word
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

type Tape = ([Word8], Word8, [Word8])

evalBF :: Tape -> Program -> IO Tape
evalBF = foldM evalBF'
    where
        evalBF' :: Tape ->  Instruction -> IO Tape
        evalBF' (ls, c, r:rs) Next = return (c:ls, r, rs)
        evalBF' (l:ls, c, rs) Prev = return (ls, l, c:rs)
        evalBF' (ls, c, rs)   Inc  = return (ls, c+1, rs)
        evalBF' (ls, c, rs)   Dec  = return (ls, c-1, rs)
        evalBF' (ls, _, rs)   Read = do
                                        c <- fromIntegral . fromEnum <$> getChar
                                        return (ls, c, rs)
        evalBF' (ls, c, rs)   Write = do
                                        putChar . toEnum $ fromIntegral c
                                        return (ls, c, rs)
        evalBF' tape (Loop prog) = evalBF tape prog
        evalBF' (ls, 0, rs) (Loop _) = return (ls, 0, rs)
