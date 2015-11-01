module Brainfuck where
import           Text.ParserCombinators.Parsec

data BFInstruction = Next | Prev | Inc | Dec | Read | Write | Loop [BFInstruction]
                deriving Show
type BFProgram = [BFInstruction]


instruction :: Parser BFInstruction
instruction = (char '>' >> return Next)
           <|> (char '<' >> return Prev)
           <|> (char '+' >> return Inc)
           <|> (char '-' >> return Dec)
           <|> (char ',' >> return Read)
           <|> (char '.' >> return Write)
           <|> do _ <- char '['
                  instructions <- many instruction
                  _ <- char ']'
                  return . Loop $ instructions

parseBF :: String -> Either ParseError BFProgram
parseBF = parse (many instruction) "bf"
