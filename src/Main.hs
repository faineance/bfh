module Main where
import Brainfuck
import Text.ParserCombinators.Parsec

program :: Either ParseError BFProgram
program = parseBF "[><+-,.]"

main :: IO()
main = print program
