module Main where
import Brainfuck
import Text.ParserCombinators.Parsec

program :: Either ParseError Program
program = parseBF "[><+-,.]"

main :: IO()
main = print program
