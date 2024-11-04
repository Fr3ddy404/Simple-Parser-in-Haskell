module Parser (parseFile) where

import Text.Parsec.String ( Parser )
import Text.Parsec ( alphaNum, many, many1, satisfy, string, char, parse, ParseError, digit, eof )
import Control.Applicative ( (<$>), (<*>), (<|>) )
import Data.Char ( isUpper, isLower )
import Text.Parsec.Char ( endOfLine )
import Data.List ( intercalate )

import Ast (Ast (Node, Leaf), mergeFunction)


strAlphaNumParser :: Parser String
strAlphaNumParser = many1 alphaNum

upperIdParser :: Parser String
upperIdParser = (:) <$> satisfy isUpper <*> many alphaNum

lowerIdParser :: Parser String
lowerIdParser = (:) <$> satisfy isLower <*> many alphaNum

numeric :: Parser String
numeric = many1 digit

spaceOrTab :: Parser Char
spaceOrTab = char ' ' <|> char '\t'

space1 :: Parser String
space1 = many1 spaceOrTab

space0 :: Parser String
space0 = many spaceOrTab

listLowerIdParser :: Parser String
listLowerIdParser = listParser lowerIdParser space1 " "

emptyString :: Parser String
emptyString = return ""

listParser :: Parser String -> Parser String -> String -> Parser String
listParser parser separatorParser separator = do
  parsedList <- nonEmptyList parser separatorParser <|> return []
  return $ intercalate separator parsedList

listAstParser :: Parser String -> Parser String -> Parser [String]
listAstParser parser separatorParser = do
  nonEmptyList parser separatorParser <|> return []

listAstAstParser :: Parser Ast -> Parser String -> Parser [Ast]
listAstAstParser parser separatorParser = nonEmptyAstList parser separatorParser <|> return []

nonEmptyAstList :: Parser Ast -> Parser String -> Parser [Ast]
nonEmptyAstList parser separatorParser = do
  parsed <- parser
  rest <- (separatorParser *> (nonEmptyAstList parser separatorParser <|> return [])) <|> return []
  return $ parsed : rest

nonEmptyList :: Parser String -> Parser String -> Parser [String]
nonEmptyList parser separatorParser = do
  parsed <- parser
  rest <- (separatorParser *> (nonEmptyList parser separatorParser <|> return [])) <|> return []
  return $ parsed : rest

constParser :: Parser Ast
constParser = do
  con <- upperIdParser
  clt <- (space1 *> listAstParser lowerIdParser space1) <|> (space0 >> return [])
  return $ Node "Const" con (map (Leaf "ConstArg") clt)

dataParser :: Parser Ast
dataParser = do
  string "data"
  space1
  type_ <- upperIdParser
  ty <- (space1 *> listAstParser lowerIdParser space1) <|> return []
  space0 >> char '=' >>  space0
  const <- listAstAstParser constParser (space0 >> string "|" >> space0)
  return $ Node "Data" type_ (map (Leaf "Ty") ty ++ const)

typeParser :: Parser Ast
typeParser = do
  typ <- upperIdParser
  ty <- (space1 *> listAstParser (lowerIdParser <|> upperIdParser) space1) <|> (space0 >> return [])
  return $ Node "Type" typ (map (Leaf "TypeArg") ty)

funcSigParser :: Parser Ast
funcSigParser = do
  string "::" >> space0
  types <- listAstAstParser typeParser (space0 >> string "->" >> space0)
  return $ Node "FuncSig" "" types

p6 :: Parser Ast
p6 = (
  do
    op <- string "ADD" <|> string "SUB"
    p6 <- p6
    p7 <- p7
    return $ Node "P6" op [p6, p7]
  )
  <|> p7

p7 :: Parser Ast
p7 = (
  do
    op <- string "MUL" <|> string "DIV"
    p7 <- p7
    p10 <- p10
    return $ Node "P7" op [p7, p10]
  )
  <|> p10

p10 :: Parser Ast
p10 = (
  do
    p10 <- lowerIdParser
    (space1 >> Node "P10" p10 <$> argsParser) <|> return (Leaf "Atom" p10)
  ) <|> atomParser

argsParser :: Parser [Ast]
argsParser = listAstAstParser p10 space1

atomParser :: Parser Ast
atomParser = do
  space0
  (do
      value <- numeric <|> lowerIdParser
      return $ Leaf "Atom" value
    )
    <|> (char '(' >> space0 >> p6 <* space0 <* char ')')

outParser :: Parser Ast
outParser = p6

funcBodyParser :: Parser Ast
funcBodyParser = do
  args <- listAstParser (lowerIdParser <|> numeric) space1
  space0 >> char '=' >> space0
  out <- outParser
  return $ Node "Body" "" (map (Leaf "Arg") args ++ [out])

funcParser :: Parser Ast
funcParser = do
  name <- lowerIdParser
  space0 -- eigentlich braucht funcBodyParser space1, aber weil name der gleiche parser wie args ist, muss ein space dazwischen sein, und args kann kein :: sein
  funcContent <- funcSigParser <|> funcBodyParser
  return $ Node "Function" name [funcContent]


lineParser :: Parser Ast
lineParser = dataParser <|> funcParser

parseFile :: String -> String -> Either ParseError Ast
parseFile = parse $ do
  ast <- many (lineParser <* many endOfLine) <* eof
  return $ Node "Root" "" $ mergeFunction ast
