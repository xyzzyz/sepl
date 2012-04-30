module Parser(translationUnit) where

import Data.Char
import Data.Maybe

import Control.Monad

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Prim
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Expr

import AST

primitiveTypes = ["void", "int", "bool"]
keywords = ["if", "while", "locals", "arrays", "input", "output", "return", "sizeof", "call", "true", "false"]

cDef = javaStyle { reservedNames = keywords ++ primitiveTypes}

cLexer = P.makeTokenParser cDef

reserved = P.reserved cLexer
reservedOp = P.reservedOp cLexer

whitespace = P.whiteSpace cLexer
stringLiteral = P.stringLiteral cLexer
charLiteral = P.charLiteral cLexer
integer = P.integer cLexer
operator = P.operator cLexer
identifier = P.identifier cLexer
symbol = P.symbol cLexer
parens = P.parens cLexer
braces = P.braces cLexer
brackets = P.brackets cLexer
commaSep = P.commaSep cLexer
semi = P.semi cLexer

varAssignment = do
  name <- identifier
  reservedOp "="
  e <- expr
  return $ VarAssign name e

arrAssignment = do
  name <- identifier
  ix <- brackets expr
  reservedOp "="
  e <- expr
  return $ ArrAssign name ix e

arrRef = do
  name <- identifier
  ix <- brackets expr
  return $ ArrRef name ix

sizeof = do
  reserved "sizeof"
  arr <- identifier
  return $ Sizeof arr

input = do
  reserved "input"
  return Input

output = do
  reserved "output"
  val <- expr
  return $ Output val

true = do
  reserved "true"
  return $ BoolLiteral True

false = do
  reserved "false"
  return $ BoolLiteral False

term = (fmap StringLiteral stringLiteral)
       <|> (fmap (IntLiteral . fromIntegral)  integer)
       <|> (fmap (IntLiteral . fromIntegral . ord) charLiteral)
       <|> (try true)
       <|> (try false)
       <|> (try sizeof)
       <|> (try varAssignment)
       <|> (try arrAssignment)
       <|> (try arrRef)
       <|> (try input)
       <|> (try output)
       <|> (fmap Variable identifier)
       <|> (parens expr)

table = [[prefix "!" (Not)],
         [binary "*" (Mul) AssocLeft, binary "/" (Div) AssocLeft,
          binary "%" (Mod) AssocLeft],
         [binary "+" (Add) AssocLeft, binary "-" (Sub) AssocLeft],
         [binary "<=" (LessOrEqual) AssocLeft, binary ">=" (GreaterOrEqual) AssocLeft,
          binary "<" (LessThan) AssocLeft, binary ">" (GreaterThan) AssocLeft],
         [binary "==" (Equals) AssocLeft],
         [binary "&&" (And) AssocLeft, binary "||" (Or) AssocLeft,
          binary "^^" (Nand) AssocLeft]]

prefix name fun       = Prefix (do{ reservedOp name; return fun })
binary name fun assoc = Infix (do{ reservedOp name; return fun }) assoc

expr = buildExpressionParser table term
       <?> "expression"

reservedKeyword name = do
  reserved name
  return name

scalarTypeDeclaration = do
  t <- choice $ map reservedKeyword ["void", "int", "bool"]
  return . fromJust . lookup t $ [("void", Void), ("int", Int), ("bool", Bool)]

intArrayTypeDeclaration = do
  brackets (reserved "int")
  return IntArray

typeDeclaration = try intArrayTypeDeclaration
                  <|> scalarTypeDeclaration
                  <?> "type declaration"

block = braces (endBy statement semi)

functionDefinition = do
  typ <- scalarTypeDeclaration
  name <- identifier
  (args, locals, arrays) <- parens defs
  body <- block
  return $ FunctionDefinition typ name args locals arrays (BlockStatement body)
  where defs = do
          args <- commaSep (varDef typeDeclaration)
          semi
          reserved "locals"
          locals <- commaSep (varDef scalarTypeDeclaration)
          semi
          reserved "arrays"
          arrays <- commaSep (varDef intArrayTypeDeclaration)
          semi
          return (args, locals, arrays)
        varDef tp = do
          typ <- tp
          name <- identifier
          return (typ, name)

functionCallStatement = do
  varAssign <- optionMaybe maybeAssign
  reserved "call"
  name <- identifier
  (args, arrays) <- parens argsAndMaybeArrays
  return $ Call varAssign name args arrays
  where maybeAssign = do
          name <- identifier
          reservedOp "="
          return name
        argsAndArrays = do
          args <- commaSep expr
          semi
          reserved "arrays"
          arrays <- commaSep expr
          return (args, arrays)
        argsAndNoArrays = do
          args <- commaSep expr
          return (args, [])
        argsAndMaybeArrays = try argsAndArrays
                             <|> argsAndNoArrays


expressionStatement = fmap ExpressionStatement expr

maybeEncloseWithBlock s@(BlockStatement _) = s
maybeEncloseWithBlock s = BlockStatement [s]

ifElseStatement = do
  reserved "if"
  cond <- parens expr
  thn <- statement
  reserved "else"
  els <- statement
  return $ IfElseStatement cond (maybeEncloseWithBlock thn) (maybeEncloseWithBlock els)

whileStatement = do
  reserved "while"
  cond <- parens expr
  body <- statement
  return $ WhileStatement cond (maybeEncloseWithBlock body)

blockStatement = do
  stmts <- braces (endBy statement semi)
  return $ BlockStatement stmts

returnStatement = do
  reserved "return"
  val <- optionMaybe expr
  return $ ReturnStatement val

statement = try functionCallStatement
            <|> expressionStatement
            <|> ifElseStatement
            <|> whileStatement
            <|> blockStatement
            <|> returnStatement
            <?> "statement"

translationUnit = do
  whitespace
  endBy functionDefinition semi
