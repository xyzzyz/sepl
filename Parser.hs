module Parser(translationUnit) where

import Data.Maybe

import Control.Monad

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Prim
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Expr

import AST

primitiveTypes = ["void", "int", "bool"]
keywords = ["if", "while", "locals", "arrays"]

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

functionCall = do
  name <- identifier
  (args, arrays) <- parens argsAndMaybeArrays
  return $ Call name args arrays
  where argsAndArrays = do
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

term = (fmap StringLiteral stringLiteral)
       <|> (fmap (IntLiteral . fromIntegral)  integer)
       <|> (try functionCall)
       <|> (try varAssignment)
       <|> (try arrAssignment)
       <|> (fmap Variable identifier)
       <|> (parens expr)

table = [[prefix "!" (Not)],
         [binary "*" (Mul) AssocLeft, binary "/" (Div) AssocLeft ],
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
  return $ FunctionDefinition typ name args locals arrays body
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

expressionStatement = fmap ExpressionStatement expr

ifElseStatement = do
  reserved "if"
  cond <- parens expr
  thn <- statement
  reserved "else"
  els <- statement
  return $ IfElseStatement cond thn els

whileStatement = do
  reserved "while"
  cond <- parens expr
  body <- statement
  return $ WhileStatement cond body

blockStatement = do
  stmts <- braces (endBy statement semi)
  return $ BlockStatement stmts

statement = expressionStatement
            <|> ifElseStatement
            <|> whileStatement
            <|> blockStatement
            <?> "statement"

translationUnit = endBy functionDefinition semi