module AST(Type(..),
           Expression(..),
           Statement(..),
           FunctionDefinition(..),
           TranslationUnit) where


data Type = Int | Bool | Void | IntArray
                     deriving (Show, Eq)

data Expression = IntLiteral Int
                | StringLiteral String
                | Input
                | Sizeof String
                | Variable String
                | VarAssign String Expression
                | ArrAssign String Expression Expression
                | ArrRef String Expression
                | Not Expression
                | Or Expression Expression
                | And Expression Expression
                | Nand Expression Expression
                | Add Expression Expression
                | Sub Expression Expression
                | Mul Expression Expression
                | Div Expression Expression
                | Equals Expression Expression
                | LessThan Expression Expression
                | GreaterThan Expression Expression
                | LessOrEqual Expression Expression
                | GreaterOrEqual Expression Expression
                | Call String [Expression] [Expression]
                deriving (Show)

type TypedExpression = (Type, Expression)

data Statement = ExpressionStatement Expression
               | BlockStatement [Statement]
               | WhileStatement Expression Statement
               | IfElseStatement Expression Statement Statement
               | OutputStatement Expression
               | ReturnStatement (Maybe Expression)
               deriving (Show)

data FunctionDefinition = FunctionDefinition {
  retType :: Type,
  name    :: String,
  args    :: [(Type, String)],
  locals  :: [(Type, String)],
  arrays  :: [(Type, String)],
  body    :: Statement
  } deriving (Show)

type TranslationUnit = [FunctionDefinition]