{-# LANGUAGE TupleSections
            ,TypeSynonymInstances
            ,FlexibleInstances
            ,TypeApplications
            ,TypeFamilies
            ,DeriveFunctor #-}
module Oblig2 where
import Control.Arrow
import Control.Monad
import Data.List (intersperse,transpose,genericLength)
import Data.Functor (($>))
import Data.Tuple (swap)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Read (reads)
import Control.Applicative
import System.IO
import System.Exit

-- | A data type to represent expressions that can be evaluated to a number.
-- This type is parametric over both the number type and the cell reference type.
data Expression number cell
  = Ref cell                     -- ^ Reference to another cell
  | Constant number              -- ^ Constant numerical value
  | Sum (CellRange cell)         -- ^ Summation over a range of cells
  | Add
      (Expression number cell)   -- ^ Left operand of addition
      (Expression number cell)   -- ^ Right operand of addition
  | Mul
      (Expression number cell)   -- ^ Left operand of multiplication
      (Expression number cell)   -- ^ Right operand of multiplication
  | Sub
      (Expression number cell)   -- ^ Left operand of subtraction
      (Expression number cell)   -- ^ Right operand of subtraction
  deriving (Eq, Ord)

instance (Show number, Show cell) => Show (Expression number cell) where
  show (Ref cell) = "!" ++ show cell
  show (Constant n) = show n
  show (Sum (Box ul lr)) = "SUM(" ++ show ul ++ ":" ++ show lr ++ ")"
  show (Add a b) = "(" ++ show a ++ "+" ++ show b ++ ")"
  show (Mul a b) = "(" ++ show a ++ "*" ++ show b ++ ")"
  show (Sub a b) = "(" ++ show a ++ "-" ++ show b ++ ")"

data CellRange cell = Box { upperLeft  :: cell
                          , lowerRight :: cell}
    deriving (Eq, Ord, Show)

-- | The ranged class gives a method of indexing ranges of cells
--   in the spreadsheet
class (Ord cell) => Ranged cell where
    data Dimension cell
    cellRange :: Dimension cell -> CellRange cell -> Set cell

-- | A data type representing a sheet. 
-- It consists of a name and a mapping from cell references to expressions.
data Sheet number cell = Sheet
  { name :: String,
    -- ^ The name of the sheet
    dimension :: Dimension cell,
    -- ^ The dimension of the sheet
    content :: Map cell (Expression number cell)
    -- ^ The content of the sheet as a mapping from cell references to expressions
  }




-- | CellRef is the standard way to refer to a cell in the spreadsheet.
data CellRef = Cell { column :: Char, row :: Integer }
  deriving (Eq,Ord)

instance Show CellRef where
    show (Cell c r) = c:show r

instance Ranged CellRef where
    data Dimension CellRef
       = Dimension { columns :: [Char]
                   , rows :: [Integer] }
    cellRange dim box = undefined

-- | A sample spreadsheet using Double for numeric type
sheet1 :: Sheet Double CellRef
sheet1 =
  Sheet
    { name = "Sheet1", -- ^ Name of the sheet
      dimension = Dimension "ABC" [1..3],
      content = 
        Map.fromList
          [ ((Cell 'A' 1), Constant 12),
            ((Cell 'B' 1), Mul (Ref (Cell 'A' 1)) (Ref (Cell 'A' 2))),
            ((Cell 'C' 1), Ref (Cell 'C' 3)),
            ((Cell 'A' 2), Constant 4),
            ((Cell 'B' 2), Add (Ref (Cell 'B' 1)) (Ref (Cell 'A' 2))),
            ((Cell 'C' 2), Constant 0),
            ((Cell 'A' 3), Constant 9),
            ( (Cell 'B' 3),
              Sum (Box (Cell 'A' 1) (Cell 'B' 2))
            ),
            ((Cell 'C' 3), Constant 0)
          ]
    }

sheet2 :: Sheet Double CellRef
sheet2 = undefined


convert ::(Alternative f, Foldable t)
        => t a -> f a
convert = foldr ((<|>) . pure) empty

-- | Evaluate an expression within the context of a sheet.
-- Return Nothing if the expression cannot be evaluated.
evaluate :: (Num number, Ranged cell)
         => Sheet number cell
         -> Expression number cell
         -> Maybe number
evaluate sheet expr = undefined



-- The type of parsers
newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}
   deriving (Functor)


evalParser :: Parser a -> String -> Maybe a
evalParser p = (snd <$>) . runParser p

-- Applicative instance for Parser
instance Applicative Parser where
  pure x = Parser (\s -> Just (s, x))
  f <*> x = Parser $ \s -> do
    (s', f') <- runParser f s
    (s'', x') <- runParser x s'
    return (s'', f' x')

-- Monad instance for Parser which allows for sequencing parsers
-- using do-notation
instance Monad Parser where
  p >>= f =
    Parser
      ( \s -> do
          (s', x) <- runParser p s
          runParser (f x) s'
      )

-- Alternative instance for Parser
instance Alternative Parser where
  empty = Parser $ const Nothing
  p <|> q = Parser $ \s -> runParser p s <|> runParser q s

-- MonadFail instance for Parser
instance MonadFail Parser where
   fail _ = empty

-- A set of utility parsers

infixr 5 <:>
(<:>) = liftA2 (:)   -- Helper operator for parsing lists.


pMany :: Parser a -> Parser [a]
pMany p = (p <:> pMany p) <|> pure []


pSome :: Parser a -> Parser [a]
pSome p = p <:> pMany p

pSepBy1 :: Parser a -> Parser b -> Parser [b]
pSepBy1 sep p = p <:> (sep *> pSepBy1 sep p <|> pure [])

-- | Parse a single character
pNext :: Parser Char
pNext = Parser next where
   next "" = Nothing
   next (c:cs) = Just (cs , c)


pChar :: Char -> Parser ()
pChar c = undefined

-- | Eat a single space
pSpace :: Parser ()
pSpace = do
   c <- pNext
   guard (c == ' ')

-- | Eat a single newline
pNewLine :: Parser ()
pNewLine = do
   c <- pNext
   guard (c == '\n')
   
-- | Parse a keyword
pKeyword :: String -> Parser ()
pKeyword [] = return ()
pKeyword (k : ks) = do
    c <- pNext
    guard (c == k)
    pKeyword ks

   
pBetween :: Parser a -> Parser b -> Parser c -> Parser c
pBetween pOpen pClose pContent = undefined

-- | Parse parenthesis
inParenthesis :: Parser a -> Parser a
inParenthesis p = do
    pKeyword "("
    x <- p
    pKeyword ")"
    return x

-- | Parse brackets
inBrackets p = do
    pKeyword "["
    x <- p
    pKeyword "]"
    return x

-- | Convert a Read instance to a parser
pRead :: (Read a) => Parser a
pRead = Parser (fmap swap . convert . reads)

-- | Parse an atomic term
pTerm :: Read number => Parser (Expression number CellRef)
pTerm =  inParenthesis pExpression
     <|> pConstant
     <|> pRef
     <|> pSum

-- | Parse a cell name
pCell :: Parser CellRef
pCell = undefined

-- | Parse a numeric constant
pConstant :: (Read number) => Parser (Expression number cell)
pConstant = undefined

-- | Parse a cell reference
pRef :: Parser (Expression number CellRef)
pRef = undefined


-- | Parse a sum of cell refences like SUM(A1:C3)
pSum :: Parser (Expression number CellRef)
pSum = undefined

-- | Parse a multiplication expression
pMul :: Read number => Parser (Expression number CellRef)
pMul = undefined

-- | Parse an addition expression
pAdd :: Read number => Parser (Expression number CellRef)
pAdd = undefined


pSub :: Read number => Parser (Expression number CellRef)
pSub = undefined

-- | Parse cell expressions
pExpression :: (Read number)
            => Parser (Expression number CellRef)
pExpression = undefined



-- Now follows parsers for the sheet structure itself


pAlphaNum :: Parser Char
pAlphaNum = do
   x <- pNext
   guard (x `elem` (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']))
   pure x

pDigit :: Parser Integer
pDigit =   (pChar '0' $> 0)
      <|>  (pChar '1' $> 1)
      <|>  (pChar '2' $> 2)
      <|>  (pChar '3' $> 3)
      <|>  (pChar '4' $> 4)
      <|>  (pChar '5' $> 5)
      <|>  (pChar '6' $> 6)
      <|>  (pChar '7' $> 7)
      <|>  (pChar '8' $> 8)
      <|>  (pChar '9' $> 9)

fromDigits :: [Integer] -> Integer
fromDigits digits = sum $ zipWith (*)
                                  (reverse digits)
                                  [10^n | n <- [0..]]

-- | Parse a row number
pRowNumber :: Parser Integer
pRowNumber = fromDigits <$> pSome pDigit

-- | Parse a column name
pColName :: Parser Char
pColName = do
  c <- pNext
  guard (c `elem` ['A' .. 'Z'])
  return c

-- | Parse a sheet name
pSheetName :: Parser String
pSheetName = pSome pAlphaNum

-- | Parse a row, starting with "[n]" indicating the row number
pRow :: Read number => [Char] -> Parser (Integer,[(CellRef, Expression number CellRef)])
pRow cols = undefined


-- | Parse a spreadsheet
pSheet :: (Read number) => Parser (Sheet number CellRef)
pSheet = undefined

-- | Utility function to pad a list of columns to
--   specified lengths
padColumns :: [Integer] -> [String] -> String
padColumns lengths columns = concat $ intersperse " " $ zipWith pad lengths columns where
    pad len str = zipWith const (str ++ repeat ' ') [0..len -1]

-- | Pretty print a spreadsheet
instance (Show number) => Show (Sheet number CellRef) where
 show sheet = unlines (padColumns maxWidths <$> printedRows) where
    bracket s = "[" ++ s ++ "]"
    printedRows = (bracket (name sheet) : ( (bracket . pure) <$> columns (dimension sheet)))
                : [bracket (show r) : [maybe "" (bracket . show)
                                                (Map.lookup (Cell c r)
                                                            (content sheet))
                                         | c <- columns (dimension sheet)]
                              | r <- rows (dimension sheet)]
    maxWidths = (maximum . map genericLength) <$> transpose printedRows
                       
--  | Read a spreadsheet from file, evaluate and print it
getSpreadSheet :: FilePath -> IO (Sheet Double CellRef)
getSpreadSheet file = do
   unparsed <- readFile file
   convert (evalParser pSheet unparsed) <|> do
                  hPutStrLn stderr "No spreadsheet found"
                  exitWith $ ExitFailure 1

--  | Read a spreadsheet from file, evaluate and print it
runSpreadSheet :: FilePath -> IO (Sheet Double CellRef)
runSpreadSheet file = do
    sheet <- getSpreadSheet file
    let evaluated = Map.mapMaybe ((Constant <$>) . evaluate sheet)
                                 (content sheet)
    return $ Sheet (name sheet)
                   (dimension sheet)
                   (evaluated)

