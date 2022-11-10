-- | Handling simple submission conditions operating on tags and parameters
module Data.SubmissionConditions where

import Import hiding (many, try)

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text as T hiding (empty)
import Text.Read (readMaybe)

import qualified Data.Map as Map

data VariantEntry = VariantEntry {
  variantEntryTags :: [Tag],
  variantEntryParams :: [Parameter]
}

type VariantEntryMap = Map Text Text

entryToMap :: VariantEntry -> VariantEntryMap
entryToMap entry = Map.fromList ((Import.map (\t -> (tagName t, "TAG")) tags)
                                 ++ (Import.map (\p -> (parameterName p, parameterValue p)) params))
   where tags = variantEntryTags entry
         params = variantEntryParams entry

data SubmissionCondition =
  Simple SubmissionSimpleCondition
  | Neg SubmissionCondition
  | BBinary BBinaryOp SubmissionCondition SubmissionCondition
  deriving (Show, Eq)

data BBinaryOp = And | Or
  deriving (Show, Eq)

data SubmissionSimpleCondition =
  Existence ValueHolder
  | SBinary Atom SBinaryOp Atom
  deriving (Show, Eq)

data SBinaryOp = Equal | NotEqual | Less | Greater | LessOrEqual | GreaterOrEqual
  deriving (Show, Eq)

data Atom =
  ReadFrom ValueHolder
  | IntegerAtom Integer
  | DoubleAtom Double
  | StringAtom Text
  deriving (Show, Eq)

data ValueHolder = ValueHolder Text
  deriving (Show, Eq)

type Parser = Parsec Void Text

spaceConsumer = L.space space1 empty empty

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

conditionP :: Parser SubmissionCondition
conditionP = makeExprParser bTerm bOperators

bTerm =
  (parens conditionP)
  <|>
  (Simple <$> simpleConditionP)

bOperators :: [[Operator Parser SubmissionCondition]]
bOperators =
  [ [Prefix (Neg <$ symbol "!") ]
  , [InfixL (BBinary And <$ symbol "&&")
    , InfixL (BBinary Or <$ symbol "||") ]
  ]

simpleConditionP :: Parser SubmissionSimpleCondition
simpleConditionP =
  (try (SBinary <$> valP <*> sopP <*> valP))
  <|>
  (Existence <$> valueHolderP)

sopP :: Parser SBinaryOp
sopP =
  (try (symbol "==" *> pure Equal))
  <|>
  symbol "=" *> pure Equal
  <|>
  symbol "!=" *> pure NotEqual
  <|>
  symbol "/=" *> pure NotEqual
  <|>
  (try (symbol "<>" *> pure NotEqual))
  <|>
  (try (symbol "<=" *> pure LessOrEqual))
  <|>
  (try (symbol ">=" *> pure GreaterOrEqual))
  <|>
  symbol "<" *> pure Less
  <|>
  symbol ">" *> pure Greater

valP :: Parser Atom
valP =
  (ReadFrom <$> valueHolderP)
  <|>
  (try $ lexeme $ DoubleAtom <$> L.signed spaceConsumer L.float)
  <|>
  (lexeme $ IntegerAtom <$> L.signed spaceConsumer L.decimal)
  <|>
  (StringAtom <$> literalP)

literalP :: Parser Text
literalP = quotedP '"' <|> quotedP '\''

quotedP :: Char -> Parser Text
quotedP q = lexeme (T.pack <$> ((char q) *> (many $ anySingleBut q) <* (char q)))

valueHolderP :: Parser ValueHolder
valueHolderP = lexeme $ ValueHolder <$> T.pack <$> ((:) <$> letterChar <*> many (alphaNumChar <|> char '-'))

parseCondition :: Text -> Maybe (SubmissionCondition)
parseCondition = parseMaybe (spaceConsumer *> conditionP)

checkCondition :: Maybe SubmissionCondition -> VariantEntry -> Bool
checkCondition mCondition = checkCondition' mCondition . entryToMap

checkCondition' :: Maybe SubmissionCondition -> VariantEntryMap -> Bool
checkCondition' Nothing _ = False
checkCondition' (Just condition) entry = checkCondition'' condition entry

checkCondition'' :: SubmissionCondition -> VariantEntryMap -> Bool
checkCondition'' (Simple simpleCondition) entry = checkSimpleCondition simpleCondition entry
checkCondition'' (Neg condition) entry = not (checkCondition'' (condition) entry)
checkCondition'' (BBinary op condA condB) entry =
  (rfy op) (checkCondition'' condA entry) (checkCondition'' condB entry)
  where rfy And = (&&)
        rfy Or = (||)

data Val = IntegerVal Integer | DoubleVal Double | StringVal Text | NoVal

checkSimpleCondition :: SubmissionSimpleCondition -> VariantEntryMap -> Bool
checkSimpleCondition (Existence (ValueHolder holder)) entry = Map.member holder entry
checkSimpleCondition (SBinary valA op valB) entry = rop (getValue valA entry) (getValue valB entry)
  where rop NoVal _ = False  -- no value always brings false value
        rop _ NoVal = False  -- therefore (x < y) is not equal to !(x >= y)

        rop (IntegerVal x) (IntegerVal y) = (rfy op) x y
        rop (IntegerVal x) (DoubleVal y) = (rfy op) (fromIntegral x) y
        rop (DoubleVal x) (IntegerVal y) = (rfy op) x (fromIntegral y)
        rop (DoubleVal x) (DoubleVal y) = (rfy op) x y
        rop (StringVal x) (StringVal y) = (rfy op) x y
        rop _ _ = False

        rfy :: Ord a => SBinaryOp -> a -> a -> Bool
        rfy Equal = (==)
        rfy NotEqual = (/=)
        rfy Less = (<)
        rfy Greater = (>)
        rfy LessOrEqual = (<=)
        rfy GreaterOrEqual = (>=)

getValue :: Atom -> VariantEntryMap -> Val
getValue (IntegerAtom i) _ = IntegerVal i
getValue (DoubleAtom d) _ = DoubleVal d
getValue (StringAtom t) _ = StringVal t
getValue (ReadFrom (ValueHolder holder)) entry = case Map.lookup holder entry of
  Just v ->
    let s = T.unpack v
    in case readMaybe s of
         Just i -> IntegerVal i
         Nothing -> case readMaybe s of
           Just d -> DoubleVal d
           Nothing -> StringVal v
  Nothing -> NoVal
