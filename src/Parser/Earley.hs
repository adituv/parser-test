{-# LANGUAGE ApplicativeDo, BangPatterns, RecordWildCards #-}
module Parser.Earley where

import AST
import Control.Applicative
import Data.Char
import Data.List(foldl')
import Data.ListLike.Text() -- for the orphan instance
import Data.Text(Text)
import Text.Earley hiding(symbol)

parseMessage :: Text -> Either String MessageSpec
parseMessage input = case fullParses (parser messageSpec') input of
    (p:_, _) -> Right p
    ([], r) -> Left (show r)
  where
    messageSpec' = rule messageSpec

-- Abbreviated synonym for our productions and grammars
type Prod' r = Prod r String Char
type Grammar' r a = Grammar r (Prod' r a)

skipSpace :: Prod' r ()
skipSpace = many (satisfy isSpace) *> pure ()

lexeme :: Prod' r a -> Prod' r a
lexeme g = g <* skipSpace

symbol :: String -> Prod' r String
symbol s = list s <* skipSpace

ident :: Prod' r String
ident = (:) <$> satisfy (liftA2 (&&) isAscii isAlpha)
            <*> (many $ satisfy (liftA2 (&&) isAscii isAlphaNum))

decimal :: Integral a => Prod' r a
decimal = fromInteger <$> integerVal
  where
    integerVal :: Prod' r Integer
    integerVal = read <$> many (satisfy (liftA2 (&&) isAscii isDigit))

messageSpec :: Prod' r MessageSpec
messageSpec = do
    -- _ <- is workaround for "no instance for monad" when do
    -- is applicative
    _ <- symbol "message"
    messageName <- lexeme ident
    ~(reservedFields, knownFields) <- messageBody
    pure MessageSpec{..}

messageBody :: Prod' r ([FieldId], [FieldSpec])
messageBody = do
    _ <- symbol "{"
    entries <- many messageEntry
    _ <- symbol "}"
    pure $ recombine entries
  where
    -- NB: fmap on (,) applies to the second element
    recombine :: [MessageEntry] -> ([FieldId], [FieldSpec])
    recombine = fmap reverse . foldl' recombine' ([], [])

    recombine' (!rs, !fs) (Reserved fieldid) = (rs ++ fieldid, fs)
    recombine' (!rs, !fs) (Field fieldSpec) = (rs, fieldSpec:fs)


data MessageEntry = Reserved [FieldId] | Field FieldSpec deriving Show

messageEntry :: Prod' r MessageEntry
messageEntry = (Reserved <$> (symbol "reserved" *> reserved)
            <|> Field <$> field)
            <*  symbol ";"

reserved :: Prod' r [FieldId]
reserved = fmap concat . (:) <$> fieldIds <*> many (symbol "," *> fieldIds)
  where
    fieldIds :: Prod' r [FieldId]
    fieldIds = fmap TagId <$>
                  (enumFromTo <$> lexeme decimal
                             <*> (symbol "to" *> lexeme decimal))
            <|> pure . TagId <$> lexeme decimal
            <|> pure . NameId <$> lexeme identString
    identString = token '\"' *> ident <* token '\"'

field :: Prod' r FieldSpec
field = do
    fieldModifier <- modifierFromMaybe <$> optional (symbol "repeated")
    fieldType <- type'
    fieldName <- lexeme ident
    _ <- symbol "="
    fieldTag <- lexeme decimal
    pure FieldSpec{..}

modifierFromMaybe :: Maybe a -> FieldModifier
modifierFromMaybe (Just _) = Repeated
modifierFromMaybe Nothing = None

type' :: Prod' r ProtoType
type' =  (TUInt32 <$ symbol "uint32")
     <|> (TBool <$ symbol "bool")
     <|> (TString <$ symbol "string")