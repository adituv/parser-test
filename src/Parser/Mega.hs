{-# LANGUAGE BangPatterns, RecordWildCards #-}
module Parser.Mega where

import AST
import Control.Applicative(many, (<|>))
import Control.Monad(void)
import Data.List(foldl')
import Data.Text(Text)
import Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L

type Parser = Parsec () Text

spaceConsumer :: Parser ()
spaceConsumer = L.space (void spaceChar) (fail "") (fail "")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

parseMessage :: Text -> Either String MessageSpec
parseMessage = showError . runParser messageSpec "..."
  where
    showError (Left err) = Left (show err)
    showError (Right res) = Right res

ident :: Parser String
ident = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

decimal :: Integral a => Parser a
decimal = fromInteger <$> integerValue
  where
    integerValue :: Parser Integer
    integerValue = read <$> many digitChar

messageSpec :: Parser MessageSpec
messageSpec = do
    lexeme (string "message")
    messageName <- lexeme ident
    (reservedFields, knownFields) <- messageBody
    eof
    pure MessageSpec{..}

messageBody :: Parser ([FieldId], [FieldSpec])
messageBody = recombine <$> 
    between (symbol "{") (symbol "}") (many (messageEntry <* symbol ";"))
  where
    -- NB: fmap on (,) applies to the second element
    recombine :: [MessageEntry] -> ([FieldId], [FieldSpec])
    recombine = fmap reverse . foldl' recombine' ([], [])

    recombine' (!rs, !fs) (Reserved fieldid) = (rs ++ fieldid, fs)
    recombine' (!rs, !fs) (Field fieldSpec) = (rs, fieldSpec:fs)

data MessageEntry = Reserved [FieldId] | Field FieldSpec deriving Show

messageEntry :: Parser MessageEntry
messageEntry =  Reserved <$> (try (symbol "reserved") *> reserved)
            <|> Field <$> field

reserved :: Parser [FieldId]
reserved =  fmap TagId . concat <$> lexeme idEntry `sepBy` symbol ","
        <|> fmap NameId <$> lexeme identString `sepBy` symbol ","
  where
    idEntry = do
        d1 <- lexeme decimal
        d2 <- tryParseRange
        case d2 of
            Nothing -> pure [d1]
            Just d2' -> pure (enumFromTo d1 d2')

    identString = char '\"' *> ident <* char '\"'
    tryParseRange =  Just <$> (symbol "to" *> lexeme decimal)
                 <|> pure Nothing

field :: Parser FieldSpec
field = do
    fieldModifier <- option None (Repeated <$ symbol "repeated")
    fieldType <- type'
    fieldName <- lexeme ident
    symbol "="
    fieldTag <- lexeme decimal
    pure FieldSpec{..}

type' :: Parser ProtoType
type' =  (TUInt32 <$ symbol "uint32")
     <|> (TBool <$ symbol "bool")
     <|> (TString <$ symbol "string")