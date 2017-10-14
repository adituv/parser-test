{-# LANGUAGE BangPatterns, OverloadedStrings, RecordWildCards #-}
module Parser.Atto where

import AST
import Control.Applicative((<|>))
import Data.Attoparsec.Text
import Data.List(foldl')
import Data.Text(Text)

parseMessage :: Text -> Either String MessageSpec
parseMessage = parseOnly messageSpec

messageSpec :: Parser MessageSpec
messageSpec = do
    messageName <- messageHeader
    skipSpace
    (reservedFields, knownFields) <- messageBody
    skipSpace
    endOfInput
    pure MessageSpec{..}

ident :: Parser String
ident = (:) <$> letter <*> (many' $ letter <|> digit <|> char '_')

messageHeader :: Parser String
messageHeader = "message" *> skipSpace *> ident

messageBody :: Parser ([FieldId], [FieldSpec])
messageBody = do
    skipSpace
    char '{'
    skipSpace
    entries <- many' (messageEntry <* skipSpace)
    char '}'
    pure $ recombine entries
  where
    -- NB: fmap on (,) applies to the second element
    recombine :: [MessageEntry] -> ([FieldId], [FieldSpec])
    recombine = fmap reverse . foldl' recombine' ([], [])

    recombine' (!rs, !fs) (Reserved fieldid) = (rs ++ fieldid, fs)
    recombine' (!rs, !fs) (Field fieldSpec) = (rs, fieldSpec:fs)

data MessageEntry = Reserved [FieldId] | Field FieldSpec deriving Show

messageEntry :: Parser MessageEntry
messageEntry = ((Reserved <$> ("reserved" *> skipSpace *> reserved))
               <|> (Field <$> field))
            <* skipSpace <* char ';'

reserved :: Parser [FieldId]
reserved = 
      (fmap TagId . concat <$>
        idEntry `sepBy` (skipSpace *> char ',' *> skipSpace))
  <|> (fmap NameId <$>
        nameEntry `sepBy` (skipSpace *> char ',' *> skipSpace))
  where
      idEntry =  (pure <$> decimal)
               <|> (enumFromTo <$> decimal
                               <* skipSpace <* "to" <* skipSpace
                               <*> decimal)
      nameEntry = char '\"' *> ident <* char '\"'

field :: Parser FieldSpec
field = do
    fieldModifier <- option None (Repeated <$ "repeated")
    skipSpace
    fieldType <- type'
    skipSpace
    fieldName <- ident
    skipSpace
    char '='
    skipSpace
    fieldTag <- decimal
    pure FieldSpec{..}

type' :: Parser ProtoType
type' =  (TUInt32 <$ "uint32")
     <|> (TBool <$ "bool")
     <|> (TString <$ "string")

