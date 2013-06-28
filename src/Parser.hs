module Parser
  ( parseBF
  )
where

import Text.ParserCombinators.Parsec

import Control.Applicative hiding ((<|>),many)

import Types

succP, predP, incP, decP, putCharP, getCharP :: Parser Command
succP = char '>' >> return Succ
predP = char '<' >> return Pred
incP = char '+' >> return Inc
decP = char '-' >> return Dec
putCharP = char '.' >> return PutChar
getCharP = char ',' >> return GetChar

atom :: Parser Command
atom = succP <|> predP <|>
       incP <|> decP <|>
       getCharP <|> putCharP

whileP :: Parser Command
whileP = char '[' *> (While <$> bf) <* char ']'

bf :: Parser [Command]
bf = many (whileP <|> atom)

parseBF :: String -> Either ParseError [Command]
parseBF = parse bf "" . filter (flip elem "<>+-,.[]")
