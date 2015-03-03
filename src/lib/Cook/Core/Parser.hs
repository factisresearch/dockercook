{-# LANGUAGE OverloadedStrings #-}
module Cook.Core.Parser where

import Cook.Core.Types
import Cook.Docker.Types

import Data.Char
import Data.Maybe
import Control.Applicative
import Control.Monad
import Text.Parsec hiding (optional, many, (<|>))
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T

type Parser = Parsec String ()

parseCookFile :: FilePath -> String -> Either String (CommandFile CCook)
parseCookFile fp content =
    case parse (parseCookCore <* eof) fp content of
      Left err -> Left (show err)
      Right file -> Right file

parseCookCore :: Parser (CommandFile CCook)
parseCookCore =
    do (CommandCall cmd args) <- parseCommandCall
       when (cmd /= Command "base") $
            fail "First command of a Cookfile must be BASE!"
       cookParent <-
           case args of
             (ty:dep:_) ->
                 case T.toLower ty of
                   "cook" ->
                       return $ CookParentCookfile (T.unpack dep)
                   "docker" ->
                       case parse (parseImageName <* eof) "Cookfile BASE" (T.unpack dep) of
                         Left err -> fail (show err)
                         Right ok -> return $ CookParentDockerImage ok
                   _ ->
                       fail $ "INVALID BASE type: " ++ (T.unpack ty)
             _ -> fail "Invalid BASE command! Missing type and dependency argument"
       cookCommands <-
           optional $
           do _ <- endOfLine
              many (parseCommandCall <* ((endOfLine *> return ()) <|> eof))
       _ <- optional spaces
       return $ CommandFile cookParent (fromMaybe [] cookCommands)

parseImageName :: Parser DockerImageName
parseImageName =
    do imageName <-
           many1 (noneOf ": \n\t#")
       tagName <-
           optional $
           do _ <- char ':'
              many1 (noneOf " \n\t#:")
       return $ DockerImageName (T.pack imageName) (fmap T.pack tagName)

skipComments :: Bool -> Parser ()
skipComments commentLine =
    do _ <- many (skp <?> "comment")
       return ()
    where
      skp =
          try $
          do spaces
             _ <- char '#'
             _ <- many1 (noneOf "\n\r")
             when commentLine spaces
             return ()

parseString :: Parser String
parseString =
    do _ <- char '"'
       strings <- many character
       _ <- char '"'
       return $ "\"" ++ concat strings ++ "\""
    where
      escape =
          do d <- char '\\'
             c <- oneOf "\\\"0nrvtbf"
             return [d, c]
      nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"
      character = fmap return nonEscape <|> escape

parseArg :: Parser String
parseArg =
    many1 (multiNewLineChar <|> noneOf " \n\r\t#")
    where
      multiNewLineChar =
          try $
          do _ <- char '\\'
             _ <- many (char '\t' <|> char ' ')
             _ <- (char '\n' <|> char '\r')
             return '\n'

parseCommandCall :: Parser (CommandCall CCook)
parseCommandCall =
    do skipComments True
       cmd <- parseCommand
       args <-
           optional $
           do _ <- many1 (char ' ')
              cmdArg `sepBy` many1 (char ' ')
       skipComments False
       return $ CommandCall cmd (map T.pack $ fromMaybe [] args)
    where
      cmdArg =
          try (parseString <?> "quoted argument")
          <|> (parseArg <?> "argument")

parseCommand :: Parser (Command CCook)
parseCommand =
    Command <$> (CI.mk . T.pack <$> many1 (satisfy isAlphaNum)) <?> "command"
