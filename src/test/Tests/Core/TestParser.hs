{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Core.TestParser (htf_thisModulesTests) where

import Cook.Core.Parser
import Cook.Core.Types
import Cook.Docker.Types

import Control.Applicative
import Test.Framework
import Text.Parsec (parse, eof)
import qualified Data.Text as T

testParser expect p i =
    do res <- assertRight $ parse (p <* eof) "test-input" i
       assertEqual expect res

testParserFail p i =
    do _ <- assertLeft $ parse (p <* eof) "test-input" i
       return ()

test_imageNameParser :: IO ()
test_imageNameParser =
    do let im x = DockerImageName x Nothing
           im' x y = DockerImageName x (Just y)
           doTest x y = testParser x parseImageName y
       subAssert (doTest (im "ubuntu") "ubuntu")
       subAssert (doTest (im "agrafix/ghc") "agrafix/ghc")
       subAssert (doTest (im "library/cook-234234234a") "library/cook-234234234a")
       subAssert (doTest (im' "ubuntu" "latest") "ubuntu:latest")
       subAssert (doTest (im' "ubuntu" "234") "ubuntu:234")
       subAssert (doTest (im' "ubuntu" "3.4.5") "ubuntu:3.4.5")
       subAssert (doTest (im' "foo/ubuntu" "3.4.5") "foo/ubuntu:3.4.5")
       subAssert (testParserFail parseImageName "asfasdf:3422:asd")
       subAssert (testParserFail parseImageName "asfasdf:3422:asd ")
       subAssert (testParserFail parseImageName "asfasdf:3422:asd\t")
       subAssert (testParserFail parseImageName "asfasdf:3422:asd#")
       subAssert (testParserFail parseImageName "")
       subAssert (testParserFail parseImageName " ")

test_parseCommand :: IO ()
test_parseCommand =
    do let doTest x y = testParser x parseCommand y
       subAssert $ doTest (Command "base") "base"
       subAssert $ doTest (Command "base") "bAse"
       subAssert $ doTest (Command "base") "BASE"
       subAssert $ testParserFail parseCommand ""
       subAssert $ testParserFail parseCommand " "
       subAssert $ testParserFail parseCommand "asd "
       subAssert $ testParserFail parseCommand "asd#"
       subAssert $ testParserFail parseCommand "a b"

test_parseCommandCall :: IO ()
test_parseCommandCall =
    do let doTest x y = testParser x parseCommandCall y
           cc x y = CommandCall (Command x) y
       subAssert $ doTest (cc "BASE" ["DOCKER", "ubuntu"]) "BASE DOCKER ubuntu"
       subAssert $ doTest (cc "RUN" ["apt-get", "update", "&&"
                                    , "apt-get", "install", "-y", "emacs"]) "RUN apt-get update && apt-get install -y emacs"
       subAssert $ doTest (cc "RUN" ["echo", "\"string test\""]) "RUN echo \"string test\""
       subAssert $ doTest (cc "RUN" ["apt-get", "update", "&&"
                                    , "\napt-get", "install", "foo"]) "RUN apt-get update && \\\napt-get install foo"
       subAssert $ doTest (cc "BASE" ["DOCKER", "ubuntu"]) "# comment\nBASE DOCKER ubuntu"
       subAssert $ doTest (cc "BASE" ["DOCKER", "ubuntu"]) "BASE DOCKER ubuntu\n#comment"
       subAssert $ doTest (cc "BASE" ["DOCKER", "ubuntu"]) "BASE DOCKER ubuntu# comment"
       subAssert $ testParserFail parseCommandCall ""
       subAssert $ testParserFail parseCommandCall " "

test_parseCookCore :: IO ()
test_parseCookCore =
    do let doTest x y = testParser x parseCookCore y
           cc x y = CommandFile x y
       subAssert $ doTest (cc (CookParentCookfile "cook/test.cook") []) "BASE COOK cook/test.cook\n"
       subAssert $ doTest (cc (CookParentDockerImage $ DockerImageName "ubuntu" Nothing) []) "BASE DOCKER ubuntu"
       subAssert $ doTest (cc (CookParentCookfile "cook/test.cook")
                                  [CommandCall (Command "RUN") ["echo"]]) "BASE COOK cook/test.cook\nRUN echo"
       subAssert $ testParserFail parseCookCore ""
       subAssert $ testParserFail parseCookCore " "
       subAssert $ testParserFail parseCookCore "BASE"
       subAssert $ testParserFail parseCookCore "BASE !"
       subAssert $ testParserFail parseCookCore "BASE asd"
       subAssert $ testParserFail parseCookCore "BASE COOK"
       subAssert $ testParserFail parseCookCore "BASE DOCKER ::::::"
