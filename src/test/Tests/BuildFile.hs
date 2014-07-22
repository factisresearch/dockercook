{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.BuildFile (htf_thisModulesTests) where

import Cook.BuildFile
import Cook.Types

import Test.Framework
import qualified Data.Vector as V
import qualified Data.Text as T

test_matchFilePattern :: IO ()
test_matchFilePattern =
    do assertBool (matchesFilePattern pattern1 "foo/bar.cabal")
       assertBool (not $ matchesFilePattern pattern1 "foo/baz.cabal")
       assertBool (matchesFilePattern pattern2 "foo/hellooooo.cabal")
       assertBool (not $ matchesFilePattern pattern2 "foo/hellooooo.cabal.xzy")
       assertBool (not $ matchesFilePattern pattern2 "foo/hellooooo.xzy")
       assertBool (matchesFilePattern pattern3 "foo/asdasdas")
       assertBool (matchesFilePattern pattern3 "foo/bar/asdasdas")
    where
      Right pattern1 = parseFilePattern "foo/bar.cabal"
      Right pattern2 = parseFilePattern "foo/*.cabal"
      Right pattern3 = parseFilePattern "foo/*"

test_parseBuildFile :: IO ()
test_parseBuildFile =
    do assertEqual (BuildBaseDocker $ DockerImage "ubuntu:14.04") (bf_base parsed1)
       assertEqual parsed1 parsed2
       assertEqual (BuildBaseCook $ BuildFileId "foo.build") (bf_base parsed3)
       assertEqual commands (bf_dockerCommands parsed3)
       assertEqual parsed3 parsed4
    where
      commands =
          V.fromList [ DockerCommand "RUN" "apt-get -y install node"
                     , DockerCommand "ADD" ". /foo"
                     ]
      Right parsed1 = parseBuildFileText "sample1" sampleFile1
      Right parsed2 = parseBuildFileText "sample1" sampleFile2
      Right parsed3 = parseBuildFileText "sample3" sampleFile3
      Right parsed4 = parseBuildFileText "sample3" sampleFile4

      sampleFile1 =
          "BASE DOCKER ubuntu:14.04"
      sampleFile2 =
          "BASE DOCKER ubuntu:14.04#asdf"
      sampleFile3 =
          T.concat
          [ "BASE COOK foo.build\n"
          , "INCLUDE server/foo.cabal\n"
          , "INCLUDE server/*.js\n"
          , "RUN apt-get -y install node\n"
          , "ADD . /foo"
          ]
      sampleFile4 =
          T.concat
          [ "# Comment!\n"
          , "#Comment\n"
          , "  # Comment\n"
          , "BASE COOK foo.build\n\n"
          , "INCLUDE server/foo.cabal # comment\n"
          , "INCLUDE server/*.js\n"
          , "RUN apt-get -y install node # comment\n"
          , "ADD . /foo\n"
          , "# comment\n"
          , "\n"
          ]
