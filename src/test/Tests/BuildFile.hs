{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.BuildFile (htf_thisModulesTests) where

import Cook.BuildFile
import Cook.Extensions
import Cook.Types

import Path
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
       assertBool (not $ matchesFilePattern pattern2 "test/foo/hellooooo.xzy")
       assertBool (matchesFilePattern pattern3 "foo/asdasdas")
       assertBool (matchesFilePattern pattern3 "foo/bar/asdasdas")
    where
      Right pattern1 = parseFilePattern "foo/bar.cabal"
      Right pattern2 = parseFilePattern "foo/*.cabal"
      Right pattern3 = parseFilePattern "foo/*"

test_parseBuildFile :: IO ()
test_parseBuildFile =
    do parsed1 <- parseBuildFileText "sample1" [] sampleFile1 >>= assertRight
       parsed2 <- parseBuildFileText "sample1" [] sampleFile2 >>= assertRight
       parsed3 <- parseBuildFileText "sample3" [] sampleFile3 >>= assertRight
       parsed4 <- parseBuildFileText "sample3" [] sampleFile4 >>= assertRight
       parsed5 <- parseBuildFile "test/parsefail02.cook" [] >>= assertRight
       assertEqual (BuildBaseDocker $ DockerImage "ubuntu:14.04") (bf_base parsed1)
       assertEqual parsed1 parsed2
       assertEqual (BuildBaseCook $ BuildFileId "foo.build") (bf_base parsed3)
       assertEqual commands (bf_dockerCommands parsed3)
       assertEqual parsed3 parsed4
       assertEqual 10 (V.length (bf_include parsed5))
       assertEqual (Just "/DociData") (bf_unpackTarget parsed5)
    where
      commands =
          V.fromList [ Right $ DockerCommand "RUN" "apt-get -y install node"
                     , Right $ DockerCommand "ADD" ". /foo"
                     ]
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

test_parseCookVar :: IO ()
test_parseCookVar =
    do parsed1 <- parseBuildFileText "sample1" [] sampleFile1 >>= assertRight
       assertEqual expected1 parsed1
       parsed2 <- parseBuildFileText "sample2" [] sampleFile2 >>= assertRight
       assertEqual expected2 parsed2
    where
      expected2 =
          expected1
          { bf_requiredVars = V.fromList [("BUILD_MODE", Just "fast")]
          , bf_name = BuildFileId "sample2"
          }
      expected1 =
          (emptyBuildFile (BuildFileId "sample1") (BuildBaseCook $ BuildFileId "foo.bar"))
          { bf_requiredVars = V.fromList [("BUILD_MODE", Nothing)]
          , bf_dockerCommands =
              V.fromList
              [Right $ DockerCommand "RUN" "echo $BUILD_MODE"]
          }
      sampleFile1 =
          T.unlines
          [ "BASE COOK foo.bar"
          , "COOKVAR BUILD_MODE"
          , "RUN echo $BUILD_MODE"
          ]
      sampleFile2 =
          T.unlines
          [ "BASE COOK foo.bar"
          , "COOKVAR BUILD_MODE fast"
          , "RUN echo $BUILD_MODE"
          ]

test_parseExtensions :: IO ()
test_parseExtensions =
    do parsed1 <- assertRight $ parseOnlyBuildFile exts1 sampleFile1
       assertEqual expected1 parsed1
       parsed2 <- assertRight $ parseOnlyBuildFile exts2 sampleFile2
       assertEqual expected2 parsed2
    where
      expected2 =
          [ BaseLine (BuildBaseCook (BuildFileId "foo.bar"))
          , ExtensionLine ext1 ["a", "b", "c"]
          , ExtensionLine ext2 []
          , DockerLine (DockerCommand "RUN" "echo 'bazinga'")
          ]
      expected1 =
          [ BaseLine (BuildBaseCook (BuildFileId "foo.bar"))
          , ExtensionLine ext1 ["a", "b", "c"]
          , DockerLine (DockerCommand "RUN" "echo foo")
          ]
      ext1 = Extension "SOME/EXT" $(mkAbsFile "/home/fooo")
      ext2 = Extension "OTHER/EXT" $(mkAbsFile "/home/bar")
      exts1 =
          [ ext1 ]
      exts2 =
          [ ext1, ext2 ]
      sampleFile1 =
          T.unlines
          [ "BASE COOK foo.bar"
          , "SOME/EXT a b c"
          , "RUN echo foo"
          ]
      sampleFile2 =
          T.unlines
          [ "BASE COOK foo.bar"
          , "SOME/EXT a b c"
          , "OTHER/EXT"
          , "RUN echo 'bazinga'"
          ]

test_parseBuildAdvanced :: IO ()
test_parseBuildAdvanced =
    do t1 <- parseBuildFile "test/parsefail01.cook" []
       _ <- assertRight t1
       return ()
