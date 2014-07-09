{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.BuildFile (htf_thisModulesTests) where

import Cook.BuildFile

import Test.Framework
import qualified Data.Text as T

test_matchFilePattern :: IO ()
test_matchFilePattern =
    do assertBool (matchesFilePattern pattern1 "foo/bar.cabal")
       assertBool (not $ matchesFilePattern pattern1 "foo/baz.cabal")
       assertBool (matchesFilePattern pattern2 "foo/hellooooo.cabal")
       assertBool (not $ matchesFilePattern pattern2 "foo/hellooooo.cabal.xzy")
       assertBool (not $ matchesFilePattern pattern2 "foo/hellooooo.xzy")
       assertBool (matchesFilePattern pattern3 "foo/asdasdas")
    where
      Right pattern1 = parseFilePattern "foo/bar.cabal"
      Right pattern2 = parseFilePattern "foo/*.cabal"
      Right pattern3 = parseFilePattern "foo/*"

test_parseBuildFile :: IO ()
test_parseBuildFile =
    do assertEqual "foo.docker" (bf_dockerFile parsed1)
       assertEqual "master.docker" (bf_dockerFile parsed2)
       assertEqual (Just (BuildFileId "foo.build")) (bf_base parsed2)
       assertEqual parsed2 parsed3
    where
      Right parsed1 = parseBuildFileText "sample1" sampleFile1
      Right parsed2 = parseBuildFileText "sample2" sampleFile2
      Right parsed3 = parseBuildFileText "sample2" sampleFile3

      sampleFile1 =
          "DOCKER foo.docker"
      sampleFile2 =
          T.concat
          [ "BASE foo.build\n"
          , "DOCKER master.docker\n"
          , "INCLUDE server/foo.cabal\n"
          , "INCLUDE server/*.js"
          ]
      sampleFile3 =
          T.concat
          [ "INCLUDE server/foo.cabal\n"
          , "BASE foo.build\n"
          , "INCLUDE server/*.js\n"
          , "DOCKER master.docker"
          ]
