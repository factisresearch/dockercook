{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import {-@ HTF_TESTS @-} Tests.Core.TestParser
import {-@ HTF_TESTS @-} Tests.BuildFile

main :: IO ()
main = htfMain htf_importedTests
