{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import {-@ HTF_TESTS @-} Tests.BuildFile
import {-@ HTF_TESTS @-} Tests.DirectDocker

main :: IO ()
main = htfMain htf_importedTests
