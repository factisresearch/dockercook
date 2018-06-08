{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import {-@ HTF_TESTS @-} Tests.BuildFile
import {-@ HTF_TESTS @-} Tests.Docker.API

main :: IO ()
main = htfMain htf_importedTests
