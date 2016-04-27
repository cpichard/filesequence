{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where
import Test.Framework
import Test.Framework.BlackBoxTest

import {-@ HTF_TESTS @-} FileSequenceTests
import {-@ HTF_TESTS @-} FrameListTests

main ::IO ()
main = htfMain htf_importedTests


