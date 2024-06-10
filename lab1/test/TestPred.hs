{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad
import Dibujo
import Pred
import System.Exit (exitFailure)
import Test.HUnit

-- Test for cambiar function
testCambia :: Test
testCambia = TestCase $ do
  let dib = Figura 1
  let pred = (> 0)
  let f = const (Figura 0)
  let result = cambiar pred f dib
  assertEqual "cambiar should replace the figure" (Figura 0) result

-- Test for anyDib function
testAnyDib :: Test
testAnyDib = TestCase $ do
  let dib = Figura 1
  let pred = (> 0)
  let result = anyDib pred dib
  assertBool "anyDib should return True if any figure satisfies the predicate" result

-- Test for allDib function
testAllDib :: Test
testAllDib = TestCase $ do
  let dib = Figura 1
  let pred = (> 0)
  let result = allDib pred dib
  assertBool "allDib should return True if all figures satisfy the predicate" result

-- Test for andP function
testAndP :: Test
testAndP = TestCase $ do
  let pred1 = (> 0)
  let pred2 = (< 2)
  let result = andP pred1 pred2 1
  assertBool "andP should return True if both predicates are satisfied" result

-- Test for orP function
testOrP :: Test
testOrP = TestCase $ do
  let pred1 = (> 0)
  let pred2 = (< 0)
  let result = orP pred1 pred2 1
  assertBool "orP should return True if either predicate is satisfied" result

-- Test for falla function
testFalla :: Test
testFalla = TestCase $ do
  assertBool "falla should always return True" falla

-- Combine all tests
allTests :: Test
allTests = TestList [testCambia, testAnyDib, testAllDib, testAndP, testOrP, testFalla]

-- Run all tests
main :: IO ()
main = do
  Counts _ _ errors failures <- runTestTT allTests
  when (errors + failures > 0) $ exitFailure