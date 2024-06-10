{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Exception()
import Control.Monad (when)
import Dibujo
import System.Exit (exitFailure)
import Test.HUnit

-- Test data
fig1 :: Dibujo String
fig1 = Figura "fig1"

fig2 :: Dibujo String
fig2 = Figura "fig2"

-- Test cases
testComp :: Test
testComp = TestCase (assertEqual "for: comp 3 (+1) 0" 3 (comp 3 (+ 1) 0))

testFigura :: Test
testFigura = TestCase (assertEqual "for: figura \"fig1\"" (Figura "fig1") (figura "fig1")) -- This will fail

testEncimar :: Test
testEncimar = TestCase (assertEqual "for: encimar fig1 fig2" (Encimar fig1 fig2) (encimar fig1 fig2))

testApilar :: Test
testApilar = TestCase (assertEqual "for: apilar 0.5 0.5 fig1 fig2" (Apilar 0.5 0.5 fig1 fig2) (apilar 0.5 0.5 fig1 fig2))

testJuntar :: Test
testJuntar = TestCase (assertEqual "for: juntar 0.5 0.5 fig1 fig2" (Juntar 0.5 0.5 fig1 fig2) (juntar 0.5 0.5 fig1 fig2))

testRotar :: Test
testRotar = TestCase (assertEqual "for: rotar fig1" (Rotar fig1) (rotar fig1))

testEspejar :: Test
testEspejar = TestCase (assertEqual "for: espejar fig1" (Espejar fig1) (espejar fig1))

testFiguras :: Test
testFiguras = TestCase (assertEqual "for: figuras (encimar fig1 fig2)" ["fig1", "fig2"] (figuras (encimar fig1 fig2)))

-- Test list
tests :: Test
tests = TestList [testComp, testFigura, testEncimar, testApilar, testJuntar, testRotar, testEspejar, testFiguras]

main :: IO ()
main = do
  Counts _ _ errors failures <- runTestTT tests
  when (errors + failures > 0) $ exitFailure