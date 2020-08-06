-- determineTheType.hs

{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

exampleA = (* 9) 6

exampleB = head [(0, "doge"), (1, "kitteh")]

exampleC = head [(0 :: Integer, "doge"), (1, "kitteh")]

exampleD = if False then True else False

exampleE = length [1, 2, 3, 4, 5]

exampleF = (length [1, 2, 3, 4]) > (length "TACOCAT")

