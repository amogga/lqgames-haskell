module Main (main) where

import Numeric.LinearAlgebra

main :: IO ()
main = do
    let states = vector [6.5, 0.0, pi / 2.0 , 1.0, 1.5, 40, -(pi / 2.0), 0.1, 0.0, 22.0, 0.0, 2.0]
        input = vector [0,0,0,0,0,0]
    
    print (states, input)