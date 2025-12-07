module Main (main) where

import Simulation

main :: IO ()
main = do
    -- run simulation iterations
    let iters = runSimulationWithIterationAndHorizonE simParamsHorizon initState initInput

    -- compute costs per iteration
    let costs = map totalCostsForPlayersPerIterationE iters
    print $ last costs
    
    -- Create animation
    createAnimationOfEquilibriumE $ last iters