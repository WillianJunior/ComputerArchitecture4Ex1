------------------------------------------------------------------------
-- Hydra: A functional digital circuit design language
-- Exercise 1
------------------------------------------------------------------------

-- Run the tests for the traffic light circuit (both versions).
-- To run the simulations, enter the following:

--    ghci
--    :load RunAdd
--    main

-- The file contains:

-- Module definition and imports
-- Main program
-- Simulation driver for half adder
-- Simulation driver for word adder
-- Test data

------------------------------------------------------------------------
-- Module definition and imports

module Main where

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.StandardCircuits
import TrafficLights

------------------------------------------------------------------------
-- Main program

-- Print a line to separate output from different simulations
separator :: IO ()
separator = putStrLn (take 76 (repeat '-'))

-- The main program runs a sequence of simulations

main :: IO ()
main =
  do separator
     putStrLn "Simulate the first version of the traffic light circuit"
     run_trafficLights trafficLights_input1

     separator

------------------------------------------------------------------------
-- Simulation driver for trafficLights version 1

run_trafficLights input = runAllInput input output
  where

-- Extract input signals
    reset = getbit input 0

-- The circuit to be simulated
    (g,a,r) = trafficLights1 reset

-- Format the output
    output =
      [string "Input: reset = ", bit reset,
       string "  Output: g = ", bit g, 
       string " a = ", bit a, 
       string " r = ", bit r]

------------------------------------------------------------------------
-- Test data

-- Test data for all trafficLights versions consists of one bit per cycle

-- the first test runs for 9 cycles: 8 for the full cycle and 
-- 1 to return to initial state
trafficLights_input1 =
  [[1],[0],[0],[0],[0],[1],[0],[0],[0],[0]]

