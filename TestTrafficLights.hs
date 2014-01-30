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
-- Simulation driver for trafficLights version 1
-- Simulation driver for trafficLights version 2
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
     run_controller1 trafficLights_input1

     separator
     putStrLn "Simulate the second version of the traffic light circuit"
     run_controller2 trafficLights_input2

     separator

------------------------------------------------------------------------
-- Simulation driver for trafficLights version 1

run_controller1 input = runAllInput input output
  where

-- Extract input signals
    reset = getbit input 0

-- The circuit to be simulated
    (g,a,r) = controller1 reset

-- Format the output
    output =
      [string "Input: reset = ", bit reset,
       string "  Output: g = ", bit g, 
       string " a = ", bit a, 
       string " r = ", bit r]

------------------------------------------------------------------------
-- Simulation driver for trafficLights version 2

run_controller2 input = runAllInput input output
  where

-- Extract input signals
    reset = getbit input 0
    walkRequest = getbit input 1

-- The circuit to be simulated
    (g,a,r,wait,walk,walkCount) = controller2 reset walkRequest

-- Format the output
    output =
      [string "Input: reset = ", bit reset,
       string " walkRequest = ", bit walkRequest,
       string "  Output: g = ", bit g, 
       string " a = ", bit a, 
       string " r = ", bit r,
       string " wait = ", bit wait, 
       string " walk = ", bit walk, 
       string " count = ", bindec 16 walkCount]

------------------------------------------------------------------------
-- Test data for all trafficLights versions

-- The first test runs for 11 cycles: 8 for the full cycle and 
-- 3 to test the reset. After the reset, another 8 cycles.
trafficLights_input1 =
  [[1],[0],[0],[0],[0],[0],[0],[0],[0],[0],[0],[0],[1],[0],[0],[0],
   [0],[0],[0],[0],[0]] 

-- This test first resets the circuit and then wait for two cycles 
-- before the first walkRequest. The second walkRequest happens 3
-- cycles after the end of the first complete circuit from the first
-- walkRequest. It is also tested what happens if random walkRequests
-- happen when it is already running the full circuit. Finally, 
-- the reset is tested in the middle of a full circuit.
trafficLights_input2 =
  [[1,0],[0,0],[0,0],[0,1],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],
   [0,0],[0,0],[0,0],[0,1],[0,1],[0,0],[0,1],[0,0],[0,1],[0,0],[0,0],
   [0,0],[0,1],[0,0],[0,0],[1,1],[0,0]] 

