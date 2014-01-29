------------------------------------------------------------------------
-- Hydra: A functional digital circuit design language
-- Exercise 1: TrafficLights
------------------------------------------------------------------------

module TrafficLights where

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.StandardCircuits

------------------------------------------------------------------------
-- trafficLight version 1

trafficLights1 :: Clocked a => a -> (a,a,a)
trafficLights1 reset = (out,cout,sum)
  where
    (out,cout,sum) = counter3 zero

------------------------------------------------------------------------
-- 1-bit counter block

counter1 :: Clocked a => a -> a -> (a,a)
counter1 cin reset = (out,cout)
  where
    (cout,sum) = halfAdd out cin
    out = dff sum -- how to to the reset?

------------------------------------------------------------------------
-- 3-bit counter block

counter3 :: Clocked a => a -> (a,a,a)
counter3 reset = (x0,x1,x2)
  where
    (x0,c0) = counter1 one reset
    (x1,c1) = counter1 c0 reset
    (x2,c2) = counter1 c1 reset