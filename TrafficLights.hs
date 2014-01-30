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
trafficLights1 reset = (g,a,r)
  where
    counter = countern 4 one rst
    g = muxn 4 counter [one,  one,  one,  zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero]
    a = muxn 4 counter [zero, zero, zero, one,  zero, zero, zero, zero, one,  zero, zero, zero, zero, zero, zero, zero]
    r = muxn 4 counter [zero, zero, zero, zero, one,  one,  one,  one,  zero, zero, zero, zero, zero, zero, zero, zero]
    rst = or2 reset (andw counter)

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

------------------------------------------------------------------------
-- n-bit counter block generator

countern :: Clocked a => Int -> a -> a -> [a]
countern 0 _ _ = []
countern n cin reset = (countern (n-1) cout reset) ++ [x]
  where
    (x,cout) = counter1 cin reset

temp :: [a] -> (a,a,a)
temp (x:(y:(z:xs))) = (x,y,z)

------------------------------------------------------------------------
-- n-bit mux block generator

muxn :: Clocked a => Int -> [a] -> [a] -> a
muxn 1 (sel:[]) (x:(y:[])) = mux1 sel x y
muxn n (sel:sels) input = mux1 sel x y
  where
    x = muxn (n-1) sels inputlo
    y = muxn (n-1) sels inputho
    (inputlo,inputho) = splitAt ((length input) `div`2) input

