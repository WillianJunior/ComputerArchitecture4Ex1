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
    rst = or2 reset (andw ([x3]++[inv x2]++[inv x1]++[inv x0]))
    (x3:(x2:(x1:(x0:nothing)))) = counter

------------------------------------------------------------------------
-- 1-bit counter block

counter1 :: Clocked a => a -> a -> (a,a)
counter1 cin reset = (out,cout)
  where
    (cout,sum) = halfAdd out cin
    out = dff inp
    inp = and2 (inv reset) sum -- how to to the reset?

------------------------------------------------------------------------
-- n-bit counter block generator

countern :: Clocked a => Int -> a -> a -> [a]
countern 0 _ _ = []
countern n cin reset = (countern (n-1) cout reset) ++ [x]
  where
    (x,cout) = counter1 cin reset

------------------------------------------------------------------------
-- n-bit mux block generator

muxn :: Clocked a => Int -> [a] -> [a] -> a
muxn 1 (sel:[]) (x:(y:[])) = mux1 sel x y
muxn n (sel:sels) input = mux1 sel x y
  where
    x = muxn (n-1) sels inputlo
    y = muxn (n-1) sels inputho
    (inputlo,inputho) = splitAt ((length input) `div`2) input

