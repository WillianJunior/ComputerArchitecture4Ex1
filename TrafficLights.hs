------------------------------------------------------------------------
-- Hydra: A functional digital circuit design language
-- Exercise 1: TrafficLights
------------------------------------------------------------------------

module TrafficLights where

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.StandardCircuits

------------------------------------------------------------------------
-- trafficLight version 1

controller1 :: Clocked a => a -> (a,a,a)
controller1 reset = (g,a,r)
  where
    -- the mainCouter have the state of the traffic light
    counter = countern 4 one rst
    g = muxn 4 counter 
      [one,  one,  one,  zero, zero, zero, zero, zero, zero, 
       zero, zero, zero, zero, zero, zero, zero]
    a = muxn 4 counter 
      [zero, zero, zero, one,  zero, zero, zero, zero, one,  
       zero, zero, zero, zero, zero, zero, zero]
    r = muxn 4 counter 
      [zero, zero, zero, zero, one,  one,  one,  one,  zero, 
       zero, zero, zero, zero, zero, zero, zero]
    x3:(x2:(x1:(x0:nothing))) = counter
    
    -- return the mainCounter to its initial state after a 
    -- full cycle (1000 or on the second time on amber)
    rst = or2 reset (andw ([x3]++[inv x2]++[inv x1]++[inv x0]))

------------------------------------------------------------------------
-- trafficLight version 2

controller2 :: Clocked a => a -> a -> (a,a,a,a,a,[a])
controller2 reset walkRequest = (g,a,r,wait,walk,walkCount)
  where
    -- the mainCouter have the state of the traffic light
    mainCounter = counternen 3 one mainEnable rst
    g    = muxn 3 mainCounter [one,  zero, zero, zero, zero, zero, 
                               zero, zero]
    a    = muxn 3 mainCounter [zero, one,  zero, zero, zero, one,  
                               zero, zero]
    r    = muxn 3 mainCounter [zero, zero, one,  one,  one,  zero, 
                               zero, zero]
    walk = muxn 3 mainCounter [zero, zero, one,  one,  one,  zero, 
                               zero, zero]
    wait = muxn 3 mainCounter [one,  one,  zero, zero, zero, one,  
                               zero, zero]
    x2:(x1:(x0:nothing)) = mainCounter

    -- return the mainCounter to its initial state after a full 
    -- cycle (101 or on the second time on amber)
    rst = or2 reset (andw ([x2]++[inv x1]++[x0]))

    -- enable state for the mainCounter
    mainEnable = reg1 regMux (inv mainEnable)
    regMux = or2  (andw ([walkRequest]++[inv x2]++[inv x1]++
                        [inv x0]++[inv mainEnable])) 
                  (andw ([x2]++[inv x1]++[x0]))

    -- walkCounter
    walkCount = counternen 16 one walkEnable reset

    -- enable state for the walkCounter
    -- also uses 'inv mainEnable' to prevent the walkRequest = 1 for 
    -- more than one cycle
    walkEnable = or2  (andw ([walkRequest]++[inv x2]++[inv x1]++
                            [inv x0]++[inv mainEnable])) 
                      reset
    
------------------------------------------------------------------------
-- 1-bit counter block
------------------------------------------------------------------------
-- This building block was implemented before the counter1en.
-- This can also be implemented as:
-- counter1 = counter1en one

counter1 :: Clocked a => a -> a -> (a,a)
counter1 cin reset = (out,cout)
  where
    (cout,sum) = halfAdd out cin
    out = dff inp
    inp = and2 (inv reset) sum

------------------------------------------------------------------------
-- 1-bit counter block with enable

counter1en :: Clocked a => a -> a -> a -> (a,a)
counter1en enable cin reset = (out,cout)
  where
    (cout,sum) = halfAdd out cin
    out = reg1 enable inp
    inp = and2 (inv reset) sum

------------------------------------------------------------------------
-- n-bit counter block generator
------------------------------------------------------------------------
-- Likewise on counter1, this building block was implemented 
-- before the counternen.
-- This can also be implemented as:
-- countern = (\n cin reset -> counternen n cin one reset)

countern :: Clocked a => Int -> a -> a -> [a]
countern 0 _ _ = []
countern n cin reset = (countern (n-1) cout reset) ++ [x]
  where
    (x,cout) = counter1 cin reset

------------------------------------------------------------------------
-- n-bit counter block with enable generator
------------------------------------------------------------------------
-- In order to use this block generator you must input one as cin:
-- counter n enable reset = 
--       (\n enable reset -> counternen n one enable reset)
-- If you input zero nothing will happen. EVER!!!

counternen :: Clocked a => Int -> a -> a -> a -> [a]
counternen 0 _ _ _ = []
counternen n cin enable reset = (counternen (n-1) cout enable reset) ++ [x]
  where
    (x,cout) = counter1en enable cin reset

------------------------------------------------------------------------
-- n-bit mux block generator

muxn :: Clocked a => Int -> [a] -> [a] -> a
muxn 1 (sel:[]) (x:(y:[])) = mux1 sel x y
muxn n (sel:sels) input = mux1 sel x y
  where
    x = muxn (n-1) sels inputlo
    y = muxn (n-1) sels inputho
    (inputlo,inputho) = splitAt ((length input) `div`2) input

