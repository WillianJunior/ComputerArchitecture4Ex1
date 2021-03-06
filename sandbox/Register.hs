------------------------------------------------------------------------
-- Hydra: A functional digital circuit design language
-- Example: Register
------------------------------------------------------------------------

module Register where

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.StandardCircuits

------------------------------------------------------------------------
-- 4-bit register

reg4 :: Clocked a => a -> [a] -> [a]
reg4 ld [x0,x1,x2,x3] =
  [reg1 ld x0, reg1 ld x1, reg1 ld x2, reg1 ld x3]

------------------------------------------------------------------------
-- Register file with 4 registers, each containing 4 bits

rf4
  :: Clocked a
  => a          -- ld, a control signal bit
  -> [a]        -- x, a 4-bit data input word
  -> (a,a)      -- d, a 2-bit register address
  -> (a,a)      -- sa, a 2-bit register address
  -> (a,a)      -- sb, a 2-bit register address
  -> ([a],[a])  -- output = (reg[sa], reg[sb])

rf4 ld x d sa sb = (abus,bbus)
  where
    r0 = reg4 ld0 x
    r1 = reg4 ld1 x
    r2 = reg4 ld2 x
    r3 = reg4 ld3 x
    (ld0,ld1,ld2,ld3) = demux2 d ld
    abus = mux2w sa r0 r1 r2 r3
    bbus = mux2w sb r0 r1 r2 r3
