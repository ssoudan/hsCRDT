-- @Author: Sebastien Soudan
-- @Date:   2015-04-22 17:43:52
-- @Last Modified by:   Sebastien Soudan
-- @Last Modified time: 2015-04-22 17:44:46

module MonotonicCounter
    where

import           CRDT


data MonotonicCounter = MonotonicCounter [Int] deriving (Show, Eq)

----------------------------
-- Join semi-Lattice related definitions
----------------------------
instance Compare IntMax where
    is (IntMax a) (IntMax b) = a <= b

instance JoinSemiLattice IntMax where
    join (IntMax a) (IntMax b) = IntMax (max a b)


----------------------------
-- CvRDT related definitions
----------------------------
instance Payload IntMax where
    initial = IntMax 0

instance Query IntMax () Int where
    qlet (IntMax a) _  = Just a

instance Update IntMax IntMax where
    ulet s u = Just $ join s u

instance Merge IntMax where
    merge = join

instance CvRDT IntMax IntMax where