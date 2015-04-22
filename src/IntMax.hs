{-# LANGUAGE MultiParamTypeClasses #-}
{-
 IntMax.hs

 Copyright 2015 Sebastien Soudan

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.

-}
-- @Author: Sebastien Soudan
-- @Date:   2015-04-22 14:15:06
-- @Last Modified by:   Sebastien Soudan
-- @Last Modified time: 2015-04-22 15:13:14

module IntMax
    where

import           CRDT


data IntMax = IntMax Int deriving (Show, Eq)

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
    -- qpre = const True
    qlet (IntMax a) _  = Just a

instance Update IntMax IntMax where
    ulet s u = Just $ join s u

instance Merge IntMax where
    merge = join

instance CvRDT IntMax IntMax where

