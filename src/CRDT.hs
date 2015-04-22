{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-
 hsCRDT.hs

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
-- @Date:   2015-04-22 11:16:32
-- @Last Modified by:   Sebastien Soudan
-- @Last Modified time: 2015-04-22 15:05:36

module CRDT
    where

----------------------------
-- Join semi-Lattice related definitions
----------------------------

class Compare p where  -- aka Ord?
    is :: p -> p -> Bool -- aka <=

class (Compare p) => JoinSemiLattice p where
    join :: p -> p -> p


----------------------------
-- CRDT related definitions - state-based specification
----------------------------
class Payload p where
    initial :: p

class (Eq a, Payload p) => Query p q a where
    -- qpre :: p -> Bool -- Embedded in the definition of qlet
    qlet :: p -> q -> Maybe a

class Update p u where
    -- upre :: p -> Bool
    ulet :: p -> u -> Maybe p

class Merge p where
    merge :: p -> p -> p -- LUB merge


----------------------------
-- CvRDT definition
----------------------------
class (Payload p, {- Query p a, -} Update p u, Compare p, Merge p, JoinSemiLattice p) => CvRDT p u where


update :: forall p u. (CvRDT p u) => p -> u -> Maybe p
update s u = ulet s u
            
query :: forall p q a. (Query p q a) => p -> q -> Maybe a
query = qlet

