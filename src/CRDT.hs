{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
-- @Last Modified time: 2015-04-24 14:23:22

module CRDT
    where

import           Data.Monoid

----------------------------
-- Join semi-Lattice related definitions
----------------------------

class Compare p where  
    is :: p -> p -> Bool -- (is p p') is True if p <= p' 

class (Compare p) => JoinSemiLattice p where
    lub :: p -> p -> p -- least-upper-bound

----------------------------
-- CRDT related definitions - state-based specification
----------------------------
class Payload p where
    initial :: p

class (Eq a, Payload p) => Query p q a where
    query :: p -> q -> Maybe a

----------------------------
-- CvRDT definition
----------------------------
class (Payload p, Compare p, JoinSemiLattice p) => CvRDT p u where
        update :: p -> u -> Maybe p

instance (CvRDT p u) => Monoid p where
    mempty = initial
    mappend = lub

-- merge is an alias for 'lub'
merge :: (JoinSemiLattice p) => p -> p -> p
merge = lub
