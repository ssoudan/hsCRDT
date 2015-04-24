{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-
 MonotonicCounter.hs

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
-- @Date:   2015-04-22 17:43:52
-- @Last Modified by:   Sebastien Soudan
-- @Last Modified time: 2015-04-24 14:22:29

module MonotonicCounter
    where

import           CRDT
import           Data.Array.IArray as I
import           Data.Sized.Fin
import           Data.Sized.Matrix
import           GHC.TypeLits


data MonotonicCounter (ix :: Nat) = MonotonicCounter (Vector ix Int) deriving (Show, Eq)

data CounterUpdate = Increment Int deriving (Show, Eq)

toMonotonicCounter :: (KnownNat ix) => [Int] -> MonotonicCounter ix
toMonotonicCounter xs = MonotonicCounter $ matrix xs

----------------------------
-- Join semi-Lattice related definitions
----------------------------
instance (KnownNat ix) => Compare (MonotonicCounter (ix :: Nat)) where
    is (MonotonicCounter xs) (MonotonicCounter ys) = xs <= ys

instance (KnownNat ix) => JoinSemiLattice (MonotonicCounter ix) where
    lub (MonotonicCounter xs) (MonotonicCounter ys) = MonotonicCounter $ Data.Sized.Matrix.zipWith max xs ys

----------------------------
-- CvRDT related definitions
----------------------------
instance (KnownNat ix) => Payload (MonotonicCounter ix) where
    initial = MonotonicCounter $ forAll (const 0)

instance (KnownNat ix) => Query (MonotonicCounter ix) () Int where
    query (MonotonicCounter xs) _  = Just (sum $ I.elems xs)

instance (KnownNat ix) => CvRDT (MonotonicCounter ix) CounterUpdate where
    update (MonotonicCounter s) (Increment myId) = Just $ MonotonicCounter $ forEach s update'
        where   -- update' :: Fin ix -> Int -> Int
                update' ix x = if toInteger ix == toInteger myId
                                    then x + 1
                                    else x
