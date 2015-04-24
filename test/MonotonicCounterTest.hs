{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-
 MonotonicCounterTest.hs

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
-- @Date:   2015-04-22 14:30:37
-- @Last Modified by:   Sebastien Soudan
-- @Last Modified time: 2015-04-24 14:23:16

module MonotonicCounterTest
    where

import           CRDT
import           Data.Sized.Fin
import           Data.Sized.Matrix
import           GHC.TypeLits              (KnownNat)
import           MonotonicCounter
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen


instance forall ix. (KnownNat ix) => Arbitrary (MonotonicCounter ix) where
  arbitrary = do
      xs <- arbitrary :: Gen [Int] -- TODO(ssoudan) not sure how to control the size of this list...
      x <- arbitrary :: Gen Int
      let m :: (KnownNat ix) => Vector ix Int
          m = forAll (\n -> (cycle (x:xs)) !! (size n))
      return $ MonotonicCounter m

instance Arbitrary CounterUpdate where
  arbitrary = do
    x <- arbitrary
    return (Increment x)

testSimple :: Bool
testSimple = let q1_ :: Maybe Int
                 q1_ = do
                        let s0 :: MonotonicCounter 4
                            s0 = initial
                        s1 <- update s0 $ Increment 2
                        q1 <- query s1 ()
                        return q1
             in case q1_ of Just 1 -> True
                            Nothing -> False

testUpdatesMonotonicallyAdvance ::(KnownNat ix) => Fin ix -> MonotonicCounter ix -> CounterUpdate -> Bool
testUpdatesMonotonicallyAdvance _ s u = let s2 = update s u
                                       in case s2 of Just s2' -> is s s2'
                                                     Nothing  -> True

testCvRDTEquivalentAbstractStates :: (KnownNat ix) => Fin ix -> MonotonicCounter ix -> MonotonicCounter ix -> () -> Bool
testCvRDTEquivalentAbstractStates _ s1 s2 q = if (is s1 s2) && (is s2 s1)
                                            then
                                              ((query s1 q) == ((query s2 q) :: Maybe Int))
                                            else
                                              True

-- TODO(ssoudan) the test should actually be on the SemiLattive since the property comes from here
testMergeIdempotent :: (KnownNat ix) => Fin ix -> MonotonicCounter ix -> Bool
testMergeIdempotent _ s = merge s s == s

-- TODO(ssoudan) the test should actually be on the SemiLattive since the property comes from here
testMergeCommutative :: (KnownNat ix) => Fin ix -> MonotonicCounter ix -> MonotonicCounter ix -> Bool
testMergeCommutative _ s1 s2 = merge s1 s2 == merge s2 s1


-- TODO(ssoudan) test eventual consistency
-- "Since merge is idempotent and commutative (by the properties of ⊔v), messages may be lost,
--  received out of order, or multiple times, as long as new state eventually reaches all replicas,
--  either directly or indirectly via successive merges."
