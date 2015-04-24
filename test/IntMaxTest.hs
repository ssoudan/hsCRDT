{-# OPTIONS_GHC -fno-warn-orphans #-}
{-
 IntMaxTest.hs

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
-- @Last Modified time: 2015-04-24 15:00:13

module IntMaxTest
    where

import           CRDT
import           IntMax
import           Test.QuickCheck.Arbitrary

instance Arbitrary IntMax where
  arbitrary = do
    x <- arbitrary 
    return (intToIntMax x)
  -- shrink    = (map intToIntMax) . (shrinkIntegral . intMaxToInt)

testSimple :: Bool
testSimple = let q1_ :: Maybe Int
                 q1_ = do
                        -- on 0
                        let s0 :: IntMax
                            s0 = initial
                        s01 <- update s0 $ intToIntMax 2
                        -- on 1
                        let s1 :: IntMax
                            s1 = initial
                        s11 <- update s1 $ intToIntMax 4
                        let s12 = merge s01 s11
                        q1 <- query s12 ()
                        return q1
             in case q1_ of Just 4 -> True
                            Nothing -> False
testEq :: Bool
testEq = let q1_ :: Maybe Bool
             q1_ = do
                        -- on 0
                        let s0 :: IntMax
                            s0 = initial
                        s01 <- update s0 $ intToIntMax 2
                        -- on 1
                        let s1 :: IntMax
                            s1 = initial
                        s11 <- update s1 $ intToIntMax 2
          
                        return $ s11 == s01
             in case q1_ of Just q -> q
                            Nothing -> False

testUpdatesMonotonicallyAdvance :: IntMax -> IntMax -> Bool
testUpdatesMonotonicallyAdvance s u = let s2 = update s u
                                       in case s2 of Just s2' -> is s s2'
                                                     Nothing  -> True

testCvRDTEquivalentAbstractStates :: IntMax -> IntMax -> () -> Bool
testCvRDTEquivalentAbstractStates s1 s2 q = if (is s1 s2) && (is s2 s1)
                                            then
                                              ((query s1 q) == ((query s2 q) :: Maybe Int))
                                            else
                                              True

-- testMergeAlwaysEnabled :: IntMax -> IntMax -> Bool
-- -> no need for this one since by def of merge :: p -> p -> p it is defined on all PxP

-- TODO(ssoudan) the test should actually be on the SemiLattive since the property comes from here
testMergeIdempotent :: IntMax -> Bool
testMergeIdempotent s = merge s s == s

-- TODO(ssoudan) the test should actually be on the SemiLattive since the property comes from here
testMergeCommutative :: IntMax -> IntMax -> Bool
testMergeCommutative s1 s2 = merge s1 s2 == merge s2 s1


-- TODO(ssoudan) test eventual consistency
-- "Since merge is idempotent and commutative (by the properties of ⊔v), messages may be lost,
--  received out of order, or multiple times, as long as new state eventually reaches all replicas,
--  either directly or indirectly via successive merges."
