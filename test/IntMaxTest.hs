-- @Author: Sebastien Soudan
-- @Date:   2015-04-22 14:30:37
-- @Last Modified by:   Sebastien Soudan
-- @Last Modified time: 2015-04-22 15:15:20

module IntMaxTest
    where

import           CRDT
import           IntMax

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


testMergeIdempotent :: IntMax -> Bool
testMergeIdempotent s = merge s s == s 

testMergeCommutative :: IntMax -> IntMax -> Bool
testMergeCommutative s1 s2 = merge s1 s2 == merge s2 s1


-- TODO(ssoudan) test that "Since merge is idempotent and com- mutative (by the properties of ⊔v), messages may be lost, received out of order, or multiple times, as long as new state eventually reaches all replicas, either directly or indirectly via successive merges."