{-
 MainTestSuite.hs

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
-- @Date:   2015-04-22 11:34:51
-- @Last Modified by:   Sebastien Soudan
-- @Last Modified time: 2015-04-22 15:06:16

module Main
    where

import           CRDT
import           IntMax
import           IntMaxTest


main = do
            putStrLn "Hello"
            let s3_ = do
                    let s1 = initial :: IntMax
                    s2 <- update s1 (IntMax 12)
                    s3 <- update s2 (IntMax 42)
                    return s3
            let v :: Maybe Int
                v = s3_ >>= (`query` ())
            print v


