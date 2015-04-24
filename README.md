
# Example of C(v/m)RDT in Haskell 

To read together with 'A comprehensive study of Convergent and Commutative Replicated Data Types', Marc Shapiro et al., INRIA Technical Report 7506.

## Definition of CvRDT: (from [INRIA_TR_7506])

Specification 1: Outline of a state-based object specification. Preconditions, arguments, return values and statements are optional.

1. `payload` Payload type; instantiated at all replicas 
	`initial` Initial value
2. `query` Query (arguments) : returns
    `pre` Precondition
    `let` Evaluate synchronously, no side effects
3. `update` Source-local operation (arguments) : returns 
    `pre` Precondition
    `let` Evaluate at source, synchronously Side-effects at source to execute synchronously
4, `compare` (value1, value2) : boolean b 
    `Is` value1 ≤ value2 in semilattice?
5. `merge` (value1, value2) : payload mergedValue 
    `LUB merge` of value1 and value2, at any replica

## Concrete CvRDT:

### IntMax 
Note that updates consist of another `IntMax` (which wraps an `Int`).

### MonotonicCounter
Here updates have their own data type and consist of increment of the distributed counter. 

In the following test, we create a distributed monotonic counter with 4 seats.
In turn every machine will do a local increment (Increment $myId$) and send to the next machine which does another local update before merging the state from previous `id` and passing it to next. At the end we check that a query gives 4 -- we have 4 `Increment`.

```Haskell
testSimple :: Bool
testSimple = let q1_ :: Maybe Int
                 q1_ = do
                        -- on 0
                        let s0 :: MonotonicCounter 4
                            s0 = initial
                        s01 <- update s0 $ Increment 0    
                        -- on 1
                        let s1 :: MonotonicCounter 4
                            s1 = initial
                        s11 <- update s1 $ Increment 1
                        let s12 = merge s01 s11
                        -- on 2
                        let s2 :: MonotonicCounter 4
                            s2 = initial
                        s21 <- update s2 $ Increment 2
                        let s22 = merge s21 s12
                        -- on 3
                        let s3 :: MonotonicCounter 4
                            s3 = initial
                        s31 <- update s3 $ Increment 3
                        let s32 = merge s31 s22
                        q1 <- query s32 ()
                        return q1
             in case q1_ of Just 4 -> True
                            Nothing -> False
```