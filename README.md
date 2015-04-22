

Definition of CvRDT: (from [INRIA_TR_7506])

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

