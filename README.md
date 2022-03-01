# retry-types

This small Haskell library contains type-level representations of the retry policies from the [`retry`](https://hackage.haskell.org/package/retry) package along with a type class to reify them to their value-level equivalents. This allows other packages which wish to incorporate retry policies into type-level specifications to do so.

## Example

Each retry policy and retry policy transformer from [`Control.Retry`](https://hackage.haskell.org/package/retry/docs/Control-Retry.html) has an equivalent type in `Control.Retry.Types` with the same name (except starting with an upper-case character). For example, the `ConstantDelay 5` type represents a retry policy with a constant delay of 5 microseconds -- the equivalent of `constantDelay 5` at the value-level. We can reify `ConstantDelay 5` into `constantDelay 5` by using the `retryPolicyVal` function that is part of the `HasRetryPolicy` type class: `retryPolicyVal @(ConstantDelay 5)`.

Type-level retry policies can also be combined with the `(<+>)` type operator. For example, `ConstantDelay 5 <+> LimitRetries 10` is the equivalent of `constantDelay 5 <> limitRetries 10`.
