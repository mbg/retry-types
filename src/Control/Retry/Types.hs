-------------------------------------------------------------------------------
-- retry-types
-- Copyright 2022 Michael B. Gale (github@michael-gale.co.uk)
-------------------------------------------------------------------------------

{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Type-level descriptions of retry policies from "Control.Retry". A type
-- for each retry policy and retry policy transformer from "Control.Retry"
-- is exported by this module. The `HasRetryPolicy` type class allows for
-- type-level retry policies to be reified.
module Control.Retry.Types (
    HasRetryPolicy(..),
    ConstantDelay,
    ExponentialBackoff,
    FullJitterBackoff,
    FibonacciBackoff,
    LimitRetries,
    LimitRetriesByDelay,
    LimitRetriesByCumulativeDelay,
    CapDelay
) where

-------------------------------------------------------------------------------

import GHC.TypeLits

import Control.Monad.IO.Class
import Control.Retry

import Data.Proxy

-------------------------------------------------------------------------------

-- | A class of type-level descriptions of a retry policies for which there is
-- a corresponding retry policy on the value level.
class HasRetryPolicy t m where
    -- | `retryPolicyVal` is the retry policy corresponding to @t@ at the value
    -- level. This definition is intended to be used with @TypeApplications@
    -- > simulatePolicy 10 (retryPolicyVal @(ConstantDelay 5))
    retryPolicyVal :: RetryPolicyM m

-- | This type-level operator allows two type-level retry policies to
-- be composed into one.
data (a :: k) <+> (b :: k)

instance (Monad m, HasRetryPolicy a m, HasRetryPolicy b m)
      => HasRetryPolicy (a <+> b) m
   where
    retryPolicyVal = retryPolicyVal @a <> retryPolicyVal @b

-------------------------------------------------------------------------------

-- | Represents the `constantDelay` retry policy.
data ConstantDelay (n :: Nat)

instance (KnownNat n, Monad m) => HasRetryPolicy (ConstantDelay n) m where
    retryPolicyVal = constantDelay $ fromInteger $ natVal (Proxy :: Proxy n)

-- | Represents the `exponentialBackoff` retry policy.
data ExponentialBackoff (n :: Nat)

instance (KnownNat n, Monad m) => HasRetryPolicy (ExponentialBackoff n) m where
    retryPolicyVal = exponentialBackoff $ fromInteger $ natVal (Proxy :: Proxy n)

-- | Represents the `fullJitterBackoff` retry policy.
data FullJitterBackoff (n :: Nat)

instance (KnownNat n, MonadIO m) => HasRetryPolicy (FullJitterBackoff n) m where
    retryPolicyVal = fullJitterBackoff $ fromInteger $ natVal (Proxy :: Proxy n)

-- | Represents the `fibonacciBackoff` retry policy.
data FibonacciBackoff (n :: Nat)

instance (KnownNat n, Monad m) => HasRetryPolicy (FibonacciBackoff n) m where
    retryPolicyVal = fibonacciBackoff $ fromInteger $ natVal (Proxy :: Proxy n)

-- | Represents the `limitRetries` retry policy.
data LimitRetries (n :: Nat)

instance (KnownNat n, Monad m) => HasRetryPolicy (LimitRetries n) m where
    retryPolicyVal = limitRetries $ fromInteger $ natVal (Proxy :: Proxy n)

-------------------------------------------------------------------------------

-- | Represents the `limitRetriesByDelay` retry policy transformer.
data LimitRetriesByDelay (n :: Nat) p

instance (KnownNat n, Monad m, HasRetryPolicy p m)
       => HasRetryPolicy (LimitRetriesByDelay n p) m
    where

    retryPolicyVal = limitRetriesByDelay
        (fromInteger $ natVal (Proxy :: Proxy n))
        (retryPolicyVal @p)

-- | Represents the `limitRetriesByCumulativeDelay` retry policy transformer.
data LimitRetriesByCumulativeDelay (n :: Nat) p

instance (KnownNat n, Monad m, HasRetryPolicy p m)
       => HasRetryPolicy (LimitRetriesByCumulativeDelay n p) m
    where

    retryPolicyVal = limitRetriesByCumulativeDelay
        (fromInteger $ natVal (Proxy :: Proxy n))
        (retryPolicyVal @p)

-- | Represents the `capDelay` retry policy transformer.
data CapDelay (n :: Nat) p

instance (KnownNat n, Monad m, HasRetryPolicy p m)
       => HasRetryPolicy (CapDelay n p) m
    where

    retryPolicyVal = capDelay
        (fromInteger $ natVal (Proxy :: Proxy n))
        (retryPolicyVal @p)

-------------------------------------------------------------------------------
